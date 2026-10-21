;; Development server: watches src/ and web/, rebuilds into build/, serves it.
;; Build errors are printed to the terminal and shown in the browser at /.

(use-modules (web server)
             (web request)
             (web response)
             (web uri)
             (ice-9 format)
             (ice-9 ftw)
             (ice-9 popen)
             (ice-9 textual-ports)
             (ice-9 binary-ports)
             (ice-9 threads)
             (srfi srfi-1))

(define guild (or (getenv "WATCH_GUILD") "guild"))
(define esbuild (or (getenv "WATCH_ESBUILD") "esbuild"))
(define build-dir "build")
(define static-files '("web/index.html" "web/style.css" "web/page.css"))

(define serve-mutex (make-mutex))
(define failures '())   ; kind -> (title . output), or #f once the kind is fixed
(define generation 0)   ; bumped after every rebuild; pages poll it to reload

(define reload-script
  "<script>
(async () => {
  let seen = null;
  for (;;) {
    try {
      const now = await (await fetch(\"/__generation\")).text();
      if (seen !== null && now !== seen) location.reload();
      seen = now;
    } catch (e) {}
    await new Promise((resolve) => setTimeout(resolve, 1000));
  }
})();
</script>")

(define (inject-reload html)
  (let ((at (string-contains html "</body>")))
    (if at
        (string-append (substring html 0 at) reload-script (substring html at))
        (string-append html reload-script))))

(define (log-line message)
  (display (strftime "[%H:%M:%S] " (localtime (current-time))))
  (display message)
  (newline))

(define (shell-quote text)
  (string-append "'" (string-join (string-split text #\') "'\\''") "'"))

(define (run-step title command)
  (let* ((start (get-internal-real-time))
         (pipe (open-input-pipe
                (string-append (string-join (map shell-quote command) " ")
                               " 2>&1")))
         (output (string-trim-both (get-string-all pipe)))
         (status (close-pipe pipe))
         (took (/ (- (get-internal-real-time) start)
                  1.0 internal-time-units-per-second)))
    (if (eqv? 0 (status:exit-val status))
        (begin
          (log-line (format #f "~a: ok (~,1fs)" title took))
          #f)
        (begin
          (log-line (format #f "~a: FAILED (~,1fs)" title took))
          (display (string-append "  " (make-string 60 #\-) "\n"))
          (for-each (lambda (line) (display "  ") (display line) (newline))
                    (string-split output #\newline))
          (display (string-append "  " (make-string 60 #\-) "\n"))
          (cons title output)))))

;; Swap finished artifacts in under the lock so a request never sees a
;; half-written file.
(define (install tmp)
  (with-mutex serve-mutex
    (for-each (lambda (name)
                (rename-file (string-append tmp "/" name)
                             (string-append build-dir "/" name)))
              (scandir tmp (lambda (name) (not (member name '("." ".."))))))))

(define (build-wasm)
  (let* ((tmp (mkdtemp (string-append build-dir "/.wasm-XXXXXX")))
         (failure (run-step "wasm (guild compile-wasm)"
                            (list guild "compile-wasm" "-L" "src" "-L" "web"
                                  "-o" (string-append tmp "/lang.wasm")
                                  (string-append "--bundle=" tmp)
                                  "src/main.scm"))))
    (unless failure (install tmp))
    (system* "rm" "-rf" tmp)
    failure))

(define (build-ui)
  (let* ((tmp (mkdtemp (string-append build-dir "/.ui-XXXXXX")))
         (failure (run-step "ui (esbuild)"
                            (list esbuild "web/src/mount.jsx" "--bundle"
                                  "--splitting" "--format=esm" "--jsx=automatic"
                                  "--entry-names=app"
                                  (string-append "--outdir=" tmp)))))
    (unless failure (install tmp))
    (system* "rm" "-rf" tmp)
    failure))

(define (copy-static)
  (with-mutex serve-mutex
    (for-each (lambda (path)
                (copy-file path (string-append build-dir "/" (basename path))))
              static-files))
  (log-line (string-append "static: copied "
                           (string-join (map basename static-files) ", ")))
  #f)

(define builders
  `((wasm . ,build-wasm) (ui . ,build-ui) (static . ,copy-static)))

(define (rebuild kinds)
  (for-each (lambda (entry)
              (when (memq (car entry) kinds)
                (set! failures
                      (assq-set! failures (car entry) ((cdr entry))))))
            builders)
  (set! generation (+ generation 1))
  (when (every (lambda (entry) (not (cdr entry))) failures)
    (log-line "build clean")))

(define (classify path)
  (cond
   ((string-prefix? "web/src/" path) 'ui)
   ((string-suffix? ".scm" path) 'wasm)
   ((member path static-files) 'static)
   (else #f)))

(define (snapshot)
  (let ((files '()))
    (for-each
     (lambda (top)
       (file-system-fold
        (lambda (path stat result)          ; enter?
          (not (string-prefix? "." (basename path))))
        (lambda (path stat result)          ; leaf
          (when (classify path)
            (set! files (cons (cons path (cons (stat:mtime stat)
                                               (stat:mtimensec stat)))
                              files)))
          result)
        (lambda (path stat result) result)  ; down
        (lambda (path stat result) result)  ; up
        (lambda (path stat result) result)  ; skip
        (lambda (path stat errno result) result)
        #f top))
     '("src" "web"))
    files))

(define (changed-paths old new)
  (delete-duplicates
   (filter (lambda (path)
             (not (equal? (assoc-ref old path) (assoc-ref new path))))
           (append (map car old) (map car new)))))

(define (watcher)
  (let loop ((previous (snapshot)))
    (usleep 400000)
    (let ((current (snapshot)))
      (if (null? (changed-paths previous current))
          (loop previous)
          ;; Debounce: wait for saves to settle before rebuilding.
          (let settle ((current current))
            (usleep 150000)
            (let ((next (snapshot)))
              (if (pair? (changed-paths current next))
                  (settle next)
                  (let* ((changed (sort (changed-paths previous next) string<?))
                         (kinds (delete-duplicates (filter-map classify changed))))
                    (unless (null? kinds)
                      (log-line (string-append "changed: "
                                               (string-join changed ", ")))
                      (rebuild kinds))
                    (loop next)))))))))

(define (escape-html text)
  (string-fold-right
   (lambda (c out)
     (string-append (case c
                      ((#\&) "&amp;")
                      ((#\<) "&lt;")
                      ((#\>) "&gt;")
                      (else (string c)))
                    out))
   "" text))

(define (error-page active)
  (string-append
   "<!doctype html>\n<meta charset=\"utf-8\">\n<title>build failed</title>\n"
   "<style>\n"
   "  body { background: #1c1c1c; color: #ddd; font: 14px/1.5 monospace;\n"
   "         margin: 0; padding: 2rem; }\n"
   "  h1 { color: #ff6b6b; font-size: 1.2rem; }\n"
   "  h2 { color: #ffb86b; font-size: 1rem; margin-top: 2rem; }\n"
   "  pre { background: #111; padding: 1rem; overflow-x: auto;\n"
   "        border-left: 3px solid #ff6b6b; white-space: pre-wrap; }\n"
   "  p { color: #888; }\n"
   "</style>\n"
   "<h1>build failed</h1>\n"
   "<p>The last good build is still served on every other path.\n"
   "   Fix the error and refresh.</p>\n"
   (apply string-append
          (map (lambda (failure)
                 (string-append "<h2>" (escape-html (car failure)) "</h2>\n"
                                "<pre>" (escape-html (cdr failure)) "</pre>\n"))
               active))))

(define mime-types
  '(("html" . (text/html (charset . "utf-8")))
    ("css" . (text/css (charset . "utf-8")))
    ("js" . (text/javascript (charset . "utf-8")))
    ("json" . (application/json))
    ("map" . (application/json))
    ("wasm" . (application/wasm))
    ("svg" . (image/svg+xml))
    ("png" . (image/png))))

(define (mime-for path)
  (let ((dot (string-rindex path #\.)))
    (or (and dot (assoc-ref mime-types (substring path (+ dot 1))))
        '(application/octet-stream))))

;; "/foo/bar" -> "foo/bar"; #f if the path tries to escape the build dir.
(define (safe-relative path)
  (let ((parts (remove string-null? (string-split path #\/))))
    (and (not (any (lambda (part) (string=? part "..")) parts))
         (if (null? parts) "index.html" (string-join parts "/")))))

(define (respond code headers body)
  (cons (build-response
         #:code code
         #:headers (cons '(cache-control . (no-store)) headers))
        body))

(define (serve-file relative)
  (let ((full (string-append build-dir "/" relative)))
    (cond
     ((not (and (file-exists? full)
                (eq? 'regular (stat:type (stat full)))))
      (respond 404 '((content-type . (text/plain (charset . "utf-8"))))
               "not found"))
     ((string=? relative "index.html")
      (respond 200 '((content-type . (text/html (charset . "utf-8"))))
               (inject-reload (call-with-input-file full get-string-all))))
     (else
      (respond 200 (list (cons 'content-type (mime-for relative)))
               (let ((data (call-with-input-file full get-bytevector-all
                                                 #:binary #t)))
                 (if (eof-object? data) #vu8() data)))))))

(define (handler request body)
  (let* ((path (uri-path (request-uri request)))
         (active (filter-map (lambda (entry) (cdr entry)) failures))
         (result
          (cond
           ((string=? path "/__generation")
            (respond 200 '((content-type . (text/plain (charset . "utf-8"))))
                     (number->string generation)))
           ((and (pair? active) (member path '("/" "/index.html")))
            (respond 500 '((content-type . (text/html (charset . "utf-8"))))
                     (inject-reload (error-page active))))
           ((safe-relative path)
            => (lambda (relative)
                 (with-mutex serve-mutex (serve-file relative))))
           (else
            (respond 404 '((content-type . (text/plain (charset . "utf-8"))))
                     "not found")))))
    (format (current-error-port) "  ~a ~a ~a~%"
            (request-method request) path (response-code (car result)))
    (values (car result) (cdr result))))

(define (main args)
  (setvbuf (current-output-port) 'line)
  (let ((port (or (and (pair? (cdr args)) (string->number (cadr args))) 8080)))
    (unless (file-exists? "src/main.scm")
      (display "dev/watch.scm: run this from the repository root (src/main.scm not found)\n"
               (current-error-port))
      (exit 1))
    (unless (file-exists? build-dir) (mkdir build-dir))
    (log-line "initial build")
    (rebuild '(wasm ui static))
    (call-with-new-thread watcher)
    (log-line (format #f "serving ~a on http://localhost:~a (watching src/ and web/)"
                      build-dir port))
    (run-server handler 'http `(#:host "127.0.0.1" #:port ,port))))

(main (command-line))
