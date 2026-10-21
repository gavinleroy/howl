(define-module (lang reader)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme char)
  #:use-module (lang world)
  #:export (read-program spans-for spans-within clear-spans!
            form-span car-span nth-span))

(define-record-type <eof-marker>
  (make-eof-marker)
  eof-marker?)

(define %eof (make-eof-marker))

(define %spans (vector '()))

(define %forms (vector '()))

(define %cars (vector '()))

(define (clear-spans!)
  (vector-set! %spans 0 '())
  (vector-set! %forms 0 '())
  (vector-set! %cars 0 '()))

(define (remember-form! form span)
  (vector-set! %forms 0 (cons (cons form span) (vector-ref %forms 0))))

(define (remember-cars! form spans)
  (let loop ((cells form) (spans spans))
    (if (and (pair? cells) (pair? spans))
        (begin
          (vector-set! %cars 0 (cons (cons cells (car spans))
                                     (vector-ref %cars 0)))
          (loop (cdr cells) (cdr spans))))))

(define (lookup table key)
  (let loop ((entries (vector-ref table 0)))
    (cond ((null? entries) #f)
          ((eq? (caar entries) key) (cdar entries))
          (else (loop (cdr entries))))))

(define (form-span form) (and (pair? form) (lookup %forms form)))

(define (car-span cell) (and (pair? cell) (lookup %cars cell)))

(define (nth-span form index)
  (let loop ((cells form) (index index))
    (cond ((not (pair? cells)) #f)
          ((= index 0) (car-span cells))
          (else (loop (cdr cells) (- index 1))))))

(define (record-span! datum start end)
  (vector-set! %spans 0 (cons (cons datum (cons start end))
                              (vector-ref %spans 0))))

(define (spans-for datum)
  (let loop ((entries (vector-ref %spans 0)) (found '()))
    (cond
     ((null? entries) found)
     ((equal? (caar entries) datum)
      (loop (cdr entries) (cons (cdar entries) found)))
     (else (loop (cdr entries) found)))))

(define (inside? span outer)
  (and (>= (car span) (car outer)) (<= (cdr span) (cdr outer))))

(define (spans-within datum scope)
  (let ((spans (spans-for datum)))
    (if (not scope)
        spans
        (let ((kept (keep-inside spans (spans-for scope))))
          (if (pair? kept) kept spans)))))

(define (keep-inside spans bounds)
  (cond
   ((null? spans) '())
   ((any-span? bounds (car spans))
    (cons (car spans) (keep-inside (cdr spans) bounds)))
   (else (keep-inside (cdr spans) bounds))))

(define (any-span? bounds span)
  (cond ((null? bounds) #f)
        ((inside? span (car bounds)) #t)
        (else (any-span? (cdr bounds) span))))

(define (delimiter? character)
  (or (char=? character #\()
      (char=? character #\))
      (char=? character #\")
      (char=? character #\;)
      (char-whitespace? character)))

(define (skip-blanks source index)
  (let ((size (string-length source)))
    (let loop ((index index))
      (cond
       ((>= index size) index)
       ((char-whitespace? (string-ref source index)) (loop (+ index 1)))
       ((char=? (string-ref source index) #\;)
        (let scan ((index index))
          (cond
           ((>= index size) index)
           ((char=? (string-ref source index) #\newline) (loop (+ index 1)))
           (else (scan (+ index 1))))))
       (else index)))))

(define (read-string-literal source index)
  (let ((size (string-length source)))
    (let loop ((index (+ index 1)) (chars '()))
      (cond
       ((>= index size) (fail 'syntax "unterminated string"))
       ((char=? (string-ref source index) #\")
        (cons (list->string (reverse chars)) (+ index 1)))
       ((char=? (string-ref source index) #\\)
        (if (>= (+ index 1) size)
            (fail 'syntax "unterminated string")
            (let ((escaped (string-ref source (+ index 1))))
              (loop (+ index 2)
                    (cons (cond
                           ((char=? escaped #\n) #\newline)
                           ((char=? escaped #\t) #\tab)
                           ((char=? escaped #\r) #\return)
                           (else escaped))
                          chars)))))
       (else (loop (+ index 1) (cons (string-ref source index) chars)))))))

(define (read-atom source index)
  (let ((size (string-length source)))
    (let loop ((end index))
      (if (or (>= end size) (delimiter? (string-ref source end)))
          (cons (substring source index end) end)
          (loop (+ end 1))))))

(define (atom-value text)
  (cond
   ((string=? text "#t") #t)
   ((string=? text "#true") #t)
   ((string=? text "#f") #f)
   ((string=? text "#false") #f)
   ((string->number text) => (lambda (number) number))
   (else (string->symbol text))))

(define (read-datum source index)
  (let* ((size (string-length source))
         (start (skip-blanks source index)))
    (cond
     ((>= start size) (cons %eof start))
     ((char=? (string-ref source start) #\))
      (fail 'syntax "unexpected close paren"))
     ((char=? (string-ref source start) #\()
      (let loop ((index (+ start 1)) (items '()) (spans '()))
        (let ((next (skip-blanks source index)))
          (cond
           ((>= next size) (fail 'syntax "unterminated list"))
           ((char=? (string-ref source next) #\))
            (let ((datum (reverse items)))
              (record-span! datum start (+ next 1))
              (remember-form! datum (cons start (+ next 1)))
              (remember-cars! datum (reverse spans))
              (cons datum (+ next 1))))
           (else
            (let ((item (read-datum source next)))
              (if (eof-marker? (car item))
                  (fail 'syntax "unterminated list")
                  (loop (cdr item)
                        (cons (car item) items)
                        (cons (cons next (cdr item)) spans)))))))))
     ((char=? (string-ref source start) #\')
      (let ((quoted (read-datum source (+ start 1))))
        (if (eof-marker? (car quoted))
            (fail 'syntax "nothing to quote")
            (let ((datum (list 'quote (car quoted))))
              (record-span! datum start (cdr quoted))
              (remember-form! datum (cons start (cdr quoted)))
              (remember-cars! datum
                              (list (cons start (+ start 1))
                                    (cons (+ start 1) (cdr quoted))))
              (cons datum (cdr quoted))))))
     ((char=? (string-ref source start) #\")
      (let ((text (read-string-literal source start)))
        (record-span! (car text) start (cdr text))
        text))
     (else
      (let* ((atom (read-atom source start))
             (value (atom-value (car atom))))
        (record-span! value start (cdr atom))
        (cons value (cdr atom)))))))

(define (read-program source)
  (clear-spans!)
  (let loop ((index 0) (forms '()))
    (let ((next (read-datum source index)))
      (if (eof-marker? (car next))
          (reverse forms)
          (loop (cdr next) (cons (car next) forms))))))
