(define-module (lang program)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme cxr)
  #:use-module (lang reader)
  #:use-module (lang types)
  #:use-module (lang env)
  #:use-module (lang world)
  #:use-module (lang print)
  #:use-module (lang infer)
  #:use-module (lang eval)
  #:use-module (lang std)
  #:export (run-program
            run-program-text
            result?
            result-transcript
            result-diagnostics
            parse-forms))

(define-record-type <state>
  (make-state state-scope state-env)
  state?
  (state-scope state-scope)
  (state-env state-env))

(define (parse-forms source)
  (guard (e ((diagnostic? e) (raise e))
            (#t (fail 'syntax "cannot read this program")))
    (read-program source)))

(define (tagged? form tag)
  (and (pair? form) (eq? (car form) tag) (proper-list? form)))

(define (evaluating thunk)
  (with-phase 'evaluating thunk))

(define (resolve-signature form scope)
  (cond
   ((symbol? form) (lookup-signature scope form))
   ((tagged? form 'sig) (elaborate-signature form scope))
   (else (fail 'syntax "malformed signature: expected (sig item ...)"))))

(define (signature-label form)
  (if (symbol? form) (symbol->string form) "sig ... end"))

(define (check-module-name name form)
  (if (and (symbol? name) (capitalized? name) (not (qualified? name)))
      name
      (fail 'syntax "malformed declaration: module names start with an upper case letter")))

(define (module-body-parts form)
  (let ((rest (cddr form)))
    (if (and (pair? rest) (eq? (car rest) ':))
        (if (= (length rest) 3)
            (cons (cadr rest) (caddr rest))
            (fail 'syntax "malformed module"))
        (if (= (length rest) 1)
            (cons #f (car rest))
            (fail 'syntax "malformed module")))))

(define (renv-lookup-module-value env name)
  (let ((found (assq name (renv-modules env))))
    (if found
        (cdr found)
        (fail 'unreachable))))

(define (elaborate-module-body body state path)
  (let ((scope (state-scope state))
        (env (state-env state)))
    (cond
     ((tagged? body 'struct)
      (cons (elaborate-struct body scope path)
            (evaluating (lambda () (evaluate-struct body env)))))
     ((and (proper-list? body) (= (length body) 2) (symbol? (car body))
           (symbol? (cadr body)))
      (let* ((functor-name (car body))
             (argument-name (cadr body))
             (decl (lookup-module scope functor-name))
             (argument (lookup-module scope argument-name)))
        (if (not (functor-type? decl))
            (fail 'functor-misuse functor-name)
            (cons (apply-functor decl argument argument-name)
                  (evaluating
                   (lambda ()
                     (apply-functor-value
                      (renv-lookup-module-value env functor-name)
                      (renv-lookup-module-value env argument-name))))))))
     (else (fail 'syntax "malformed module body: expected (struct ...) or (Functor Argument)")))))

(define (process-type form state)
  (let* ((binding (elaborate-type-declaration form (state-scope state)))
         (scope (bind-type (state-scope state) (car binding) (cdr binding))))
    (cons (make-state scope (state-env state))
          (if (tycon? (cdr binding))
              (string-append "type " (symbol->string (car binding))
                             (apply string-append
                                    (map (lambda (param)
                                           (string-append " '"
                                                          (symbol->string param)))
                                         (tycon-params (cdr binding)))))
              (string-append "type " (symbol->string (car binding)) " = "
                             (type->text (nominal-body (cdr binding))))))))

(define (process-define form state)
  (let* ((binding (elaborate-definition form (state-scope state)))
         (value (evaluating
                 (lambda () (evaluate-definition form (state-env state)))))
         (scope (bind-value (state-scope state) (car binding) (cdr binding)))
         (env (renv-bind-value (state-env state) (car value) (cdr value))))
    (cons (make-state scope env)
          (string-append "val " (symbol->string (car binding))
                         " : " (entry->text (cdr binding))
                         " = " (value->string (cdr value))))))

(define (process-module-type form state)
  (if (not (= (length form) 3))
      (fail 'syntax "malformed module type")
      (let* ((name (check-module-name (cadr form) form))
             (sig (elaborate-signature (caddr form) (state-scope state)))
             (scope (bind-signature (state-scope state) name sig)))
        (cons (make-state scope (state-env state))
              (string-append "module type " (symbol->string name))))))

(define (process-module form state)
  (let* ((name (check-module-name (cadr form) form))
         (parts (module-body-parts form))
         (signature-name (car parts))
         (body (cdr parts))
         (elaborated (elaborate-module-body
                      body state (string-append (symbol->string name) ".")))
         (modtype (car elaborated))
         (value (cdr elaborated))
         (sealed (if signature-name
                     (seal-modtype modtype
                                   (resolve-signature signature-name
                                                      (state-scope state))
                                   (string-append (symbol->string name) "."))
                     modtype))
         (scope (bind-module (state-scope state) name sealed))
         (env (renv-bind-module (state-env state) name value)))
    (cons (make-state scope env)
          (string-append "module " (symbol->string name) " : "
                         (modtype->text sealed)))))

(define (process-functor form state)
  (if (not (= (length form) 4))
      (fail 'syntax "malformed functor: expected (functor Name (Param : SIG) (struct ...))")
      (let* ((name (check-module-name (cadr form) form))
             (parameter (caddr form))
             (body (cadddr form)))
        (if (not (and (proper-list? parameter)
                      (= (length parameter) 3)
                      (eq? (cadr parameter) ':)))
            (fail 'syntax "malformed functor parameter: expected (Param : SIG)")
            (let* ((param-name (check-module-name (car parameter) form))
                   (signature-name (caddr parameter))
                   (sig (resolve-signature signature-name (state-scope state)))
                   (decl (make-functor-type param-name sig body (state-scope state)))
                   (result (check-functor-body decl))
                   (scope (bind-module (state-scope state) name decl))
                   (env (renv-bind-module
                         (state-env state) name
                         (make-functor-value param-name body (state-env state)))))
              (cons (make-state scope env)
                    (string-append "functor " (symbol->string name)
                                   " (" (symbol->string param-name) " : "
                                   (signature-label signature-name) ") -> "
                                   (modtype->text result))))))))

(define (process-expression form state)
  (let ((entry (infer-expression form (form-span form) (state-scope state)))
        (value (evaluating (lambda () (evaluate form (state-env state))))))
    (cons state
          (string-append "- : " (entry->text entry)
                         " = " (value->string value)))))

(define (process-form form state)
  (cond
   ((tagged? form 'type) (process-type form state))
   ((tagged? form 'define) (process-define form state))
   ((tagged? form 'module-type) (process-module-type form state))
   ((tagged? form 'module) (process-module form state))
   ((tagged? form 'functor) (process-functor form state))
   (else (process-expression form state))))

(define-record-type <result>
  (make-result result-transcript result-diagnostics)
  result?
  (result-transcript result-transcript)
  (result-diagnostics result-diagnostics))

(define (base-state)
  (let loop ((forms prelude-forms)
             (state (make-state (initial-scope) (initial-environment))))
    (if (null? forms)
        state
        (loop (cdr forms)
              (car (with-world (make-world 'typing (car forms)
                                           (state-scope state)
                                           (state-env state)
                                           "")
                               (lambda ()
                                 (process-form (car forms) state))))))))

(define (attempt thunk)
  (guard (e (#t (if (diagnostic? e)
                    e
                    (make-diagnostic 'unreachable '() (current-world) #f))))
    (thunk)))

(define (run-program source)
  (set-current-expression! #f)
  (set-current-context! #f)
  (set-current-reason! #f)
  (set-current-path! '())
  (set-current-span! #f)
  (set-current-step! #f)
  (set-current-constraint! #f)
  (let ((parsed (attempt (lambda ()
                           (with-phase 'reading
                                       (lambda () (parse-forms source)))))))
    (if (diagnostic? parsed)
        (make-result "" (list parsed))
        (let ((base (attempt (lambda () (base-state)))))
          (if (diagnostic? base)
              (make-result "" (list base))
              (run-forms parsed base source))))))

(define (run-forms parsed base source)
  (let loop ((forms parsed)
             (state base)
             (lines '()))
    (if (null? forms)
        (make-result (string-join (reverse lines) "\n") '())
        (let ((outcome
               (begin
                 (set-current-expression! #f)
                 (set-current-context! #f)
                 (set-current-reason! #f)
                 (set-current-path! '())
                 (set-current-span! #f)
                 (set-current-step! #f)
                 (set-current-constraint! #f)
                 (attempt
                  (lambda ()
                    (with-world (make-world 'typing (car forms)
                                            (state-scope state)
                                            (state-env state)
                                            source)
                                (lambda ()
                                  (process-form (car forms) state))))))))
          (if (diagnostic? outcome)
              (make-result (string-join (reverse lines) "\n") (list outcome))
              (loop (cdr forms) (car outcome)
                    (cons (cdr outcome) lines)))))))

(define (run-program-text source)
  (let* ((result (run-program source))
         (problems (result-diagnostics result)))
    (if (null? problems)
        (cons #t (if (string=? (result-transcript result) "")
                     "nothing to do"
                     (result-transcript result)))
        (cons #f (string-join (map (lambda (diagnostic)
                                     (symbol->string
                                      (diagnostic-tag diagnostic)))
                                   problems)
                              "\n")))))
