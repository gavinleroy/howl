(define-module (lang eval)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme cxr)
  #:use-module (scheme write)
  #:use-module (lang types)
  #:use-module (lang env)
  #:use-module (lang world)
  #:use-module (lang reader)
  #:export (initial-environment
            make-renv
            renv?
            renv-values
            renv-modules
            renv-bind-value
            renv-bind-module
            evaluate
            evaluate-definition
            evaluate-struct
            make-functor-value
            functor-value?
            functor-value-param
            functor-value-body
            functor-value-env
            apply-functor-value
            module-value?
            module-value-bindings
            module-value-modules
            closure?
            closure-params
            closure-body
            closure-env
            record-value?
            record-value-fields
            tuple-value?
            tuple-value-items
            variant?
            variant-name
            variant-values
            primitive?
            primitive-name
            primitive-arity
            primitive-args
            value->string))


;; Raised where the typechecker guarantees execution cannot reach; firing
;; means a soundness bug, not a user error.
(define (unreachable) (fail 'unreachable))
(define-record-type <closure>
  (make-closure closure-params closure-body closure-env)
  closure?
  (closure-params closure-params)
  (closure-body closure-body)
  (closure-env closure-env))

(define-record-type <record-value>
  (make-record-value record-value-fields)
  record-value?
  (record-value-fields record-value-fields))

(define-record-type <tuple-value>
  (make-tuple-value tuple-value-items)
  tuple-value?
  (tuple-value-items tuple-value-items))

(define-record-type <variant>
  (make-variant variant-name variant-values)
  variant?
  (variant-name variant-name)
  (variant-values variant-values))

(define-record-type <module-value>
  (make-module-value module-value-bindings module-value-modules)
  module-value?
  (module-value-bindings module-value-bindings)
  (module-value-modules module-value-modules))

(define-record-type <functor-value>
  (make-functor-value functor-value-param functor-value-body functor-value-env)
  functor-value?
  (functor-value-param functor-value-param)
  (functor-value-body functor-value-body)
  (functor-value-env functor-value-env))

(define-record-type <primitive>
  (make-primitive primitive-name primitive-arity primitive-procedure primitive-args)
  primitive?
  (primitive-name primitive-name)
  (primitive-arity primitive-arity)
  (primitive-procedure primitive-procedure)
  (primitive-args primitive-args))

(define-record-type <renv>
  (make-renv renv-values renv-modules)
  renv?
  (renv-values renv-values)
  (renv-modules renv-modules))

(define-record-type <undefined>
  (make-undefined)
  undefined?)

(define %undefined (make-undefined))

(define (renv-bind-value env name value)
  (make-renv (cons (cons name (vector value)) (renv-values env))
             (renv-modules env)))

(define (renv-bind-cell env name cell)
  (make-renv (cons (cons name cell) (renv-values env))
             (renv-modules env)))

(define (renv-bind-module env name value)
  (make-renv (renv-values env)
             (cons (cons name value) (renv-modules env))))

(define (renv-lookup env name)
  (let ((found (assq name (renv-values env))))
    (cond
     ((not found) (unreachable))
     ((undefined? (vector-ref (cdr found) 0))
      (fail 'undefined-during-definition name))
     (else (vector-ref (cdr found) 0)))))

(define (renv-lookup-module env name)
  (let ((found (assq name (renv-modules env))))
    (if found
        (cdr found)
        (unreachable))))

(define (field-value value label)
  (if (not (record-value? value))
      (unreachable)
      (let ((found (assq label (record-value-fields value))))
        (if found
            (cdr found)
            (unreachable)))))

(define (comparable value)
  (cond
   ((closure? value) (fail 'incomparable value))
   ((primitive? value) (fail 'incomparable value))
   (else value)))

(define (structural=? left right)
  (comparable left)
  (comparable right)
  (cond
   ((and (number? left) (number? right)) (= left right))
   ((and (string? left) (string? right)) (string=? left right))
   ((and (boolean? left) (boolean? right)) (eq? left right))
   ((and (tuple-value? left) (tuple-value? right))
    (and (= (length (tuple-value-items left)) (length (tuple-value-items right)))
         (let loop ((items (tuple-value-items left))
                    (others (tuple-value-items right)))
           (cond ((null? items) #t)
                 ((structural=? (car items) (car others))
                  (loop (cdr items) (cdr others)))
                 (else #f)))))
   ((and (record-value? left) (record-value? right))
    (let loop ((fields (record-value-fields left)))
      (cond
       ((null? fields) #t)
       ((structural=? (cdar fields)
                      (field-value right (caar fields)))
        (loop (cdr fields)))
       (else #f))))
   ((and (variant? left) (variant? right))
    (and (eq? (variant-name left) (variant-name right))
         (= (length (variant-values left)) (length (variant-values right)))
         (let loop ((a (variant-values left)) (b (variant-values right)))
           (cond
            ((null? a) #t)
            ((structural=? (car a) (car b)) (loop (cdr a) (cdr b)))
            (else #f)))))
   (else #f)))

(define (structural<? left right)
  (cond
   ((and (number? left) (number? right)) (< left right))
   ((and (string? left) (string? right)) (string<? left right))
   ((and (boolean? left) (boolean? right)) (and (not left) right))
   (else (fail 'incomparable left right))))

(define (checked-divide name operation left right)
  (if (and (exact-integer? right) (= right 0))
      (fail 'division-by-zero name)
      (operation left right)))

(define (primitive-entry name arity procedure)
  (cons name (make-primitive name arity procedure '())))

(define (initial-environment)
  (make-renv
   (map (lambda (entry) (cons (car entry) (vector (cdr entry))))
        (list
         (primitive-entry '+ 2 +)
         (primitive-entry '- 2 -)
         (primitive-entry '* 2 *)
         (primitive-entry '/ 2 (lambda (a b) (checked-divide "/" truncate-quotient a b)))
         (primitive-entry 'mod 2 (lambda (a b) (checked-divide "mod" truncate-remainder a b)))
         (primitive-entry '+. 2 +)
         (primitive-entry '-. 2 -)
         (primitive-entry '*. 2 *)
         (primitive-entry '/. 2 /)
         (primitive-entry '^ 2 string-append)
         (primitive-entry '= 2 structural=?)
         (primitive-entry '<> 2 (lambda (a b) (not (structural=? a b))))
         (primitive-entry '< 2 structural<?)
         (primitive-entry '> 2 (lambda (a b) (structural<? b a)))
         (primitive-entry '<= 2 (lambda (a b) (not (structural<? b a))))
         (primitive-entry '>= 2 (lambda (a b) (not (structural<? a b))))
         (primitive-entry 'not 1 not)
         (primitive-entry 'float-of-int 1 inexact)
         (primitive-entry 'int-of-float 1 (lambda (x) (exact (truncate x))))
         (primitive-entry 'string-of-int 1 number->string)
         (primitive-entry 'string-of-float 1 number->string)
         (primitive-entry 'string-of-bool 1 (lambda (b) (if b "#t" "#f")))
         (primitive-entry 'string-length 1 string-length)
         (primitive-entry 'string-code-at 2
                          (lambda (s i)
                            (if (and (>= i 0) (< i (string-length s)))
                                (make-variant 'Some
                                              (list (char->integer
                                                     (string-ref s i))))
                                (make-variant 'None '()))))
         (primitive-entry 'string-of-code 1
                          (lambda (code)
                            (string
                             (integer->char
                              (if (and (>= code 0)
                                       (<= code #x10FFFF)
                                       (or (< code #xD800) (> code #xDFFF)))
                                  code
                                  #xFFFD)))))))
   '()))

(define (bind-parameter env param argument)
  (if (symbol? param)
      (renv-bind-value env param argument)
      (let ((bindings (match-pattern param argument)))
        (if (not bindings)
            (unreachable)
            (let loop ((bindings bindings) (inner env))
              (if (null? bindings)
                  inner
                  (loop (cdr bindings)
                        (renv-bind-value inner (caar bindings)
                                         (cdar bindings)))))))))

(define (apply-values function arguments)
  (cond
   ((closure? function)
    (let ((params (closure-params function)))
      (if (not (= (length params) (length arguments)))
          (unreachable)
          (let loop ((params params) (arguments arguments)
                     (env (closure-env function)))
            (if (null? params)
                (evaluate (closure-body function) env)
                (loop (cdr params) (cdr arguments)
                      (bind-parameter env (car params) (car arguments))))))))
   ((primitive? function)
    (if (= (length arguments) (primitive-arity function))
        (apply (primitive-procedure function) arguments)
        (unreachable)))
   (else (unreachable))))

(define (apply-functor-value function argument)
  (evaluate-struct (functor-value-body function)
                   (renv-bind-module (functor-value-env function)
                                     (functor-value-param function)
                                     argument)))

(define (module-binding module name)
  (let ((found (assq name (module-value-bindings module))))
    (if found
        (cdr found)
        (unreachable))))

(define (module-descend value parts)
  (cond
   ((null? (cdr parts)) (cons value (car parts)))
   ((and (module-value? value) (assq (car parts) (module-value-modules value)))
    => (lambda (found) (module-descend (cdr found) (cdr parts))))
   (else (cons value (car parts)))))

(define (module-depth value parts)
  (let loop ((value value) (parts parts) (used 1))
    (cond
     ((null? parts) used)
     ((and (module-value? value) (assq (car parts) (module-value-modules value)))
      => (lambda (found) (loop (cdr found) (cdr parts) (+ used 1))))
     (else (+ used 1)))))

(define (drop-segments parts used)
  (if (= used 0) parts (drop-segments (cdr parts) (- used 1))))

(define (evaluate-path env parts)
  (let* ((head (car parts))
         (module-access? (and (capitalized? head) (pair? (cdr parts))))
         (entry (and module-access? (renv-lookup-module env head)))
         (used (if module-access? (module-depth entry (cdr parts)) 1))
         (base (cond
                (module-access?
                 (let ((walked (module-descend entry (cdr parts))))
                   (module-binding (car walked) (cdr walked))))
                ((capitalized? head) (make-variant head '()))
                (else (renv-lookup env head))))
         (rest (drop-segments parts used)))
    (let loop ((value base) (labels rest))
      (if (null? labels)
          value
          (loop (field-value value (car labels)) (cdr labels))))))

(define (evaluate expr env) (evaluate-at expr (form-span expr) env))

(define (evaluate-at expr where env)
  (let ((previous (current-expression))
        (previous-span (current-span)))
    (set-current-expression! expr)
    (set-current-span! where)
    (let ((value (evaluate-node expr env)))
      (set-current-expression! previous)
      (set-current-span! previous-span)
      value)))

(define (each-cell work cells)
  (if (null? cells)
      '()
      (cons (work (car cells) (car-span cells))
            (each-cell work (cdr cells)))))

(define (evaluate-node expr env)
  (cond
   ((or (number? expr) (string? expr) (boolean? expr)) expr)
   ((symbol? expr) (evaluate-path env (split-name expr)))
   ((pair? expr) (evaluate-compound expr env))
   (else (unreachable))))

(define (evaluate-compound expr env)
  (let ((head (car expr))
        (args (cdr expr)))
    (cond
     ((eq? head 'fun) (make-closure (cadr expr) (caddr expr) env))
     ((eq? head 'let)
      (let ((inner (let loop ((bindings (cadr expr)) (inner env))
                     (if (null? bindings)
                         inner
                         (loop (cdr bindings)
                               (bind-parameter inner (caar bindings)
                                               (evaluate-at
                                                (cadr (car bindings))
                                                (nth-span (car bindings) 1)
                                                env)))))))
        (evaluate-at (caddr expr) (nth-span expr 2) inner)))
     ((eq? head 'if)
      (if (evaluate-at (car args) (car-span args) env)
          (evaluate-at (cadr args) (nth-span args 1) env)
          (evaluate-at (caddr args) (nth-span args 2) env)))
     ((eq? head 'match) (evaluate-match expr env))
     ((eq? head 'record)
      (make-record-value (map (lambda (field)
                                (cons (car field)
                                      (evaluate-at (cadr field)
                                                   (nth-span field 1) env)))
                              args)))
     ((eq? head 'tuple)
      (make-tuple-value (each-cell (lambda (item span)
                                     (evaluate-at item span env))
                                   args)))
     ((eq? head 'dot)
      (field-value (evaluate-at (car args) (car-span args) env) (cadr args)))
     ((eq? head 'and)
      (let loop ((args args))
        (cond ((null? args) #t)
              ((evaluate-at (car args) (car-span args) env) (loop (cdr args)))
              (else #f))))
     ((eq? head 'or)
      (let loop ((args args))
        (cond ((null? args) #f)
              ((evaluate-at (car args) (car-span args) env) #t)
              (else (loop (cdr args))))))
     ((and (symbol? head) (capitalized? head) (not (qualified? head)))
      (make-variant head (each-cell (lambda (arg span)
                                      (evaluate-at arg span env))
                                    args)))
     (else
      (apply-values (evaluate-at head (nth-span expr 0) env)
                    (each-cell (lambda (arg span) (evaluate-at arg span env))
                               args))))))

(define (evaluate-match expr env)
  (let ((scrutinee (evaluate-at (cadr expr) (nth-span expr 1) env)))
    (let loop ((clauses (cddr expr)))
      (if (null? clauses)
          (unreachable)
          (let ((bindings (match-pattern (car (car clauses)) scrutinee)))
            (if bindings
                (evaluate-at (cadr (car clauses))
                             (nth-span (car clauses) 1)
                          (let loop ((bindings bindings) (inner env))
                            (if (null? bindings)
                                inner
                                (loop (cdr bindings)
                                      (renv-bind-value inner (caar bindings)
                                                       (cdar bindings))))))
                (loop (cdr clauses))))))))

(define (match-pattern pattern value)
  (cond
   ((eq? pattern '_) '())
   ((and (symbol? pattern) (not (capitalized? pattern)))
    (list (cons pattern value)))
   ((symbol? pattern)
    (and (variant? value)
         (eq? (variant-name value) pattern)
         (null? (variant-values value))
         '()))
   ((or (number? pattern) (string? pattern) (boolean? pattern))
    (and (structural=? pattern value) '()))
   ((and (pair? pattern) (eq? (car pattern) 'tuple))
    (and (tuple-value? value)
         (= (length (tuple-value-items value)) (length (cdr pattern)))
         (let loop ((patterns (cdr pattern))
                    (items (tuple-value-items value))
                    (bindings '()))
           (if (null? patterns)
               bindings
               (let ((matched (match-pattern (car patterns) (car items))))
                 (and matched
                      (loop (cdr patterns) (cdr items)
                            (append bindings matched))))))))
   ((pair? pattern)
    (and (variant? value)
         (eq? (variant-name value) (car pattern))
         (= (length (variant-values value)) (length (cdr pattern)))
         (let loop ((patterns (cdr pattern))
                    (values (variant-values value))
                    (bindings '()))
           (if (null? patterns)
               bindings
               (let ((matched (match-pattern (car patterns) (car values))))
                 (and matched
                      (loop (cdr patterns) (cdr values)
                            (append bindings matched))))))))
   (else (unreachable))))

(define (evaluate-definition form env)
  (if (symbol? (cadr form))
      (cons (cadr form) (evaluate-at (caddr form) (nth-span form 2) env))
      (let* ((name (car (cadr form)))
             (params (cdr (cadr form)))
             (cell (vector %undefined))
             (inner (renv-bind-cell env name cell))
             (value (make-closure params (caddr form) inner)))
        (vector-set! cell 0 value)
        (cons name value))))

(define (nested-body form)
  (let ((rest (cddr form)))
    (if (and (pair? rest) (eq? (car rest) ':)) (caddr rest) (car rest))))

(define (evaluate-struct form env)
  (let loop ((items (cdr form)) (env env) (bindings '()) (modules '()))
    (cond
     ((null? items)
      (make-module-value (reverse bindings) (reverse modules)))
     ((and (pair? (car items))
           (memq (car (car items)) '(type module-type)))
      (loop (cdr items) env bindings modules))
     ((and (pair? (car items)) (eq? (car (car items)) 'module))
      (let* ((name (cadr (car items)))
             (value (evaluate-struct (nested-body (car items)) env)))
        (loop (cdr items)
              (renv-bind-module env name value)
              bindings
              (cons (cons name value) modules))))
     (else
      (let ((binding (evaluate-definition (car items) env)))
        (loop (cdr items)
              (renv-bind-value env (car binding) (cdr binding))
              (cons binding bindings)
              modules))))))

(define (variant->string value nested?)
  (if (null? (variant-values value))
      (symbol->string (variant-name value))
      (let ((text (string-append
                   (symbol->string (variant-name value))
                   " "
                   (string-join (map (lambda (v) (value->string/nested v #t))
                                     (variant-values value))
                                " "))))
        (if nested? (string-append "(" text ")") text))))

(define (value->string/nested value nested?)
  (cond
   ((boolean? value) (if value "#t" "#f"))
   ((exact-integer? value) (number->string value))
   ((number? value) (number->string value))
   ((string? value) (form->string value))
   ((record-value? value)
    (string-append "{ "
                   (string-join (map (lambda (field)
                                       (string-append (symbol->string (car field))
                                                      " = "
                                                      (value->string/nested (cdr field) #f)))
                                     (record-value-fields value))
                                "; ")
                   " }"))
   ((tuple-value? value)
    (string-append "("
                   (string-join (map (lambda (item)
                                       (value->string/nested item #f))
                                     (tuple-value-items value))
                                ", ")
                   ")"))
   ((variant? value) (variant->string value nested?))
   ((or (closure? value) (primitive? value)) "<fun>")
   ((module-value? value) "<module>")
   ((functor-value? value) "<functor>")
   (else "<value>")))

(define (value->string value)
  (value->string/nested value #f))
