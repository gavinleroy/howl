(define-module (lang env)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme char)
  #:use-module (scheme cxr)
  #:use-module (lang types)
  #:use-module (lang world)
  #:export (make-modtype
            modtype?
            modtype-types
            modtype-values
            modtype-modules
            make-functor-type
            functor-type?
            functor-param
            functor-signature
            functor-body
            functor-scope
            make-signature
            signature?
            signature-items
            signature-scope
            make-scope
            scope?
            scope-types
            scope-values
            scope-modules
            scope-signatures
            bind-type
            bind-value
            bind-values
            bind-module
            bind-signature
            lookup-type
            lookup-value
            lookup-module
            lookup-signature
            split-name
            capitalized?
            qualified?
            parse-type
            with-type-variables
            make-tycon
            tycon?
            tycon-name
            tycon-params
            tycon-body
            tycon-scope
            set-tycon-scope!
            apply-tycon))

(define-record-type <modtype>
  (make-modtype modtype-types modtype-values modtype-modules)
  modtype?
  (modtype-types modtype-types)
  (modtype-values modtype-values)
  (modtype-modules modtype-modules))

(define-record-type <functor-type>
  (make-functor-type functor-param functor-signature functor-body functor-scope)
  functor-type?
  (functor-param functor-param)
  (functor-signature functor-signature)
  (functor-body functor-body)
  (functor-scope functor-scope))

(define-record-type <signature>
  (make-signature signature-items signature-scope)
  signature?
  (signature-items signature-items)
  (signature-scope signature-scope))

(define-record-type <scope>
  (make-scope scope-types scope-values scope-modules scope-signatures)
  scope?
  (scope-types scope-types)
  (scope-values scope-values)
  (scope-modules scope-modules)
  (scope-signatures scope-signatures))

(define (bind-type scope name type)
  (make-scope (cons (cons name type) (scope-types scope))
              (scope-values scope)
              (scope-modules scope)
              (scope-signatures scope)))

(define (bind-value scope name entry)
  (make-scope (scope-types scope)
              (cons (cons name entry) (scope-values scope))
              (scope-modules scope)
              (scope-signatures scope)))

(define (bind-values scope bindings)
  (if (null? bindings)
      scope
      (bind-values (bind-value scope (caar bindings) (cdar bindings))
                   (cdr bindings))))

(define (bind-module scope name entry)
  (make-scope (scope-types scope)
              (scope-values scope)
              (cons (cons name entry) (scope-modules scope))
              (scope-signatures scope)))

(define (bind-signature scope name sig)
  (make-scope (scope-types scope)
              (scope-values scope)
              (scope-modules scope)
              (cons (cons name sig) (scope-signatures scope))))

(define (lookup-type scope name)
  (let ((found (assq name (scope-types scope))))
    (if found
        (cdr found)
        (fail 'unbound name scope))))

(define (lookup-value scope name)
  (let ((found (assq name (scope-values scope))))
    (and found (cdr found))))

(define (lookup-module scope name)
  (let ((found (assq name (scope-modules scope))))
    (if found
        (cdr found)
        (fail 'unbound name scope))))

(define (lookup-signature scope name)
  (let ((found (assq name (scope-signatures scope))))
    (if found
        (cdr found)
        (fail 'unbound name scope))))

(define (dot-segments text)
  (let ((size (string-length text)))
    (let loop ((index 0) (start 0) (parts '()))
      (cond
       ((= index size) (reverse (cons (substring text start size) parts)))
       ((char=? (string-ref text index) #\.)
        (loop (+ index 1) (+ index 1) (cons (substring text start index) parts)))
       (else (loop (+ index 1) start parts))))))

(define (split-name name)
  (let ((parts (dot-segments (symbol->string name))))
    (if (any-of (lambda (part) (= (string-length part) 0)) parts)
        (list name)
        (map string->symbol parts))))

(define (capitalized? name)
  (let ((text (symbol->string name)))
    (and (> (string-length text) 0)
         (char-upper-case? (string-ref text 0)))))

(define (qualified? name)
  (> (length (split-name name)) 1))

(define (descend-modules entry parts name)
  (cond
   ((null? (cdr parts)) (cons entry (car parts)))
   ((not (modtype? entry)) (fail 'functor-misuse (car parts)))
   ((assq (car parts) (modtype-modules entry))
    => (lambda (found) (descend-modules (cdr found) (cdr parts) name)))
   ((null? (cddr parts)) (cons entry (car parts)))
   (else (fail 'unbound name entry))))

(define (module-type-component scope name)
  (let* ((parts (split-name name))
         (entry (lookup-module scope (car parts))))
    (if (< (length parts) 2)
        (fail 'syntax "malformed type name")
        (let* ((walked (descend-modules entry (cdr parts) name))
               (holder (car walked))
               (label (cdr walked)))
          (if (not (modtype? holder))
              (fail 'functor-misuse (car parts))
              (let ((found (assq label (modtype-types holder))))
                (if found
                    (cdr found)
                    (fail 'unbound name holder))))))))

(define-record-type <tycon>
  (%make-tycon tycon-name tycon-params tycon-body tycon-scope)
  tycon?
  (tycon-name tycon-name)
  (tycon-params tycon-params)
  (tycon-body tycon-body)
  (tycon-scope tycon-scope set-tycon-scope!))

(define (make-tycon name params body)
  (%make-tycon name params body #f))

(define %applications (vector '()))

(define (same-arguments? left right)
  (cond ((and (null? left) (null? right)) #t)
        ((or (null? left) (null? right)) #f)
        ((eq? (car left) (car right)) (same-arguments? (cdr left) (cdr right)))
        (else #f)))

(define (find-application tycon args)
  (let loop ((entries (vector-ref %applications 0)))
    (cond ((null? entries) #f)
          ((and (eq? (vector-ref (car entries) 0) tycon)
                (same-arguments? (vector-ref (car entries) 1) args))
           (vector-ref (car entries) 2))
          (else (loop (cdr entries))))))

(define (apply-tycon tycon args)
  (cond
   ((not (= (length args) (length (tycon-params tycon))))
    (fail 'type-arity (tycon-name tycon)
          (length (tycon-params tycon)) (length args)))
   ((find-application tycon args))
   (else
    (let ((node (make-applied (tycon-name tycon) args
                              (lambda (fresh) (apply-tycon tycon fresh)))))
      (vector-set! %applications 0
                   (cons (vector tycon args node) (vector-ref %applications 0)))
      (when (tycon-body tycon)
        (set-nominal-body!
         node
         (with-type-variables
          'rigid
          (lambda ()
            (for-each bind-type-variable! (tycon-params tycon) args)
            (parse-type (tycon-body tycon) (tycon-scope tycon))))))
      node))))

(define %variables (vector #f))

(define (with-type-variables mode thunk)
  (let ((previous (vector-ref %variables 0)))
    (vector-set! %variables 0 (and mode (cons mode '())))
    (let ((result (thunk)))
      (vector-set! %variables 0 previous)
      result)))

(define (bind-type-variable! name type)
  (let ((state (vector-ref %variables 0)))
    (when state
      (vector-set! %variables 0 (cons (car state)
                                      (cons (cons name type) (cdr state)))))))

(define (type-variable name)
  (let ((state (vector-ref %variables 0)))
    (cond
     ((not state) (fail 'type-variable-not-allowed name))
     ((assq name (cdr state)) => cdr)
     (else
      (let ((fresh (if (eq? (car state) 'rigid)
                       (make-abstract (string-append "'" (symbol->string name)))
                       (fresh-tvar 1))))
        (bind-type-variable! name fresh)
        fresh)))))

(define (parse-recursive form scope)
  (if (not (and (= (length form) 3)
                (pair? (cadr form))
                (eq? (car (cadr form)) 'quote)
                (symbol? (cadr (cadr form)))))
      (fail 'syntax "malformed type")
      (let* ((name (cadr (cadr form)))
             (node (make-nominal name))
             (state (vector-ref %variables 0)))
        (if (not state)
            (fail 'type-variable-not-allowed name)
            (let ((previous (cdr state)))
              (vector-set! %variables 0
                           (cons (car state) (cons (cons name node) previous)))
              (set-nominal-body! node (parse-type (caddr form) scope))
              (vector-set! %variables 0 (cons (car state) previous))
              node)))))

(define (parse-field form scope)
  (if (and (proper-list? form) (= (length form) 2) (symbol? (car form)))
      (cons (car form) (parse-type (cadr form) scope))
      (fail 'syntax "malformed field")))

(define (parse-case form scope)
  (cond
   ((and (symbol? form) (capitalized? form)) (cons form '()))
   ((and (proper-list? form) (pair? form) (symbol? (car form))
         (capitalized? (car form)))
    (cons (car form)
          (map (lambda (payload) (parse-type payload scope)) (cdr form))))
   (else (fail 'syntax "malformed case"))))

(define (parse-type form scope)
  (cond
   ((symbol? form)
    (let ((found (if (qualified? form)
                     (module-type-component scope form)
                     (lookup-type scope form))))
      (if (tycon? found)
          (apply-tycon found '())
          found)))
   ((not (and (pair? form) (proper-list? form)))
    (fail 'syntax "malformed type"))
   ((eq? (car form) '->)
    (if (and (= (length form) 3)
             (proper-list? (cadr form)))
        (make-func (map (lambda (part) (parse-type part scope)) (cadr form))
                   (parse-type (caddr form) scope))
        (fail 'syntax
              "malformed type: -> needs an argument list and a result")))
   ((eq? (car form) 'quote)
    (if (and (= (length form) 2) (symbol? (cadr form)))
        (type-variable (cadr form))
        (fail 'syntax "malformed type")))
   ((eq? (car form) 'rec) (parse-recursive form scope))
   ((and (symbol? (car form))
         (not (memq (car form) '(-> record tuple enum quote rec))))
    (let ((found (if (qualified? (car form))
                     (module-type-component scope (car form))
                     (lookup-type scope (car form)))))
      (cond
       ((tycon? found)
        (apply-tycon found (map (lambda (arg) (parse-type arg scope)) (cdr form))))
       ((null? (cdr form)) found)
       (else (fail 'type-arity (car form) 0 (length (cdr form)))))))
   ((eq? (car form) 'record)
    (make-record-type (map (lambda (field) (parse-field field scope)) (cdr form))))
   ((eq? (car form) 'tuple)
    (make-tuple-type (map (lambda (item) (parse-type item scope)) (cdr form))))
   ((eq? (car form) 'enum)
    (make-variant-type (map (lambda (variant) (parse-case variant scope))
                            (cdr form))))
   (else (fail 'syntax "malformed type"))))
