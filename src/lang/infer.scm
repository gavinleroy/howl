(define-module (lang infer)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme cxr)
  #:use-module (lang types)
  #:use-module (lang env)
  #:use-module (lang world)
  #:use-module (lang print)
  #:use-module (lang reader)
  #:export (initial-scope
            infer-expression
            elaborate-definition
            elaborate-type-declaration
            elaborate-signature
            elaborate-struct
            seal-modtype
            signature-abstract-modtype
            check-functor-body
            apply-functor
            modtype->text))

(define (binary type) (funcs (list type type) type))

(define (comparison)
  (let ((var (fresh-tvar 1)))
    (make-poly 0 (funcs (list var var) bool-type))))

(define (initial-scope)
  (let ((types (list (cons 'int int-type)
                     (cons 'float float-type)
                     (cons 'bool bool-type)
                     (cons 'string string-type)))
        (values
         (append
          (map (lambda (name) (cons name (binary int-type)))
               '(+ - * / mod))
          (map (lambda (name) (cons name (binary float-type)))
               '(+. -. *. /.))
          (list (cons '^ (binary string-type)))
          (map (lambda (name) (cons name (comparison)))
               '(= <> < > <= >=))
          (list (cons 'not (make-func (list bool-type) bool-type))
                (cons 'float-of-int (make-func (list int-type) float-type))
                (cons 'int-of-float (make-func (list float-type) int-type))
                (cons 'string-of-int (make-func (list int-type) string-type))
                (cons 'string-of-float (make-func (list float-type) string-type))
                (cons 'string-of-bool (make-func (list bool-type) string-type))
                (cons 'string-length (make-func (list string-type) int-type))
                (cons 'string-code-at
                      (funcs (list string-type int-type)
                             (make-variant-type
                              (list (cons 'Some (list int-type))
                                    (cons 'None '())))))
                (cons 'string-of-code (make-func (list int-type) string-type))))))
    (make-scope types values '() '())))

(define (constrain-because reason sub sup)
  (constrain-at reason (current-expression) (current-span) sub sup))

(define (constrain-at reason expression span sub sup)
  (let ((was-reason (current-reason))
        (was-expression (current-expression))
        (was-span (current-span))
        (was-context (current-context)))
    (set-current-reason! reason)
    (set-current-context! was-expression)
    (set-current-expression! expression)
    (set-current-span! span)
    (constrain sub sup)
    (set-current-expression! was-expression)
    (set-current-span! was-span)
    (set-current-context! was-context)
    (set-current-reason! was-reason)))

(define (tagged? form tag)
  (and (pair? form) (eq? (car form) tag) (proper-list? form)))

(define (field-access type label level)
  (let ((result (fresh-tvar level)))
    (constrain-because 'field type (make-record-type (list (cons label result))))
    result))

(define (descend-module entry parts module-name)
  (cond
   ((null? (cdr parts)) (cons entry (car parts)))
   ((not (modtype? entry)) (fail 'functor-misuse module-name))
   ((assq (car parts) (modtype-modules entry))
    => (lambda (found)
         (descend-module (cdr found) (cdr parts) (car parts))))
   (else (cons entry (car parts)))))

(define (module-value-type entry parts module-name level)
  (let* ((walked (descend-module entry parts module-name))
         (holder (car walked))
         (label (cdr walked)))
    (if (not (modtype? holder))
        (fail 'functor-misuse module-name)
        (let ((found (assq label (modtype-values holder))))
          (if found
              (instantiate (cdr found) level)
              (fail 'unbound label holder))))))

(define (module-path-length entry parts)
  (let loop ((entry entry) (parts parts) (used 1))
    (cond
     ((null? parts) used)
     ((and (modtype? entry) (assq (car parts) (modtype-modules entry)))
      => (lambda (found) (loop (cdr found) (cdr parts) (+ used 1))))
     (else (+ used 1)))))

(define (drop-used parts used)
  (if (= used 0) parts (drop-used (cdr parts) (- used 1))))

(define (infer-path scope parts level)
  (let* ((head (car parts))
         (module-access? (and (capitalized? head) (pair? (cdr parts))))
         (entry (and module-access? (lookup-module scope head)))
         (used (if module-access? (module-path-length entry (cdr parts)) 1))
         (base
          (cond
           (module-access? (module-value-type entry (cdr parts) head level))
           ((capitalized? head)
            (make-variant-type (list (cons head '()))))
           (else
            (let ((found (lookup-value scope head)))
              (if found
                  (instantiate found level)
                  (fail 'unbound head scope))))))
         (rest (drop-used parts used)))
    (let loop ((type base) (labels rest))
      (if (null? labels)
          type
          (loop (field-access type (car labels) level) (cdr labels))))))

(define (infer expr where scope level)
  (let ((previous (current-expression))
        (previous-span (current-span)))
    (set-current-expression! expr)
    (set-current-span! where)
    (let ((type (infer-form expr scope level)))
      (set-current-expression! previous)
      (set-current-span! previous-span)
      type)))

(define (infer-form expr scope level)
  (cond
   ((exact-integer? expr) int-type)
   ((and (number? expr) (inexact? expr)) float-type)
   ((string? expr) string-type)
   ((boolean? expr) bool-type)
   ((symbol? expr) (infer-path scope (split-name expr) level))
   ((and (pair? expr) (proper-list? expr)) (infer-compound expr scope level))
   (else (fail 'syntax "malformed expression"))))

(define (infer-compound expr scope level)
  (let ((head (car expr))
        (args (cdr expr)))
    (cond
     ((eq? head 'fun) (infer-fun expr scope level))
     ((eq? head 'let) (infer-let expr scope level))
     ((eq? head 'if) (infer-if args scope level))
     ((eq? head 'match) (infer-match expr scope level))
     ((eq? head 'record) (infer-record args scope level))
     ((eq? head 'tuple)
      (if (null? args)
          (fail 'syntax "a tuple needs at least one part")
          (make-tuple-type (map-cells
                            (lambda (item span) (infer item span scope level))
                            args))))
     ((eq? head 'dot) (infer-dot args scope level))
     ((or (eq? head 'and) (eq? head 'or))
      (for-each-cell
       (lambda (arg span)
         (constrain-at 'boolean-op arg span (infer arg span scope level)
                       bool-type))
       args)
      bool-type)
     ((and (symbol? head) (capitalized? head) (not (qualified? head)))
      (make-variant-type
       (list (cons head (map-cells (lambda (arg span)
                                     (infer arg span scope level))
                                   args)))))
     (else (infer-application head (nth-span expr 0) args scope level)))))

(define (map-cells work cells)
  (if (null? cells)
      '()
      (cons (work (car cells) (car-span cells))
            (map-cells work (cdr cells)))))

(define (for-each-cell work cells)
  (if (pair? cells)
      (begin (work (car cells) (car-span cells))
             (for-each-cell work (cdr cells)))))

(define (plain-name? form)
  (and (symbol? form) (not (capitalized? form))))

(define (tuple-pattern? form)
  (and (pair? form) (proper-list? form) (eq? (car form) 'tuple)
       (pair? (cdr form))
       (every-one? parameter? (cdr form))))

(define (parameter? form)
  (or (plain-name? form) (eq? form '_) (tuple-pattern? form)))

(define (every-one? ok? items)
  (cond ((null? items) #t)
        ((ok? (car items)) (every-one? ok? (cdr items)))
        (else #f)))

(define (parameter-types params cells level)
  (if (null? params)
      '()
      (cons (fresh-tvar-at level (car params) (car-span cells))
            (parameter-types (cdr params) (cdr cells) level))))

(define (parameter-bindings params cells types scope level)
  (if (null? params)
      '()
      (append (if (plain-name? (car params))
                  (list (cons (car params) (car types)))
                  (infer-pattern (car params) (car-span cells) (car types)
                                 scope level))
              (parameter-bindings (cdr params) (cdr cells) (cdr types)
                                  scope level))))

(define (parameter-list form) (checked-parameters (cadr form) form))

(define (checked-parameters params form)
  (let ((params params))
    (cond
     ((not (and (proper-list? params) (pair? params)))
      (fail 'syntax "malformed fun: expected (fun (parameter ...) body)"))
     ((any-of (lambda (param) (not (parameter? param))) params)
      (fail 'syntax "malformed parameter list"))
     ((not (= (length form) 3))
      (fail 'syntax "malformed fun: expected exactly one body"))
     (else params))))

(define (infer-fun expr scope level)
  (infer-lambda (parameter-list expr) (cadr expr)
                (caddr expr) (nth-span expr 2) scope level))

(define (infer-lambda params cells body body-span scope level)
  (let* ((types (parameter-types params cells level))
         (inner (bind-values scope
                             (parameter-bindings params cells types
                                                 scope level))))
    (funcs types (infer body body-span inner level))))

(define (binding-list form)
  (let ((bindings (cadr form)))
    (cond
     ((not (and (proper-list? bindings) (pair? bindings)))
      (fail 'syntax "malformed let: expected (let ((name expr) ...) body)"))
     ((not (= (length form) 3))
      (fail 'syntax "malformed let: expected exactly one body"))
     ((any-of (lambda (binding)
                (not (and (proper-list? binding)
                          (= (length binding) 2)
                          (parameter? (car binding)))))
              bindings)
      (fail 'syntax "malformed let binding"))
     (else bindings))))

(define (infer-let expr scope level)
  (let* ((bindings (binding-list expr))
         (inner (bind-values
                 scope
                 (apply append
                        (map-cells (lambda (binding span)
                                     (let-bindings binding scope level))
                                   bindings)))))
    (infer (caddr expr) (nth-span expr 2) inner level)))

(define (let-bindings binding scope level)
  (let ((value (cadr binding))
        (value-span (nth-span binding 1)))
    (if (plain-name? (car binding))
        (list (cons (car binding)
                    (generalize (infer value value-span scope (+ level 1))
                                level)))
        (infer-pattern (car binding) (nth-span binding 0)
                       (infer value value-span scope level)
                       scope level))))

(define (infer-if args scope level)
  (if (not (= (length args) 3))
      (fail 'syntax "malformed if: expected (if test then else)")
      (let ((result (fresh-tvar level)))
        (constrain-at 'condition (car args) (car-span args)
                      (infer (car args) (car-span args) scope level) bool-type)
        (constrain-at (list 'branch 'then) (cadr args) (nth-span args 1)
                      (infer (cadr args) (nth-span args 1) scope level) result)
        (constrain-at (list 'branch 'else) (caddr args) (nth-span args 2)
                      (infer (caddr args) (nth-span args 2) scope level)
                      result)
        result)))

(define (infer-record fields scope level)
  (make-record-type
   (map (lambda (field)
          (if (and (proper-list? field) (= (length field) 2) (symbol? (car field)))
              (cons (car field)
                    (infer (cadr field) (nth-span field 1) scope level))
              (fail 'syntax "malformed record field")))
        fields)))

(define (infer-dot args scope level)
  (if (not (and (= (length args) 2) (symbol? (cadr args))))
      (fail 'syntax "malformed field access: expected (dot expr label)")
      (field-access (infer (car args) (car-span args) scope level)
                    (cadr args) level)))

(define (infer-application head head-span args scope level)
  (if (null? args)
      (fail 'syntax "malformed application")
      (let ((function (infer head head-span scope level))
            (arguments (map-cells (lambda (arg span)
                                    (infer arg span scope level))
                                  args))
            (result (fresh-tvar level)))
        (constrain-because (list 'application head args)
                           function (make-func arguments result))
        result)))

(define (pattern-head pattern)
  (cond
   ((and (symbol? pattern) (capitalized? pattern)) (cons pattern 0))
   ((and (pair? pattern) (proper-list? pattern) (symbol? (car pattern))
         (capitalized? (car pattern)))
    (cons (car pattern) (length (cdr pattern))))
   (else #f)))

(define (clause-cases clauses level)
  (let loop ((clauses clauses) (cases '()))
    (if (null? clauses)
        (reverse cases)
        (let ((head (pattern-head (car (car clauses)))))
          (cond
           ((not head) (loop (cdr clauses) cases))
           ((assq (car head) cases)
            (let ((known (assq (car head) cases)))
              (if (= (length (cdr known)) (cdr head))
                  (loop (cdr clauses) cases)
                  (fail 'syntax "a case is matched with inconsistent arity"))))
           (else
            (loop (cdr clauses)
                  (cons (cons (car head)
                              (payload-types (car (car clauses))
                                             (cdr head) level))
                        cases))))))))

(define (payload-types pattern count level)
  (let loop ((cells (if (pair? pattern) (cdr pattern) '()))
             (count count)
             (found '()))
    (if (= count 0)
        (reverse found)
        (loop (if (pair? cells) (cdr cells) '())
              (- count 1)
              (cons (if (pair? cells)
                        (fresh-tvar-at level (car cells) (car-span cells))
                        (fresh-tvar level))
                    found)))))

(define (iota-list count)
  (let loop ((count count) (result '()))
    (if (= count 0) result (loop (- count 1) (cons count result)))))

(define (infer-match expr scope level)
  (if (< (length expr) 3)
      (fail 'syntax "malformed match")
      (let* ((scrutinee (infer (cadr expr) (nth-span expr 1) scope level))
             (clauses (cddr expr))
             (cases (clause-cases clauses level))
             (result (fresh-tvar level)))
        (when (pair? cases)
          (constrain-at 'scrutinee (cadr expr) (nth-span expr 1)
                        scrutinee (make-variant-type cases)))
        (for-each-cell
         (lambda (clause clause-span)
           (if (not (and (proper-list? clause) (= (length clause) 2)))
               (fail 'syntax "malformed match clause")
               (let* ((pattern (car clause))
                      (pattern-span (nth-span clause 0))
                      (head (pattern-head pattern))
                      (bindings
                       (if head
                           (let ((payloads (cdr (assq (car head) cases)))
                                 (sub (if (pair? pattern) (cdr pattern) '())))
                             (let loop ((sub sub) (payloads payloads) (bindings '()))
                               (if (null? sub)
                                   bindings
                                   (loop (cdr sub) (cdr payloads)
                                         (append bindings
                                                 (infer-pattern (car sub)
                                                                (car-span sub)
                                                                (car payloads)
                                                                scope level))))))
                           (infer-pattern pattern pattern-span scrutinee
                                          scope level))))
                 (constrain-at (list 'clause pattern) (cadr clause)
                               (nth-span clause 1)
                               (infer (cadr clause) (nth-span clause 1)
                                      (bind-values scope bindings) level)
                               result))))
         clauses)
        result)))

(define (infer-pattern pattern span type scope level)
  (cond
   ((eq? pattern '_) '())
   ((and (symbol? pattern) (not (capitalized? pattern)))
    (list (cons pattern type)))
   ((tuple-pattern? pattern)
    (let ((items (parameter-types (cdr pattern) (cdr pattern) level)))
      (constrain-at (list 'pattern pattern) pattern span type
                    (make-tuple-type items))
      (apply append
             (let loop ((cells (cdr pattern)) (types items) (found '()))
               (if (null? cells)
                   (reverse found)
                   (loop (cdr cells) (cdr types)
                         (cons (infer-pattern (car cells) (car-span cells)
                                              (car types) scope level)
                               found)))))))
   ((exact-integer? pattern)
    (constrain-at (list 'pattern pattern) pattern span type int-type) '())
   ((and (number? pattern) (inexact? pattern))
    (constrain-at (list 'pattern pattern) pattern span type float-type) '())
   ((string? pattern)
    (constrain-at (list 'pattern pattern) pattern span type string-type) '())
   ((boolean? pattern)
    (constrain-at (list 'pattern pattern) pattern span type bool-type) '())
   ((pattern-head pattern)
    => (lambda (head)
         (let* ((payloads (map (lambda (index) (fresh-tvar level))
                               (iota-list (cdr head))))
                (sub (if (pair? pattern) (cdr pattern) '())))
           (constrain-at (list 'pattern pattern) pattern span type
                         (make-variant-type
                          (list (cons (car head) payloads))))
           (let loop ((sub sub) (payloads payloads) (bindings '()))
             (if (null? sub)
                 bindings
                 (loop (cdr sub) (cdr payloads)
                       (append bindings
                               (infer-pattern (car sub) (car-span sub)
                                              (car payloads)
                                              scope level))))))))
   (else (fail 'syntax "malformed pattern"))))

(define (infer-expression expr span scope)
  (generalize (infer expr span scope 1) 0))

(define (quoted-parameter? form)
  (and (pair? form) (eq? (car form) 'quote) (symbol? (cadr form))))

(define (type-parameters form)
  (let loop ((rest (cddr form)) (params '()))
    (cond
     ((null? rest) (cons (reverse params) #f))
     ((quoted-parameter? (car rest))
      (loop (cdr rest) (cons (cadr (car rest)) params)))
     ((null? (cdr rest)) (cons (reverse params) (car rest)))
     (else (fail 'syntax "malformed type declaration")))))

(define (elaborate-type-declaration form scope)
  (if (not (and (proper-list? form) (>= (length form) 3) (symbol? (cadr form))))
      (fail 'syntax "malformed type declaration")
      (let* ((name (cadr form))
             (split (type-parameters form))
             (params (car split))
             (body (cdr split)))
        (if (not body)
            (fail 'syntax "malformed type declaration")
        (if (null? params)
            (let* ((node (make-nominal name))
                   (inner (bind-type scope name node)))
              (set-nominal-body! node (parse-type body inner))
              (cons name node))
            (let ((tycon (make-tycon name params body)))
              (set-tycon-scope! tycon (bind-type scope name tycon))
              (cons name tycon)))))))

(define (elaborate-definition form scope)
  (cond
   ((not (and (proper-list? form) (= (length form) 3)))
    (fail 'syntax "malformed define"))
   ((symbol? (cadr form))
    (cons (cadr form)
          (generalize (infer (caddr form) (nth-span form 2) scope 1) 0)))
   ((and (pair? (cadr form)) (proper-list? (cadr form)) (symbol? (car (cadr form))))
    (let* ((name (car (cadr form)))
           (params (cdr (cadr form)))
           (self (fresh-tvar-at 1 (cadr form) (nth-span form 1)))
           (inner (bind-value scope name self)))
      (constrain-at 'recursive (cadr form) (nth-span form 1)
                    (infer-lambda (checked-parameters params form)
                                  (cdr (cadr form))
                                  (caddr form) (nth-span form 2) inner 1)
                    self)
      (cons name (generalize self 0))))
   (else (fail 'syntax "malformed define"))))

(define (signature-item-kind item)
  (cond
   ((tagged? item 'type) 'type)
   ((tagged? item 'val) 'val)
   ((tagged? item 'module) 'module)
   (else (fail 'syntax "malformed signature item"))))

(define (module-spec-signature item scope)
  (if (not (and (= (length item) 4)
                (symbol? (cadr item))
                (eq? (caddr item) ':)))
      (fail 'syntax "malformed signature item")
      (resolve-signature-form (cadddr item) scope)))

(define (elaborate-signature form scope)
  (if (not (tagged? form 'sig))
      (fail 'syntax "malformed signature: expected (sig item ...)")
      (begin
        (for-each signature-item-kind (cdr form))
        (make-signature (cdr form) scope))))

(define (modtype-type-ref modtype name)
  (let ((found (assq name (modtype-types modtype))))
    (if found
        (cdr found)
        (fail 'unbound name modtype))))

(define (equivalent left right)
  (constrain-because 'equivalent left right)
  (constrain-because 'equivalent right left))

(define-record-type <view>
  (make-view view-scope view-types view-modules)
  view?
  (view-scope view-scope)
  (view-types view-types)
  (view-modules view-modules))

(define (signature-view sig modtype prefix)
  (let ((collected (vector '())))
    (let loop ((items (signature-items sig))
               (scope (signature-scope sig))
               (types '()))
      (if (null? items)
          (make-view scope (reverse types) (reverse (vector-ref collected 0)))
          (let* ((item (car items))
                 (kind (signature-item-kind item)))
            (cond
             ((eq? kind 'type)
              (let* ((name (cadr item))
                     (split (type-parameters item))
                     (params (car split))
                     (manifest? (and (cdr split) #t))
                     (type (cond
                            ((and manifest? (pair? params))
                             (let ((tycon (make-tycon
                                           (string->symbol
                                            (string-append
                                             prefix (symbol->string name)))
                                           params (cdr split))))
                               (set-tycon-scope! tycon scope)
                               tycon))
                            (manifest? (parse-type (cdr split) scope))
                            (modtype (modtype-type-ref modtype name))
                            ((pair? params)
                             (make-tycon (fresh-type-name
                                          (string-append prefix
                                                         (symbol->string name)))
                                         params #f))
                            (else (make-abstract
                                   (string-append prefix
                                                  (symbol->string name)))))))
                (when (and manifest? modtype (not (pair? params)))
                  (equivalent (modtype-type-ref modtype name) type))
                (loop (cdr items)
                      (bind-type scope name type)
                      (cons (cons name type) types))))
             ((eq? kind 'module)
              (let* ((name (cadr item))
                     (entry (signature-module-entry item scope modtype prefix)))
                (vector-set! collected 0
                             (cons (cons name entry) (vector-ref collected 0)))
                (loop (cdr items) (bind-module scope name entry) types)))
             (else (loop (cdr items) scope types))))))))

(define %module-entry (vector #f))

(define (signature-module-entry item scope modtype prefix)
  ((vector-ref %module-entry 0) item scope modtype prefix))

(define (resolve-module-entry item scope modtype prefix)
  (let* ((name (cadr item))
         (nested (module-spec-signature item scope))
         (path (string-append prefix (symbol->string name) ".")))
    (if modtype
        (let ((entry (modtype-module-ref modtype name)))
          (check-against-signature entry nested path)
          entry)
        (signature-abstract-modtype nested path))))

(define (modtype-module-ref modtype name)
  (let ((found (assq name (modtype-modules modtype))))
    (if found
        (cdr found)
        (fail 'unbound name modtype))))

(define (signature-values sig scope mode)
  (let loop ((items (signature-items sig)) (values '()))
    (cond
     ((null? items) (reverse values))
     ((eq? (signature-item-kind (car items)) 'val)
      (let ((item (car items)))
        (if (not (and (= (length item) 3) (symbol? (cadr item))))
            (fail 'syntax "malformed signature item")
            (loop (cdr items)
                  (cons (cons (cadr item)
                              (with-type-variables
                               mode
                               (lambda () (parse-type (caddr item) scope))))
                        values)))))
     (else (loop (cdr items) values)))))

(define (check-value name entry required)
  (let ((actual (instantiate entry 0))
        (written (if (poly? entry) (poly-body entry) entry)))
    (guard (e (#t (if (diagnostic? e)
                      (fail 'type-mismatch written required)
                      (raise e))))
      (constrain-because 'signature actual required))))

(define (check-signature-values sig scope modtype)
  (for-each (lambda (binding)
              (let ((found (assq (car binding) (modtype-values modtype)))
                    (item (signature-item-for sig (car binding))))
                (if (not found)
                    (fail 'unbound (car binding) modtype)
                    (checking-item
                     item
                     (lambda ()
                       (check-value (car binding) (cdr found)
                                    (cdr binding)))))))
            (signature-values sig scope 'rigid)))

(define (signature-item-for sig name)
  (find-first (lambda (item)
                (and (tagged? item 'val) (= (length item) 3)
                     (eq? (cadr item) name)))
              (signature-items sig)))

(define (checking-item item work)
  (let ((was-expression (current-expression))
        (was-span (current-span)))
    (if item
        (begin (set-current-expression! item)
               (set-current-span! (form-span item))))
    (let ((outcome (work)))
      (set-current-expression! was-expression)
      (set-current-span! was-span)
      outcome)))

(define (signature-abstract-modtype sig prefix)
  (let* ((view (signature-view sig #f prefix))
         (scope (view-scope view)))
    (make-modtype (view-types view)
                  (map (lambda (binding)
                         (cons (car binding) (generalize (cdr binding) 0)))
                       (signature-values sig scope 'flexible))
                  (view-modules view))))

(define (check-against-signature modtype sig prefix)
  (let ((view (signature-view sig modtype prefix)))
    (check-signature-values sig (view-scope view) modtype)))

(define (resolve-signature-form form scope)
  (cond
   ((symbol? form) (lookup-signature scope form))
   ((tagged? form 'sig) (elaborate-signature form scope))
   (else (fail 'syntax "malformed signature: expected (sig item ...)"))))

(define (seal-modtype modtype sig prefix)
  (check-against-signature modtype sig prefix)
  (signature-abstract-modtype sig prefix))

(vector-set! %module-entry 0 resolve-module-entry)

(define (elaborate-struct form scope prefix)
  (if (not (tagged? form 'struct))
      (fail 'syntax "malformed module body: expected (struct item ...)")
      (let loop ((items (cdr form))
                 (scope scope)
                 (types '())
                 (values '())
                 (modules '()))
        (cond
         ((null? items)
          (make-modtype (reverse types) (reverse values) (reverse modules)))
         ((tagged? (car items) 'type)
          (let ((binding (elaborate-type-declaration (car items) scope)))
            (loop (cdr items)
                  (bind-type scope (car binding) (cdr binding))
                  (cons binding types)
                  values
                  modules)))
         ((tagged? (car items) 'define)
          (let ((binding (elaborate-definition (car items) scope)))
            (loop (cdr items)
                  (bind-value scope (car binding) (cdr binding))
                  types
                  (cons binding values)
                  modules)))
         ((tagged? (car items) 'module-type)
          (let ((sig (elaborate-signature (caddr (car items)) scope)))
            (loop (cdr items)
                  (bind-signature scope (cadr (car items)) sig)
                  types
                  values
                  modules)))
         ((tagged? (car items) 'module)
          (let* ((binding (elaborate-nested-module (car items) scope prefix))
                 (name (car binding)))
            (loop (cdr items)
                  (bind-module scope name (cdr binding))
                  types
                  values
                  (cons binding modules))))
         (else (fail 'syntax "malformed struct item"))))))

(define (nested-module-parts form)
  (let ((rest (cddr form)))
    (if (and (pair? rest) (eq? (car rest) ':))
        (if (= (length rest) 3)
            (cons (cadr rest) (caddr rest))
            (fail 'syntax "malformed module"))
        (if (= (length rest) 1)
            (cons #f (car rest))
            (fail 'syntax "malformed module")))))

(define (elaborate-nested-module form scope prefix)
  (let* ((name (cadr form))
         (parts (nested-module-parts form))
         (signature-form (car parts))
         (body (cdr parts))
         (path (string-append prefix (symbol->string name) "."))
         (modtype (if (tagged? body 'struct)
                      (elaborate-struct body scope path)
                      (fail 'syntax "malformed module body: expected (struct ...) or (Functor Argument)"))))
    (cons name
          (if signature-form
              (seal-modtype modtype
                            (resolve-signature-form signature-form scope)
                            path)
              modtype))))

(define (check-functor-body decl)
  (let ((param (functor-param decl)))
    (elaborate-struct
     (functor-body decl)
     (bind-module (functor-scope decl) param
                  (signature-abstract-modtype
                   (functor-signature decl)
                   (string-append (symbol->string param) ".")))
     "")))

(define (apply-functor decl argument argument-name)
  (if (not (modtype? argument))
      (fail 'functor-misuse argument-name)
      (begin
        (check-against-signature argument (functor-signature decl)
                                 (string-append (symbol->string argument-name) "."))
        (elaborate-struct (functor-body decl)
                          (bind-module (functor-scope decl)
                                       (functor-param decl)
                                       argument)
                          ""))))

(define (type-item->text name type)
  (cond
   ((tycon? type)
    (string-append "type " (symbol->string name)
                   (apply string-append
                          (map (lambda (param)
                                 (string-append " '" (symbol->string param)))
                               (tycon-params type)))))
   ((and (nominal? type) (not (nominal-body type)))
    (string-append "type " (symbol->string name)))
   (else
    (string-append "type " (symbol->string name) " = "
                   (type->text (if (nominal? type) (nominal-body type) type))))))

(define (modtype->text modtype)
  (string-append
   "sig "
   (string-join
    (append (map (lambda (binding) (type-item->text (car binding) (cdr binding)))
                 (modtype-types modtype))
            (map (lambda (binding)
                   (string-append "module " (symbol->string (car binding))))
                 (modtype-modules modtype))
            (map (lambda (binding)
                   (string-append "val " (symbol->string (car binding))
                                  " : " (entry->text (cdr binding))))
                 (modtype-values modtype)))
    "; ")
   " end"))
