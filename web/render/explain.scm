(define-module (render explain)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme cxr)
  #:use-module (lang types)
  #:use-module (lang env)
  #:use-module (lang eval)
  #:use-module (lang world)
  #:export (closest-name
            diagnostic-schema
            text
            code
            type-node
            brief-node
            variable-node
            expression-node
            value-node
            paragraph
            section
            bullets
            steps
            table
            note
            field
            scope-of
            bindings-of
            binding-names
            near-misses
            provenance
            define-explainer
            register-explainer!
            explain))

(define (edit-distance left right)
  (let ((rows (string-length left))
        (columns (string-length right)))
    (let ((row (make-vector (+ columns 1) 0)))
      (do ((column 0 (+ column 1))) ((> column columns))
        (vector-set! row column column))
      (do ((index 1 (+ index 1))) ((> index rows) (vector-ref row columns))
        (let ((diagonal (vector-ref row 0)))
          (vector-set! row 0 index)
          (do ((column 1 (+ column 1))) ((> column columns))
            (let ((above (vector-ref row column)))
              (vector-set! row column
                           (if (char=? (string-ref left (- index 1))
                                       (string-ref right (- column 1)))
                               diagonal
                               (+ 1 (min diagonal
                                         above
                                         (vector-ref row (- column 1))))))
              (set! diagonal above))))))))

(define (closest-name name candidates)
  (let ((text (symbol->string name)))
    (let loop ((candidates candidates) (best #f) (distance 3))
      (cond
       ((null? candidates) best)
       ((eq? (car candidates) name) (loop (cdr candidates) best distance))
       (else
        (let ((measured (edit-distance text (symbol->string (car candidates)))))
          (if (and (< measured distance) (< measured (string-length text)))
              (loop (cdr candidates) (car candidates) measured)
              (loop (cdr candidates) best distance))))))))

(define (diagnostic-schema tag)
  (cond
   ((eq? tag 'syntax) '(("detail" text 0)))
   ((eq? tag 'unbound) '(("name" name 0)))
   ((memq tag '(type-mismatch unresolved-union unsatisfiable-demand))
    '(("actual" type 0) ("expected" type 1)))
   ((eq? tag 'functor-misuse) '(("module" name 0)))
   ((eq? tag 'type-arity)
    '(("name" name 0) ("declared" count 1) ("used" count 2)))
   ((eq? tag 'type-variable-not-allowed) '(("name" name 0)))
   ((eq? tag 'undefined-during-definition) '(("name" name 0)))
   ((eq? tag 'division-by-zero) '(("operator" text 0)))
   (else '())))

(define (present children)
  (let loop ((children children) (kept '()))
    (cond ((null? children) (reverse kept))
          ((car children) (loop (cdr children) (cons (car children) kept)))
          (else (loop (cdr children) kept)))))

(define (text value) (list 'text value))
(define (code value)
  (list 'code (if (symbol? value) (symbol->string value) value)))
(define (type-node type) (and type (list 'type type)))
(define (variable-node variable) (and variable (list 'variable variable)))
(define (brief-node type) (and type (list 'brief type)))
(define (expression-node form) (and form (list 'expression form)))
(define (value-node value) (list 'value value))
(define (paragraph . children) (cons 'paragraph (present children)))
(define (section title . children) (cons* 'section title (present children)))
(define (bullets . items) (cons 'bullets (present items)))
(define (steps . items) (cons 'steps (present items)))
(define (note . children) (cons 'note (present children)))
(define (table columns rows) (list 'table columns rows))

(define (cons* a b rest) (cons a (cons b rest)))

(define (field diagnostic name)
  (let ((schema (find-first (lambda (item) (string=? (car item) name))
                            (diagnostic-schema (diagnostic-tag diagnostic))))
        (data (diagnostic-data diagnostic)))
    (and schema
         (< (caddr schema) (length data))
         (list-ref data (caddr schema)))))

(define (scope-of diagnostic)
  (or (field diagnostic "scope")
      (let ((world (diagnostic-world diagnostic)))
        (and (world? world) (scope? (world-scope world)) (world-scope world)))))

(define (bindings-of diagnostic)
  (let ((scope (scope-of diagnostic)))
    (if (scope? scope) (scope-values scope) '())))

(define (binding-names diagnostic)
  (map car (bindings-of diagnostic)))

(define (near-misses name candidates)
  (let ((candidate (closest-name name candidates)))
    (if candidate (list candidate) '())))

(define (bound-row variable direction bound)
  (let ((origin (bound-origin bound)))
    (list (variable-node variable)
          (text direction)
          (brief-node (bound-type bound))
          (or (expression-node (and (origin? origin)
                                    (origin-expression origin)))
              (text "-"))
          (if (and (origin? origin) (origin-left origin))
              (paragraph (brief-node (origin-left origin))
                         (text " flows into ")
                         (brief-node (origin-right origin)))
              (text "-")))))

(define (seen? table item)
  (let loop ((items (vector-ref table 0)))
    (cond ((null? items) #f)
          ((eq? (car items) item) #t)
          (else (loop (cdr items))))))

(define (remember! table item)
  (vector-set! table 0 (cons item (vector-ref table 0))))

(define (collect-variables type table)
  (if (or (not type) (seen? table type))
      '()
      (begin
        (remember! table type)
        (cond
         ((tvar? type)
          (cons type
                (append (collect-list (bound-types (tvar-lower type)) table)
                        (collect-list (bound-types (tvar-upper type)) table))))
         ((func? type)
          (append (apply append
                         (map (lambda (domain)
                                (collect-variables domain table))
                              (func-domains type)))
                  (collect-variables (func-codomain type) table)))
         ((record-type? type)
          (collect-list (map cdr (record-type-fields type)) table))
         ((variant-type? type)
          (collect-list (apply append (map cdr (variant-type-cases type))) table))
         (else '())))))

(define (collect-list types table)
  (if (null? types)
      '()
      (append (collect-variables (car types) table)
              (collect-list (cdr types) table))))

(define (constrained-variables types)
  (let ((table (vector '())))
    (let loop ((types types) (found '()))
      (if (null? types)
          (let keep ((variables found) (kept '()))
            (cond
             ((null? variables) (reverse kept))
             ((or (pair? (tvar-lower (car variables)))
                  (pair? (tvar-upper (car variables))))
              (keep (cdr variables) (cons (car variables) kept)))
             (else (keep (cdr variables) kept))))
          (loop (cdr types)
                (append found (collect-variables (car types) table)))))))

(define (variable-rows variable)
  (append (map (lambda (bound) (bound-row variable "accepts" bound))
               (tvar-lower variable))
          (map (lambda (bound) (bound-row variable "must satisfy" bound))
               (tvar-upper variable))))

(define (constraint-types diagnostic)
  (let ((constraint (diagnostic-constraint diagnostic)))
    (if constraint (list (car constraint) (cdr constraint)) '())))

(define (provenance diagnostic . extra)
  (let* ((variables (constrained-variables
                     (append (present extra) (constraint-types diagnostic))))
         (rows (apply append (map variable-rows variables))))
    (and (pair? rows)
         (section "Where these constraints came from"
                  (table (list "variable" "direction" "bound" "from"
                               "while checking")
                         rows)))))

(define (while-checking diagnostic)
  (let ((constraint (diagnostic-constraint diagnostic)))
    (and constraint
         (paragraph (text "The checker was asking whether ")
                    (brief-node (car constraint))
                    (text " can be used as ")
                    (brief-node (cdr constraint))
                    (text ".")))))

(define %explainers (vector '()))

(define (register-explainer! tag explainer)
  (vector-set! %explainers 0
               (cons (cons tag explainer) (vector-ref %explainers 0))))

(define-syntax define-explainer
  (syntax-rules ()
    ((_ tag (parameter) body ...)
     (register-explainer! 'tag (lambda (parameter) body ...)))))

(define (required-paragraph diagnostic prefix)
  (let ((required (field diagnostic "expected")))
    (and required
         (paragraph (text prefix) (brief-node required) (text ".")))))

(define (where-paragraph diagnostic)
  (let ((expr (diagnostic-expr diagnostic)))
    (and expr (paragraph (text "In ") (expression-node expr) (text ".")))))

(define (binding-rows diagnostic)
  (let loop ((bindings (bindings-of diagnostic)) (rows '()))
    (if (null? bindings)
        (reverse rows)
        (loop (cdr bindings)
              (cons (list (code (caar bindings))
                          (type-node (let ((entry (cdar bindings)))
                                       (if (poly? entry)
                                           (poly-body entry)
                                           entry))))
                    rows)))))

(define (in-scope-section diagnostic)
  (let ((rows (binding-rows diagnostic)))
    (and (pair? rows)
         (section "In scope here" (table (list "name" "type") rows)))))

(define-explainer type-mismatch (diagnostic)
  (section "Type mismatch"
           (paragraph (text "This expression has type ")
                      (type-node (field diagnostic "actual"))
                      (text " but the code around it needs ")
                      (type-node (field diagnostic "expected"))
                      (text "."))
           (where-paragraph diagnostic)
           (while-checking diagnostic)
           (provenance diagnostic
                       (field diagnostic "actual")
                       (field diagnostic "expected"))))

(define-explainer not-a-function (diagnostic)
  (section "Not a function"
           (paragraph (text "Something here has to be a function, but it has type ")
                      (type-node (field diagnostic "actual"))
                      (text "."))
           (required-paragraph diagnostic "It is used as ")
           (where-paragraph diagnostic)
           (while-checking diagnostic)
           (provenance diagnostic
                       (field diagnostic "actual")
                       (field diagnostic "expected"))))

(define-explainer not-a-record (diagnostic)
  (section "Not a record"
           (paragraph (text "Something here has to be a record, but it has type ")
                      (type-node (field diagnostic "actual"))
                      (text "."))
           (required-paragraph diagnostic "The fields asked for are ")
           (where-paragraph diagnostic)
           (while-checking diagnostic)
           (provenance diagnostic
                       (field diagnostic "actual")
                       (field diagnostic "expected"))))

(define-explainer not-a-variant (diagnostic)
  (section "Not a variant"
           (paragraph (text "Something here has to be a variant, but it has type ")
                      (type-node (field diagnostic "actual"))
                      (text "."))
           (required-paragraph diagnostic "The cases asked for are ")
           (where-paragraph diagnostic)
           (while-checking diagnostic)
           (provenance diagnostic
                       (field diagnostic "actual")
                       (field diagnostic "expected"))))

(define-explainer missing-field (diagnostic)
  (let* ((record (field diagnostic "actual"))
         (label (field diagnostic "field"))
         (labels (if (record-type? record)
                     (map car (record-type-fields record))
                     '()))
         (suggestions (near-misses label labels)))
    (section "No such field"
             (paragraph (text "This record has type ")
                        (type-node record)
                        (text ", which has no field ")
                        (code label)
                        (text "."))
             (and (pair? suggestions)
                  (paragraph (text "Did you mean ")
                             (code (car suggestions))
                             (text "?")))
             (and (pair? labels)
                  (section "Fields it does have"
                           (table (list "field" "type")
                                  (map (lambda (entry)
                                         (list (code (car entry))
                                               (type-node (cdr entry))))
                                       (record-type-fields record)))))
             (where-paragraph diagnostic))))

(define-explainer unhandled-case (diagnostic)
  (let* ((expected (field diagnostic "expected"))
         (missing (field diagnostic "case"))
         (handled (if (variant-type? expected)
                      (map car (variant-type-cases expected))
                      '())))
    (section "Unhandled case"
             (paragraph (text "This value can be ")
                        (code missing)
                        (text ", which the match does not handle."))
             (and (pair? handled)
                  (section "Cases the match handles"
                           (apply bullets (map (lambda (name) (code name))
                                               handled))))
             (where-paragraph diagnostic))))

(define-explainer case-arity (diagnostic)
  (section "Wrong number of arguments"
           (paragraph (text "The case ")
                      (code (field diagnostic "case"))
                      (text " is declared with ")
                      (code (number->string (field diagnostic "declared")))
                      (text " argument(s) but used with ")
                      (code (number->string (field diagnostic "used")))
                      (text "."))
           (where-paragraph diagnostic)))

(define-explainer unbound-value (diagnostic)
  (let* ((name (field diagnostic "name"))
         (suggestions (near-misses name (binding-names diagnostic))))
    (section "Unbound value"
             (paragraph (text "There is no value named ")
                        (code name)
                        (text " in scope here."))
             (and (pair? suggestions)
                  (paragraph (text "Did you mean ")
                             (code (car suggestions))
                             (text "?")))
             (in-scope-section diagnostic))))

(define-explainer signature-value-mismatch (diagnostic)
  (section "Signature mismatch"
           (paragraph (text "The module defines ")
                      (code (field diagnostic "name"))
                      (text " with type ")
                      (type-node (field diagnostic "actual"))
                      (text ", but the signature requires ")
                      (type-node (field diagnostic "expected"))
                      (text "."))
           (note (text (string-append "Sealing checks that the implementation "
                                      "is at least as general as the "
                                      "signature asks.")))))

(define-explainer missing-signature-value (diagnostic)
  (section "Missing implementation"
           (paragraph (text "The signature requires a value named ")
                      (code (field diagnostic "name"))
                      (text ", which this module does not define."))
           (in-scope-section diagnostic)))

(define-explainer missing-signature-type (diagnostic)
  (section "Missing implementation"
           (paragraph (text "The signature requires a type named ")
                      (code (field diagnostic "name"))
                      (text ", which this module does not define."))))

(define-explainer division-by-zero (diagnostic)
  (section "Division by zero"
           (paragraph (text "The right operand of ")
                      (code (field diagnostic "operator"))
                      (text " was zero."))
           (where-paragraph diagnostic)))

(define-explainer no-matching-clause (diagnostic)
  (section "No clause matched"
           (paragraph (text "This value reached the end of a match: ")
                      (value-node (field diagnostic "value"))
                      (text "."))
           (where-paragraph diagnostic)))

(define-explainer unreadable-source (diagnostic)
  (section "Cannot read this program"
           (paragraph (text (field diagnostic "detail")))))

(define (field-rows diagnostic)
  (map (lambda (entry)
         (let ((name (car entry))
               (kind (cadr entry))
               (datum (field diagnostic (car entry))))
           (list (code name)
                 (cond
                  ((eq? kind 'type) (type-node datum))
                  ((eq? kind 'value) (value-node datum))
                  ((eq? kind 'form) (expression-node datum))
                  ((eq? kind 'count) (code (number->string datum)))
                  ((eq? kind 'scope) (text "the scope at this point"))
                  ((eq? kind 'text) (code datum))
                  (else (code datum))))))
       (diagnostic-schema (diagnostic-tag diagnostic))))

(define (default-explanation diagnostic)
  (section "Diagnostic"
           (paragraph (text "The checker reported ")
                      (code (symbol->string (diagnostic-tag diagnostic)))
                      (text "."))
           (where-paragraph diagnostic)
           (let ((rows (field-rows diagnostic)))
             (and (pair? rows)
                  (table (list "field" "value") rows)))))

(define (explain diagnostic)
  (let ((found (assq (diagnostic-tag diagnostic) (vector-ref %explainers 0))))
    (if found
        ((cdr found) diagnostic)
        (default-explanation diagnostic))))
