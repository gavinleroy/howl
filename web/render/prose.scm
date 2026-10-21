(define-module (render prose)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme cxr)
  #:use-module (lang types)
  #:use-module (lang world)
  #:use-module (render explain)
  #:use-module (lang reader)
  #:use-module (render base)
  #:export (prose-outline
            prose? prose-value prose-value-span prose-scope
            prose-expected prose-actual
            passage? passage-line passage-type passage-sentences
            sentence? sentence-parts sentence-site sentence-span
            sentence-scope sentence-type sentence-children
            part? part-kind part-text part-form part-type part-positive?))

;; The derivation tree the prose walks. This used to live in (render tree);
;; that module now exports the flat constraint graph instead, so the tree
;; construction is private here.

(define-record-type <node>
  (make-node node-kind node-sub node-sup node-site node-span node-origin
             node-children)
  node?
  (node-kind node-kind)
  (node-sub node-sub)
  (node-sup node-sup)
  (node-site node-site)
  (node-span node-span)
  (node-origin node-origin)
  (node-children node-children))

(define (tree-naming type)
  (if (and (tvar? type) (tvar-source type))
      (list (make-node "constant" #f type (tvar-source type)
                       (tvar-source-span type) #f '()))
      '()))

(define (edge-node bound type down? seen)
  (let ((origin (bound-origin bound))
        (other (bound-type bound)))
    (make-node "constraint"
               (if down? other type)
               (if down? type other)
               (and (origin? origin) (origin-expression origin))
               (and (origin? origin) (origin-span origin))
               origin
               (type-provenance other down? seen))))

(define (type-provenance type down? seen)
  (cond
   ((not type) '())
   ((seen? seen type) '())
   (else
    (remember! seen type)
    (cond
     ((tvar? type)
      (append (tree-naming type)
              (map (lambda (bound) (edge-node bound type down? seen))
                   (if down? (tvar-lower type) (tvar-upper type)))))
     ((func? type)
      (append (type-provenance-list (func-domains type) (not down?) seen)
              (type-provenance (func-codomain type) down? seen)))
     ((record-type? type)
      (type-provenance-list (map cdr (record-type-fields type)) down? seen))
     ((tuple-type? type)
      (type-provenance-list (tuple-type-items type) down? seen))
     ((variant-type? type)
      (type-provenance-list (apply append (map cdr (variant-type-cases type)))
                       down? seen))
     ((nominal? type) (type-provenance-list (nominal-args type) down? seen))
     (else '())))))

(define (type-provenance-list types down? seen)
  (apply append
         (map (lambda (type) (type-provenance type down? seen)) types)))

(define (side-provenance type context down?)
  (let ((seen (vector '())))
    (append (type-provenance type down? seen)
            (type-provenance context down? seen))))

(define (constraint-tree diagnostic)
  (let ((step (diagnostic-step diagnostic))
        (constraint (diagnostic-constraint diagnostic)))
    (and (step? step)
         (let ((origin (step-origin step)))
           (make-node "constraint" (step-sub step) (step-sup step)
                      (and (origin? origin) (origin-expression origin))
                      (and (origin? origin) (origin-span origin))
                      origin
                      (append (side-provenance (step-sub step)
                                               (and constraint (car constraint))
                                               #t)
                              (side-provenance (step-sup step)
                                               (and constraint (cdr constraint))
                                               #f)))))))

(define-record-type <part>
  (make-part part-kind part-text part-form part-type part-positive?)
  part?
  (part-kind part-kind)
  (part-text part-text)
  (part-form part-form)
  (part-type part-type)
  (part-positive? part-positive?))

(define-record-type <sentence>
  (make-sentence sentence-parts sentence-site sentence-span sentence-scope
                 sentence-type sentence-children)
  sentence?
  (sentence-parts sentence-parts)
  (sentence-site sentence-site)
  (sentence-span sentence-span)
  (sentence-scope sentence-scope)
  (sentence-type sentence-type)
  (sentence-children sentence-children))

(define-record-type <passage>
  (make-passage passage-line passage-type passage-sentences)
  passage?
  (passage-line passage-line)
  (passage-type passage-type)
  (passage-sentences passage-sentences))

(define-record-type <prose>
  (make-prose prose-value prose-value-span prose-scope prose-expected
              prose-actual)
  prose?
  (prose-value prose-value)
  (prose-value-span prose-value-span)
  (prose-scope prose-scope)
  (prose-expected prose-expected)
  (prose-actual prose-actual))

(define (words text) (make-part "text" text #f #f #f))

(define (quoting form) (make-part "code" (form->string form) form #f #f))

(define (naming label) (make-part "code" (symbol->string label) #f #f #f))

(define (showing type) (make-part "type" #f #f type #t))

(define (demanding type) (make-part "type" #f #f type #f))

(define (sole-field type)
  (and (record-type? type)
       (pair? (record-type-fields type))
       (car (car (record-type-fields type)))))

(define (ordinal index)
  (cond ((eq? index 1) "1st") ((eq? index 2) "2nd") ((eq? index 3) "3rd")
        (else (string-append (number->string index) "th"))))

(define (within context expression)
  (within-word " of " context expression))

(define (within-word connector context expression)
  (if (and context (not (equal? context expression)))
      (list (words connector) (quoting context))
      '()))

(define (signature-name form)
  (and (pair? form) (proper-list? form) (= (length form) 3)
       (eq? (car form) 'val) (symbol? (cadr form))
       (cadr form)))

(define (signature-subject expression)
  (let ((name (signature-name expression)))
    (if name (naming name) (quoting expression))))

(define (branch-word extra)
  (cond ((eq? extra 'then) "then") ((eq? extra 'else) "else") (else "")))

(define (claim connector type) (claim-at connector type #t))

(define (demand connector type) (claim-at connector type #f))

(define (claim-at connector type positive?)
  (if (tvar? type)
      '()
      (list (words connector)
            (if positive? (showing type) (demanding type)))))

(define (list-item items index)
  (cond ((null? items) #f)
        ((= index 1) (car items))
        (else (list-item (cdr items) (- index 1)))))

(define (tuple-part form index)
  (and (pair? form) (proper-list? form) (eq? (car form) 'tuple)
       (let ((item (list-item (cdr form) index)))
         (and item (cons item (nth-span form index))))))

(define (record-part form label)
  (and (pair? form) (proper-list? form) (eq? (car form) 'record)
       (let loop ((fields (cdr form)))
         (cond
          ((null? fields) #f)
          ((and (pair? (car fields)) (proper-list? (car fields))
                (= (length (car fields)) 2)
                (eq? (car (car fields)) label))
           (cons (cadr (car fields)) (nth-span (car fields) 1)))
          (else (loop (cdr fields)))))))

(define (step-into located segment reason)
  (let ((kind (car segment))
        (form (car located)))
    (cond
     ((eq? kind 'domain)
      (and (reason-extra reason 2)
           (cons (reason-extra reason 2) (reason-extra reason 3))))
     ((eq? kind 'item) (tuple-part form (cadr segment)))
     ((eq? kind 'field) (record-part form (cadr segment)))
     (else #f))))

(define (resolve-path expression span path reason)
  (let loop ((located (cons expression span)) (segments path))
    (cond
     ((not located) #f)
     ((null? segments) located)
     (else (loop (step-into located (car segments) reason) (cdr segments))))))

(define (subject reason expression path)
  (if (null? path)
      (list (quoting expression))
      (let* ((reversed (reverse path))
             (segment (car reversed))
             (outer (reverse (cdr reversed))))
        (if (and (null? outer)
                 (eq? (car segment) 'domain)
                 (reason-extra reason 2))
            (list (quoting (reason-extra reason 2)))
            (append (segment-parts segment reason)
                    (list (words " of "))
                    (subject reason expression outer))))))

(define (segment-parts segment reason)
  (let ((kind (car segment)))
    (cond
     ((eq? kind 'domain)
      (let ((index (reason-extra reason 0)))
        (if (and (eq? (reason-tag reason) 'argument) index)
            (list (words "the ") (words (ordinal index)) (words " argument"))
            (list (words "the argument")))))
     ((eq? kind 'result) (list (words "the result")))
     ((eq? kind 'field)
      (list (words "the ") (naming (cadr segment)) (words " field")))
     ((eq? kind 'item)
      (list (words "the ") (words (ordinal (cadr segment))) (words " part")))
     ((eq? kind 'case)
      (list (words "the payload of ") (naming (cadr segment))))
     ((eq? kind 'argument-of)
      (list (words "the ") (naming (cadr segment)) (words " argument")))
     (else (list (words "part"))))))

(define (demand-parts reason expression type context)
  (let ((tag (reason-tag reason))
        (here (quoting expression))
        (field-label (sole-field type)))
    (cond
     ((and (eq? tag 'field) field-label)
      (list here (words " reads the field ") (naming field-label)
            (words " here, so it must be a record with that field")))
     ((eq? tag 'argument)
      (append (list (quoting (or (reason-extra reason 2) expression))
                    (words " is the ")
                    (words (ordinal (or (reason-extra reason 0) 1)))
                    (words " argument of ") (quoting expression))
              (demand ", so the function it is applied to must be " type)))
     ((eq? tag 'condition)
      (append (list here (words " is the condition"))
              (within context expression)
              (demand ", so it must be " type)))
     ((eq? tag 'boolean-op)
      (append (list here (words " is used in a boolean operator"))
              (within context expression)
              (demand ", so it must be " type)))
     ((eq? tag 'scrutinee)
      (append (list here (words " is matched"))
              (within-word " by " context expression)
              (demand ", so it must be one of the cases in " type)))
     ((eq? tag 'branch)
      (append (list here (words " is the ")
                    (words (branch-word (reason-extra reason 0)))
                    (words " branch"))
              (within context expression)
              (list (words ", and every branch must agree"))
              (demand ", so it must be " type)))
     ((eq? tag 'clause)
      (append (list here (words " is the "))
              (if (reason-extra reason 0)
                  (list (quoting (reason-extra reason 0)) (words " branch"))
                  (list (words "branch")))
              (within context expression)
              (list (words ", and every branch must agree"))
              (demand ", so it must be " type)))
     ((eq? tag 'pattern)
      (append (list (words "the pattern ") here)
              (demand " requires " type)))
     ((eq? tag 'signature)
      (append (list (words "the signature requires ")
                    (signature-subject expression))
              (demand " to be " type)))
     ((eq? tag 'recursive)
      (list here (words " is defined recursively, so its own calls share ")
            (words "these types")))
     ((eq? tag 'equivalent)
      (append (list (words "an abstract type at ") here)
              (demand " forces it to be exactly " type)))
     (else (append (list here) (demand " must be " type))))))

(define (source-parts reason expression type context)
  (let ((tag (reason-tag reason))
        (here (quoting expression)))
    (cond
     ((eq? tag 'argument)
      (append (list (quoting (or (reason-extra reason 2) expression))
                    (words " is the ")
                    (words (ordinal (or (reason-extra reason 0) 1)))
                    (words " argument of ") (quoting expression))
              (claim ", and the function it is applied to has type " type)))
     ((eq? tag 'branch)
      (append (list here (words " is the ")
                    (words (branch-word (reason-extra reason 0)))
                    (words " branch"))
              (within context expression)
              (claim ", and it has type " type)))
     ((eq? tag 'clause)
      (append (list here (words " is the "))
              (if (reason-extra reason 0)
                  (list (quoting (reason-extra reason 0)) (words " branch"))
                  (list (words "branch")))
              (within context expression)
              (claim ", and it has type " type)))
     ((eq? tag 'field)
      (append (list (words "reading ") here) (claim " gives " type)))
     ((eq? tag 'recursive)
      (list here (words " is defined recursively, so its own calls share ")
            (words "these types")))
     ((eq? tag 'signature)
      (append (list (words "the signature gives ")
                    (signature-subject expression))
              (claim " type " type)))
     ((eq? tag 'pattern)
      (append (list (words "the pattern ") here) (claim " gives it " type)))
     ((or (eq? tag 'condition) (eq? tag 'boolean-op))
      (append (list here (words " is used as a condition"))
              (within context expression)
              (claim ", so it is " type)))
     ((eq? tag 'scrutinee)
      (append (list here (words " is matched"))
              (within-word " by " context expression)
              (claim " as " type)))
     ((eq? tag 'equivalent)
      (append (list (words "an abstract type at ") here)
              (claim " makes it exactly " type)))
     (else (append (list here) (claim " gives it type " type))))))

(define (site-for reason expression span path type located)
  (cond
   ((and located (not (tvar? type))) located)
   ((and (pair? path) (not (tvar? type))) (cons expression span))
   ((and (eq? (reason-tag reason) 'argument) (reason-extra reason 2))
    (cons (reason-extra reason 2) (reason-extra reason 3)))
   (else (cons expression span))))

(define (say reason side expression type context path direct)
  (cond
   ((and direct (eq? (reason-tag reason) 'argument) (equal? path '((domain))))
    (append (list (quoting direct) (words " is the ")
                  (words (ordinal (or (reason-extra reason 0) 1)))
                  (words " argument of ") (quoting expression))
            (if (eq? side 'expected)
                (demand ", so it must be " type)
                (claim ", and it has type " type))))
   ((and direct (not (tvar? type)))
    (append (list (quoting direct))
            (if (eq? side 'expected)
                (list (words " must be ") (demanding type))
                (list (words " has type ") (showing type)))))
   ((and (pair? path) (not (tvar? type)))
    (append (subject reason expression path)
            (if (eq? side 'expected)
                (list (words " must be ") (demanding type))
                (list (words " has type ") (showing type)))))
   ((eq? side 'expected) (demand-parts reason expression type context))
   (else (source-parts reason expression type context))))

(define (keep-if ok? items)
  (cond
   ((null? items) '())
   ((ok? (car items)) (cons (car items) (keep-if ok? (cdr items))))
   (else (keep-if ok? (cdr items)))))

(define (demanding-reason? reason)
  (memq (reason-tag reason)
        '(argument field condition boolean-op scrutinee pattern signature
          equivalent)))

(define (node-side node)
  (let ((sub (node-sub node))
        (sup (node-sup node))
        (origin (node-origin node)))
    (cond
     ((and (concrete? sub) (not (concrete? sup))) 'actual)
     ((and (concrete? sup) (not (concrete? sub))) 'expected)
     ((and (origin? origin) (demanding-reason? (origin-reason origin)))
      'expected)
     (else 'actual))))

(define (constraint-parts node side)
  (if (eq? side 'actual)
      (list (showing (node-sub node)) (words " flows into ")
            (showing (node-sup node)))
      (list (showing (node-sub node)) (words " must be ")
            (demanding (node-sup node)))))

(define (constant-parts node)
  (list (quoting (node-site node)) (words " is ") (demanding (node-sup node))))

(define (repeat-parts node)
  (list (showing (node-sub node)) (words " flowing into ")
        (showing (node-sup node))
        (words " is the same constraint as above")))

(define (node-sentence node)
  (let* ((origin (node-origin node))
         (children (map node-sentence (node-children node))))
    (cond
     ((string=? (node-kind node) "constant")
      (make-sentence (constant-parts node) (node-site node) (node-span node)
                     #f (node-sup node) children))
     ((string=? (node-kind node) "repeat")
      (make-sentence (repeat-parts node) #f #f #f (node-sup node) children))
     ((not (origin? origin))
      (make-sentence (constraint-parts node (node-side node))
                     (node-site node) (node-span node) #f
                     (node-sup node) children))
     (else
      (let* ((side (node-side node))
             (type (if (eq? side 'actual) (node-sub node) (node-sup node)))
             (expression (origin-expression origin))
             (reason (origin-reason origin))
             (context (origin-context origin))
             (span (origin-span origin))
             (path (origin-path origin))
             (located (and (pair? path) (not (tvar? type))
                           (resolve-path expression span path reason)))
             (direct (and located (car located)))
             (parts (if (and expression reason)
                        (say reason side expression type context path direct)
                        (constraint-parts node side)))
             (site (if (and expression reason)
                       (site-for reason expression span path type located)
                       (cons (node-site node) (node-span node)))))
        (make-sentence parts (car site) (cdr site)
                       (if direct expression context)
                       type children))))))

(define (failing-value diagnostic actual)
  (let* ((expression (diagnostic-expr diagnostic))
         (span (diagnostic-span diagnostic))
         (path (diagnostic-path diagnostic))
         (reason (diagnostic-reason diagnostic))
         (located (and (pair? path) (not (tvar? actual))
                       (resolve-path expression span path reason))))
    (cond
     ((field diagnostic "name") (cons (field diagnostic "name") #f))
     (located located)
     (expression (cons expression span))
     ((world? (diagnostic-world diagnostic))
      (cons (world-form (diagnostic-world diagnostic)) #f))
     (else (cons #f #f)))))

(define (value-sentence value actual)
  (make-sentence (list (quoting (car value)) (words " has type ")
                       (showing actual))
                 (car value) (cdr value) #f actual '()))

(define (demand-sentence diagnostic expected)
  (let ((expression (diagnostic-expr diagnostic))
        (reason (diagnostic-reason diagnostic))
        (span (diagnostic-span diagnostic)))
    (if (and expression reason (demanding-reason? reason))
        (let ((site (site-for reason expression span '() expected #f)))
          (list (make-sentence (demand-parts reason expression expected
                                             (diagnostic-context diagnostic))
                               (car site) (cdr site)
                               (diagnostic-context diagnostic) expected '())))
        '())))

(define (prose-outline diagnostic)
  (let ((root (constraint-tree diagnostic))
        (actual (field diagnostic "actual"))
        (expected (field diagnostic "expected")))
    (and (node? root) actual expected
         (let ((value (failing-value diagnostic actual)))
           (and (car value)
                (make-prose
                 (car value) (cdr value) #f
                 (make-passage (list (words "The value ") (quoting (car value))
                                     (words " was expected to have type ")
                                     (demanding expected))
                               expected
                               (append
                                (demand-sentence diagnostic expected)
                                (map node-sentence
                                     (side-provenance
                                      expected
                                      (let ((c (diagnostic-constraint diagnostic)))
                                        (and c (cdr c)))
                                      #f))))
                 (make-passage (list (words "but it has type ")
                                     (showing actual))
                               actual
                               (cons (value-sentence value actual)
                                     (map node-sentence
                                          (side-provenance
                                           actual
                                           (let ((c (diagnostic-constraint
                                                     diagnostic)))
                                             (and c (car c)))
                                           #t))))))))))
