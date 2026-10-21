(define-module (lang types)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme cxr)
  #:use-module (scheme write)
  #:use-module (lang world)
  #:export (tvar?
            tvar-id
            tvar-level
            tvar-lower
            tvar-upper
            tvar-source
            tvar-source-span
            set-tvar-lower!
            set-tvar-upper!
            make-bound
            bound?
            bound-type
            bound-step
            bound-origin
            step?
            step-sub
            step-sup
            step-origin
            step-causes
            bound-types
            make-origin
            origin?
            origin-expression
            origin-left
            origin-right
            origin-reason
            origin-context
            origin-path
            origin-span
            fresh-tvar
            fresh-tvar-like
            fresh-tvar-at
            fresh-type-name
            prim?
            prim-name
            int-type
            float-type
            bool-type
            string-type
            make-func
            func?
            func-domains
            func-codomain
            funcs
            make-record-type
            record-type?
            record-type-fields
            make-tuple-type
            tuple-type?
            tuple-type-items
            make-variant-type
            variant-type?
            variant-type-cases
            make-nominal
            make-applied
            make-abstract
            nominal?
            nominal-name
            nominal-args
            nominal-builder
            nominal-body
            set-nominal-body!
            type-level
            constrain
            make-poly
            poly?
            poly-level
            poly-body
            instantiate
            generalize
            form->string
            string-join
            proper-list?
            find-first
            any-of
            display-name))

(define %counter (vector 0))

(define (next-id!)
  (let ((n (+ (vector-ref %counter 0) 1)))
    (vector-set! %counter 0 n)
    n))

(define (fresh-type-name base)
  (string->symbol (string-append base "/" (number->string (next-id!)))))

(define (display-name name)
  (let* ((text (symbol->string name))
         (size (string-length text)))
    (let loop ((index (- size 1)))
      (cond ((< index 0) text)
            ((char=? (string-ref text index) #\/) (substring text 0 index))
            (else (loop (- index 1)))))))

(define-record-type <tvar>
  (%make-tvar tvar-id tvar-level tvar-lower tvar-upper tvar-source
              tvar-source-span)
  tvar?
  (tvar-id tvar-id)
  (tvar-level tvar-level)
  (tvar-lower tvar-lower set-tvar-lower!)
  (tvar-upper tvar-upper set-tvar-upper!)
  (tvar-source tvar-source)
  (tvar-source-span tvar-source-span))

(define (fresh-tvar level)
  (%make-tvar (next-id!) level '() '() (current-expression) (current-span)))

(define (fresh-tvar-at level source span)
  (%make-tvar (next-id!) level '() '() source span))

(define (fresh-tvar-like level original)
  (%make-tvar (next-id!) level '() '()
              (tvar-source original) (tvar-source-span original)))

(define-record-type <origin>
  (make-origin origin-expression origin-left origin-right origin-reason
               origin-context origin-path origin-span)
  origin?
  (origin-expression origin-expression)
  (origin-left origin-left)
  (origin-right origin-right)
  (origin-reason origin-reason)
  (origin-context origin-context)
  (origin-path origin-path)
  (origin-span origin-span))

(define-record-type <step>
  (make-step step-sub step-sup step-origin step-causes)
  step?
  (step-sub step-sub)
  (step-sup step-sup)
  (step-origin step-origin)
  (step-causes step-causes))

(define-record-type <bound>
  (make-bound bound-type bound-step)
  bound?
  (bound-type bound-type)
  (bound-step bound-step))

(define (bound-origin bound)
  (let ((step (bound-step bound)))
    (and (step? step) (step-origin step))))

(define (bound-types bounds) (map bound-type bounds))

(define (origin-here)
  (let ((root (current-constraint)))
    (make-origin (current-expression)
                 (and root (car root))
                 (and root (cdr root))
                 (current-reason)
                 (current-context)
                 (current-path)
                 (current-span))))

(define-record-type <prim>
  (make-prim prim-name)
  prim?
  (prim-name prim-name))

(define int-type (make-prim 'int))
(define float-type (make-prim 'float))
(define bool-type (make-prim 'bool))
(define string-type (make-prim 'string))

(define-record-type <func>
  (make-func func-domains func-codomain)
  func?
  (func-domains func-domains)
  (func-codomain func-codomain))

(define (funcs domains codomain)
  (make-func domains codomain))

(define-record-type <record-type>
  (make-record-type record-type-fields)
  record-type?
  (record-type-fields record-type-fields))

(define-record-type <tuple-type>
  (make-tuple-type tuple-type-items)
  tuple-type?
  (tuple-type-items tuple-type-items))

(define-record-type <variant-type>
  (make-variant-type variant-type-cases)
  variant-type?
  (variant-type-cases variant-type-cases))

(define-record-type <nominal>
  (%make-nominal nominal-name nominal-args nominal-builder nominal-body)
  nominal?
  (nominal-name nominal-name)
  (nominal-args nominal-args)
  (nominal-builder nominal-builder)
  (nominal-body nominal-body set-nominal-body!))

(define (make-nominal name)
  (%make-nominal name '() #f #f))

(define (make-applied name args builder)
  (%make-nominal name args builder #f))

(define (make-abstract name)
  (%make-nominal (fresh-type-name name) '() #f #f))

(define-record-type <poly>
  (make-poly poly-level poly-body)
  poly?
  (poly-level poly-level)
  (poly-body poly-body))

(define (proper-list? x)
  (cond ((null? x) #t)
        ((pair? x) (proper-list? (cdr x)))
        (else #f)))

(define (find-first pred lst)
  (cond ((null? lst) #f)
        ((pred (car lst)) (car lst))
        (else (find-first pred (cdr lst)))))

(define (any-of pred lst)
  (and (find-first pred lst) #t))

(define (form->string form)
  (let ((port (open-output-string)))
    (write form port)
    (get-output-string port)))

(define (string-join strings separator)
  (if (null? strings)
      ""
      (let loop ((text (car strings)) (rest (cdr strings)))
        (if (null? rest)
            text
            (loop (string-append text separator (car rest)) (cdr rest))))))

(define (maximum numbers)
  (let loop ((numbers numbers) (best 0))
    (if (null? numbers)
        best
        (loop (cdr numbers) (if (> (car numbers) best) (car numbers) best)))))

(define (type-level type)
  (cond
   ((tvar? type) (tvar-level type))
   ((func? type) (max (maximum (map type-level (func-domains type)))
                      (type-level (func-codomain type))))
   ((record-type? type)
    (maximum (map (lambda (field) (type-level (cdr field)))
                  (record-type-fields type))))
   ((tuple-type? type) (maximum (map type-level (tuple-type-items type))))
   ((variant-type? type)
    (maximum (map (lambda (variant)
                    (maximum (map type-level (cdr variant))))
                  (variant-type-cases type))))
   ((nominal? type) (maximum (map type-level (nominal-args type))))
   (else 0)))

(define (make-cache) (vector '()))

(define (cached? cache left right)
  (any-of (lambda (entry) (and (eq? (car entry) left) (eq? (cdr entry) right)))
          (vector-ref cache 0)))

(define (remember! cache left right)
  (vector-set! cache 0 (cons (cons left right) (vector-ref cache 0))))

(define (deeper sub sup cache causes)
  (set-current-depth! (+ 1 (current-depth)))
  (descend sub sup cache causes)
  (set-current-depth! (- (current-depth) 1)))

(define (deeper-at segment sub sup cache causes)
  (let ((was (current-path)))
    (set-current-path! (append was (list segment)))
    (deeper sub sup cache causes)
    (set-current-path! was)))

(define (mismatch sub sup)
  (fail 'type-mismatch sub sup))

;; Two types may share a join (meet in a variable's lower bounds) only when
;; one is still a variable or both have the same shape: same primitive,
;; variants (which merge case-wise), records, functions, equal-width tuples,
;; or the same nominal type. Anything else is a branch disagreement.
;; A nominal type joins like the shape it names; abstract nominals (no
;; visible body) keep their identity.
(define (join-shape type)
  (if (and (nominal? type) (nominal-body type))
      (join-shape (nominal-body type))
      type))

(define (all-pairs? ok? as bs)
  (cond ((and (null? as) (null? bs)) #t)
        ((or (null? as) (null? bs)) #f)
        ((ok? (car as) (car bs)) (all-pairs? ok? (cdr as) (cdr bs)))
        (else #f)))

;; Deep shape compatibility for a join (meet? = #f, two values flowing into
;; the same place) or a meet (meet? = #t, two demands on the same place).
;; Variables are compatible with anything. Function domains flip mode
;; (contravariance). Variants merge freely in a join, but two variant
;; demands are only satisfiable when they share a case: nothing is both a
;; Cons and a Nil. The cache assumes compatibility on cycles so recursive
;; types terminate.
(define (compatible? left right meet? cache)
  (let ((a (join-shape left))
        (b (join-shape right)))
    (cond
     ((eq? a b) #t)
     ((cached? cache a b) #t)
     (else
      (remember! cache a b)
      (cond
       ;; In a meet, a variable stands for whatever satisfies its own
       ;; demands, so compare through its upper bounds (vacuously fine
       ;; when unconstrained). In a join, variables stay lazy — the
       ;; generalization check owns that side.
       ((tvar? a)
        (or (not meet?)
            (let loop ((uppers (bound-types (tvar-upper a))))
              (cond ((null? uppers) #t)
                    ((compatible? (car uppers) b meet? cache)
                     (loop (cdr uppers)))
                    (else #f)))))
       ((tvar? b)
        (or (not meet?)
            (let loop ((uppers (bound-types (tvar-upper b))))
              (cond ((null? uppers) #t)
                    ((compatible? a (car uppers) meet? cache)
                     (loop (cdr uppers)))
                    (else #f)))))
       ((and (prim? a) (prim? b)) (eq? (prim-name a) (prim-name b)))
       ((and (func? a) (func? b))
        (and (= (length (func-domains a)) (length (func-domains b)))
             (all-pairs? (lambda (x y) (compatible? x y (not meet?) cache))
                         (func-domains a) (func-domains b))
             (compatible? (func-codomain a) (func-codomain b) meet? cache)))
       ((and (record-type? a) (record-type? b))
        (let loop ((fields (record-type-fields a)))
          (cond
           ((null? fields) #t)
           ((let ((found (assq (caar fields) (record-type-fields b))))
              (or (not found)
                  (compatible? (cdar fields) (cdr found) meet? cache)))
            (loop (cdr fields)))
           (else #f))))
       ((and (tuple-type? a) (tuple-type? b))
        (and (= (length (tuple-type-items a)) (length (tuple-type-items b)))
             (all-pairs? (lambda (x y) (compatible? x y meet? cache))
                         (tuple-type-items a) (tuple-type-items b))))
       ((and (variant-type? a) (variant-type? b))
        (if meet?
            (any-of (lambda (case)
                      (let ((found (assq (car case)
                                         (variant-type-cases b))))
                        (and found
                             (= (length (cdr case)) (length (cdr found)))
                             (all-pairs? (lambda (x y)
                                           (compatible? x y meet? cache))
                                         (cdr case) (cdr found)))))
                    (variant-type-cases a))
            #t))
       ((and (nominal? a) (nominal? b)) (eq? (nominal-name a) (nominal-name b)))
       (else #f))))))

(define (join-compatible? left right)
  (compatible? left right #f (make-cache)))

(define (meet-compatible? left right)
  (compatible? left right #t (make-cache)))

(define (constrain sub sup)
  (let ((previous (current-constraint))
        (was-step (current-step)))
    (set-current-constraint! (cons sub sup))
    (set-current-depth! 0)
    (set-current-path! '())
    (let ((step (make-step sub sup (origin-here) '())))
      (set-current-step! step)
      (constrain-with sub sup (make-cache) step))
    (set-current-constraint! previous)
    (set-current-step! was-step)))

(define (descend sub sup cache causes)
  (let ((step (make-step sub sup (origin-here) causes))
        (was (current-step)))
    (set-current-step! step)
    (constrain-with sub sup cache step)
    (set-current-step! was)))

(define (constrain-with sub sup cache step)
  (cond
   ((eq? sub sup) #t)
   ((cached? cache sub sup) #t)
   (else
    (remember! cache sub sup)
    (cond
     ((and (prim? sub) (prim? sup))
      (if (eq? (prim-name sub) (prim-name sup)) #t (mismatch sub sup)))
     ((and (func? sub) (func? sup))
      (if (not (= (length (func-domains sub)) (length (func-domains sup))))
          (fail 'type-mismatch sub sup)
          (begin
            (let loop ((above (func-domains sup))
                       (below (func-domains sub))
                       (index 1))
              (if (pair? above)
                  (begin
                    (deeper-at (list 'domain index) (car above) (car below)
                               cache (list step))
                    (loop (cdr above) (cdr below) (+ index 1)))))
            (deeper-at '(result) (func-codomain sub) (func-codomain sup)
                       cache (list step)))))
     ((and (record-type? sub) (record-type? sup))
      (for-each (lambda (field)
                  (let ((found (assq (car field) (record-type-fields sub))))
                    (if found
                        (deeper-at (list 'field (car field))
                                   (cdr found) (cdr field) cache
                                   (list step))
                        (fail 'type-mismatch sub sup))))
                (record-type-fields sup)))
     ((and (tuple-type? sub) (tuple-type? sup))
      (if (not (= (length (tuple-type-items sub)) (length (tuple-type-items sup))))
          (fail 'type-mismatch sub sup)
          (let loop ((from (tuple-type-items sub))
                     (to (tuple-type-items sup))
                     (index 1))
            (if (pair? from)
                (begin
                  (deeper-at (list 'item index) (car from) (car to) cache
                             (list step))
                  (loop (cdr from) (cdr to) (+ index 1)))))))
     ((and (variant-type? sub) (variant-type? sup))
      (for-each (lambda (variant)
                  (let ((found (assq (car variant) (variant-type-cases sup))))
                    (cond
                     ((not found) (fail 'type-mismatch sub sup))
                     ((not (= (length (cdr found)) (length (cdr variant))))
                      (fail 'type-mismatch sub sup))
                     (else
                      (for-each (lambda (from to)
                                  (deeper-at (list 'case (car variant))
                                             from to cache (list step)))
                                (cdr variant) (cdr found))))))
                (variant-type-cases sub)))
     ((tvar? sub)
      (cond
       ((any-of (lambda (bound) (eq? (bound-type bound) sup)) (tvar-upper sub))
        #t)
       ((<= (type-level sup) (tvar-level sub))
        (for-each (lambda (bound)
                    (unless (meet-compatible? sup (bound-type bound))
                      (fail 'unsatisfiable-demand sup (bound-type bound))))
                  (tvar-upper sub))
        (set-tvar-upper! sub (cons (make-bound sup step) (tvar-upper sub)))
        (for-each (lambda (bound)
                    (deeper (bound-type bound) sup cache
                            (list step (bound-step bound))))
                  (tvar-lower sub)))
       (else
        (deeper sub (extrude sup #f (tvar-level sub) (make-cache)) cache
                (list step)))))
     ((tvar? sup)
      (cond
       ((any-of (lambda (bound) (eq? (bound-type bound) sub)) (tvar-lower sup))
        #t)
       ((<= (type-level sub) (tvar-level sup))
        (for-each (lambda (bound)
                    (unless (join-compatible? sub (bound-type bound))
                      (mismatch sub (bound-type bound))))
                  (tvar-lower sup))
        (set-tvar-lower! sup (cons (make-bound sub step) (tvar-lower sup)))
        (for-each (lambda (bound)
                    (deeper sub (bound-type bound) cache
                            (list step (bound-step bound))))
                  (tvar-upper sup)))
       (else
        (deeper (extrude sub #t (tvar-level sup) (make-cache)) sup cache
                (list step)))))
     ((and (nominal? sub) (nominal? sup)
           (not (nominal-body sub)) (not (nominal-body sup))
           (eq? (nominal-name sub) (nominal-name sup))
           (= (length (nominal-args sub)) (length (nominal-args sup))))
      (for-each (lambda (left right)
                  (deeper-at (list 'argument-of (nominal-name sub))
                             left right cache (list step))
                  (deeper-at (list 'argument-of (nominal-name sub))
                             right left cache (list step)))
                (nominal-args sub) (nominal-args sup)))
     ((and (nominal? sub) (nominal-body sub))
      (deeper (nominal-body sub) sup cache (list step)))
     ((and (nominal? sup) (nominal-body sup))
      (deeper sub (nominal-body sup) cache (list step)))
     (else (mismatch sub sup))))))

(define (extrusion-step sub sup)
  (make-step sub sup (origin-here)
             (if (current-step) (list (current-step)) '())))

(define (extrude type polarity level cache)
  (if (<= (type-level type) level)
      type
      (cond
       ((func? type)
        (make-func (map (lambda (domain)
                          (extrude domain (not polarity) level cache))
                        (func-domains type))
                   (extrude (func-codomain type) polarity level cache)))
       ((record-type? type)
        (make-record-type
         (map (lambda (field)
                (cons (car field) (extrude (cdr field) polarity level cache)))
              (record-type-fields type))))
       ((tuple-type? type)
        (make-tuple-type
         (map (lambda (item) (extrude item polarity level cache))
              (tuple-type-items type))))
       ((variant-type? type)
        (make-variant-type
         (map (lambda (variant)
                (cons (car variant)
                      (map (lambda (payload)
                             (extrude payload polarity level cache))
                           (cdr variant))))
              (variant-type-cases type))))
       ((and (nominal? type) (nominal-body type))
        (extrude (nominal-body type) polarity level cache))
       ((nominal? type)
        (if (null? (nominal-args type))
            type
            ((nominal-builder type)
             (map (lambda (arg) (extrude arg polarity level cache))
                  (nominal-args type)))))
       ((tvar? type)
        (let ((known (assq type (vector-ref cache 0))))
          (if known
              (cdr known)
              (let ((copy (fresh-tvar-like level type)))
                (vector-set! cache 0 (cons (cons type copy) (vector-ref cache 0)))
                (if polarity
                    (begin
                      (set-tvar-upper! type
                                       (cons (make-bound copy (extrusion-step
                                                               type copy))
                                             (tvar-upper type)))
                      (set-tvar-lower! copy (extrude-bounds (tvar-lower type)
                                                            polarity level cache
                                                            copy #t)))
                    (begin
                      (set-tvar-lower! type
                                       (cons (make-bound copy (extrusion-step
                                                               copy type))
                                             (tvar-lower type)))
                      (set-tvar-upper! copy (extrude-bounds (tvar-upper type)
                                                            polarity level cache
                                                            copy #f))))
                copy))))
       (else type))))

(define (extrude-bounds bounds polarity level cache owner lower?)
  (map (lambda (bound)
         (let ((copy (extrude (bound-type bound) polarity level cache)))
           (make-bound copy (copied-step copy owner lower? (bound-step bound)))))
       bounds))

(define (freshen-bounds bounds limit level cache owner lower?)
  (map (lambda (bound)
         (let ((copy (freshen (bound-type bound) limit level cache)))
           (make-bound copy (copied-step copy owner lower? (bound-step bound)))))
       bounds))

(define (copied-step copy owner lower? original)
  (make-step (if lower? copy owner)
             (if lower? owner copy)
             (if (step? original) (step-origin original) (origin-here))
             (if (step? original) (list original) '())))

(define (freshen type limit level cache)
  (if (<= (type-level type) limit)
      type
      (cond
       ((tvar? type)
        (let ((known (assq type (vector-ref cache 0))))
          (if known
              (cdr known)
              (let ((copy (fresh-tvar-like level type)))
                (vector-set! cache 0 (cons (cons type copy) (vector-ref cache 0)))
                (set-tvar-lower! copy (freshen-bounds (tvar-lower type)
                                                      limit level cache
                                                      copy #t))
                (set-tvar-upper! copy (freshen-bounds (tvar-upper type)
                                                      limit level cache
                                                      copy #f))
                copy))))
       ((func? type)
        (make-func (map (lambda (domain) (freshen domain limit level cache))
                        (func-domains type))
                   (freshen (func-codomain type) limit level cache)))
       ((record-type? type)
        (make-record-type
         (map (lambda (field)
                (cons (car field) (freshen (cdr field) limit level cache)))
              (record-type-fields type))))
       ((tuple-type? type)
        (make-tuple-type
         (map (lambda (item) (freshen item limit level cache))
              (tuple-type-items type))))
       ((variant-type? type)
        (make-variant-type
         (map (lambda (variant)
                (cons (car variant)
                      (map (lambda (payload)
                             (freshen payload limit level cache))
                           (cdr variant))))
              (variant-type-cases type))))
       ((and (nominal? type) (pair? (nominal-args type)))
        ((nominal-builder type)
         (map (lambda (arg) (freshen arg limit level cache))
              (nominal-args type))))
       (else type))))

(define (instantiate entry level)
  (if (poly? entry)
      (freshen (poly-body entry) (poly-level entry) level (make-cache))
      entry))

(define (union-seen? table type)
  (let loop ((items (vector-ref table 0)))
    (cond ((null? items) #f)
          ((eq? (car items) type) #t)
          (else (loop (cdr items))))))

(define (union-remember! table type)
  (vector-set! table 0 (cons type (vector-ref table 0))))

;; A variable joined with a concrete type is only acceptable when its own
;; demands already force it into that type: some (transitive) upper bound
;; has the concrete type's shape.
(define (guaranteed? variable concrete seen)
  (any-of (lambda (bound)
            (let ((upper (bound-type bound)))
              (cond
               ((tvar? upper)
                (and (not (union-seen? seen upper))
                     (begin (union-remember! seen upper)
                            (guaranteed? upper concrete seen))))
               (else (join-compatible? upper concrete)))))
          (tvar-upper variable)))

;; The variables the printed type actually shows, tagged by polarity: a
;; positive variable displays its lower bounds as a union, a negative one
;; (a caller-chosen input) its upper bounds as an intersection. Mirrors the
;; printer's walk. Returns (positive-table . negative-table).
(define (polar-variables type)
  (let ((positive (vector '()))
        (negative (vector '()))
        (seen-pos (vector '()))
        (seen-neg (vector '())))
    (let walk ((type type) (positive? #t))
      (cond
       ((not type) #t)
       ((union-seen? (if positive? seen-pos seen-neg) type) #t)
       (else
        (union-remember! (if positive? seen-pos seen-neg) type)
        (cond
         ((tvar? type)
          ;; Collect the variable but do not descend into its bounds:
          ;; only variables in structural positions of the type itself
          ;; participate, not the solver's internal flow variables.
          (let ((table (if positive? positive negative)))
            (unless (union-seen? table type)
              (union-remember! table type))))
         ((func? type)
          (for-each (lambda (domain) (walk domain (not positive?)))
                    (func-domains type))
          (walk (func-codomain type) positive?))
         ((record-type? type)
          (for-each (lambda (field) (walk (cdr field) positive?))
                    (record-type-fields type)))
         ((tuple-type? type)
          (for-each (lambda (item) (walk item positive?))
                    (tuple-type-items type)))
         ((variant-type? type)
          (for-each (lambda (case)
                      (for-each (lambda (payload) (walk payload positive?))
                                (cdr case)))
                    (variant-type-cases type)))
         ((nominal? type)
          (for-each (lambda (arg) (walk arg positive?))
                    (nominal-args type)))
         (else #t)))))
    (cons positive negative)))

;; Generalization-time check. A variable x flowing into a variable u that
;; the type displays positively and that also has a concrete lower bound C
;; is the union (∪ 'x C): it can only ever be instantiated at C, so unless
;; x's own demands already force it into C, refuse to generalize. Unions
;; internal to the solving (never displayed) are left alone, and variants
;; are exempt: joining a variable with constructor cases is the
;; open-variant idiom, still eliminable by match.
;; A definition's generalized type may just be the recursion variable;
;; the displayed type is what flowed into it. Expand such roots through
;; their lower bounds before walking the skeleton.
(define (skeleton-roots type seen)
  (cond
   ((not type) '())
   ((not (tvar? type)) (list type))
   ((union-seen? seen type) '())
   (else
    (union-remember! seen type)
    (let loop ((lowers (bound-types (tvar-lower type))) (found '()))
      (if (null? lowers)
          found
          (loop (cdr lowers)
                (append found (skeleton-roots (car lowers) seen))))))))

(define (check-unions type)
  (for-each check-union-root
            (if (tvar? type)
                (skeleton-roots type (vector '()))
                (list type))))

;; All concrete demands pressing on a variable, following flows through
;; intermediate variables: x <: y and y <: (-> ...) demands the function
;; shape of x too.
(define (concrete-demands variable seen found)
  (if (union-seen? seen variable)
      found
      (begin
        (union-remember! seen variable)
        (let loop ((uppers (bound-types (tvar-upper variable)))
                   (found found))
          (cond
           ((null? uppers) found)
           ((tvar? (car uppers))
            (loop (cdr uppers)
                  (concrete-demands (car uppers) seen found)))
           (else (loop (cdr uppers) (cons (car uppers) found))))))))

;; Generalization-time dual of the union check: a caller-chosen variable
;; whose transitive demands cannot all be met by one value makes the
;; function uncallable, so refuse the definition.
;; The variables sitting inside a demand at the same polarity: a demand
;; (tuple d k l) also demands things of d, k and l. Function domains are
;; the supply side, so only codomains are followed.
(define (component-vars type seen found)
  (cond
   ((not type) found)
   ((union-seen? seen type) found)
   (else
    (union-remember! seen type)
    (let ((shape (join-shape type)))
      (cond
       ((tvar? shape) (cons shape found))
       ((func? shape) (component-vars (func-codomain shape) seen found))
       ((record-type? shape)
        (let loop ((fields (record-type-fields shape)) (found found))
          (if (null? fields)
              found
              (loop (cdr fields)
                    (component-vars (cdar fields) seen found)))))
       ((tuple-type? shape)
        (let loop ((items (tuple-type-items shape)) (found found))
          (if (null? items)
              found
              (loop (cdr items) (component-vars (car items) seen found)))))
       ((variant-type? shape)
        (let loop ((cases (variant-type-cases shape)) (found found))
          (if (null? cases)
              found
              (loop (cdr cases)
                    (let inner ((payloads (cdar cases)) (found found))
                      (if (null? payloads)
                          found
                          (inner (cdr payloads)
                                 (component-vars (car payloads) seen
                                                 found))))))))
       ((nominal? shape)
        (let loop ((args (nominal-args shape)) (found found))
          (if (null? args)
              found
              (loop (cdr args) (component-vars (car args) seen found)))))
       (else found))))))

(define (check-demands variable checked)
  (unless (union-seen? checked variable)
    (union-remember! checked variable)
    (let ((all (concrete-demands variable (vector '()) '())))
      (let loop ((demands all))
        (when (pair? demands)
          (for-each (lambda (other)
                      (unless (meet-compatible? (car demands) other)
                        (fail 'unsatisfiable-demand (car demands) other)))
                    (cdr demands))
          (loop (cdr demands))))
      (for-each (lambda (demand)
                  (for-each (lambda (inner) (check-demands inner checked))
                            (component-vars demand (vector '()) '())))
                all))))

(define (check-union-root type)
  (let* ((polar (polar-variables type))
         (positive (car polar))
         (negative (cdr polar)))
    (let ((checked (vector '())))
      (for-each (lambda (variable) (check-demands variable checked))
                (vector-ref negative 0)))
    (for-each
     (lambda (variable)
       (for-each
        (lambda (upper)
          (when (and (tvar? upper) (union-seen? positive upper))
            (for-each
             (lambda (low)
               (when (and (not (tvar? low))
                          (not (variant-type? (join-shape low)))
                          (not (guaranteed? variable low (vector '()))))
                 (fail 'unresolved-union variable low)))
             (bound-types (tvar-lower upper)))))
        (bound-types (tvar-upper variable))))
     (vector-ref negative 0))))

(define (generalize type level)
  (check-unions type)
  (make-poly level type))
