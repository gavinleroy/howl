(define-module (lang print)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme cxr)
  #:use-module (lang types)
  #:export (type->text
            entry->text
            render-parts
            render-groups
            make-type-printer
            variable-printer
            brief-printer
            clean-printer
            scoped-printer
            bare-printer))

(define-record-type <binder>
  (make-binder binder-id)
  binder?
  (binder-id binder-id))

(define-record-type <pvar>
  (make-pvar pvar-key)
  pvar?
  (pvar-key pvar-key))

(define-record-type <patom>
  (make-patom patom-text)
  patom?
  (patom-text patom-text))

(define-record-type <papply>
  (make-papply papply-name papply-args)
  papply?
  (papply-name papply-name)
  (papply-args papply-args))

(define-record-type <parrow>
  (make-parrow parrow-domains parrow-codomain)
  parrow?
  (parrow-domains parrow-domains)
  (parrow-codomain parrow-codomain))

(define-record-type <precord>
  (make-precord precord-fields)
  precord?
  (precord-fields precord-fields))

(define-record-type <ptuple>
  (make-ptuple ptuple-items)
  ptuple?
  (ptuple-items ptuple-items))

(define-record-type <pvariant>
  (make-pvariant pvariant-cases)
  pvariant?
  (pvariant-cases pvariant-cases))

(define-record-type <pjoin>
  (make-pjoin pjoin-parts)
  pjoin?
  (pjoin-parts pjoin-parts))

(define-record-type <pmeet>
  (make-pmeet pmeet-parts)
  pmeet?
  (pmeet-parts pmeet-parts))

(define-record-type <prec>
  (make-prec prec-key prec-body)
  prec?
  (prec-key prec-key)
  (prec-body prec-body))

(define %binder-counter (vector 0))

(define (fresh-binder)
  (vector-set! %binder-counter 0 (+ (vector-ref %binder-counter 0) 1))
  (make-binder (vector-ref %binder-counter 0)))

(define (bottom) (make-pjoin '()))
(define (top) (make-pmeet '()))
(define (bottom? p) (and (pjoin? p) (null? (pjoin-parts p))))
(define (top? p) (and (pmeet? p) (null? (pmeet-parts p))))

(define (ptype=? left right)
  (cond
   ((eq? left right) #t)
   ((and (pvar? left) (pvar? right)) (eq? (pvar-key left) (pvar-key right)))
   ((and (patom? left) (patom? right))
    (string=? (patom-text left) (patom-text right)))
   ((and (papply? left) (papply? right))
    (and (string=? (papply-name left) (papply-name right))
         (list=? (papply-args left) (papply-args right))))
   ((and (parrow? left) (parrow? right))
    (and (list=? (parrow-domains left) (parrow-domains right))
         (ptype=? (parrow-codomain left) (parrow-codomain right))))
   ((and (precord? left) (precord? right))
    (fields=? (precord-fields left) (precord-fields right)))
   ((and (ptuple? left) (ptuple? right))
    (list=? (ptuple-items left) (ptuple-items right)))
   ((and (pvariant? left) (pvariant? right))
    (cases=? (pvariant-cases left) (pvariant-cases right)))
   ((and (pjoin? left) (pjoin? right))
    (list=? (pjoin-parts left) (pjoin-parts right)))
   ((and (pmeet? left) (pmeet? right))
    (list=? (pmeet-parts left) (pmeet-parts right)))
   ((and (prec? left) (prec? right))
    (and (eq? (prec-key left) (prec-key right))
         (ptype=? (prec-body left) (prec-body right))))
   (else #f)))

(define (list=? left right)
  (cond ((and (null? left) (null? right)) #t)
        ((or (null? left) (null? right)) #f)
        ((ptype=? (car left) (car right)) (list=? (cdr left) (cdr right)))
        (else #f)))

(define (fields=? left right)
  (cond ((and (null? left) (null? right)) #t)
        ((or (null? left) (null? right)) #f)
        ((and (eq? (caar left) (caar right))
              (ptype=? (cdar left) (cdar right)))
         (fields=? (cdr left) (cdr right)))
        (else #f)))

(define (cases=? left right)
  (cond ((and (null? left) (null? right)) #t)
        ((or (null? left) (null? right)) #f)
        ((and (eq? (caar left) (caar right))
              (list=? (cdar left) (cdar right)))
         (cases=? (cdr left) (cdr right)))
        (else #f)))

(define (pair-table-ref table node polarity)
  (let ((known (find-first (lambda (entry)
                             (and (eq? (caar entry) node)
                                  (eq? (cdar entry) polarity)))
                           (vector-ref table 0))))
    (and known (cdr known))))

(define (pair-table-set! table node polarity value)
  (vector-set! table 0
               (cons (cons (cons node polarity) value)
                     (vector-ref table 0))))

(define (coalesce type) (coalesce-at type #t))

(define (coalesce-at type start)
  (let ((recursive (vector '()))
        (finished (vector '())))
    (define (binder-for key polarity create?)
      (let ((known (pair-table-ref recursive key polarity)))
        (cond
         (known known)
         ((not create?) #f)
         (else
          (let ((binder (fresh-binder)))
            (pair-table-set! recursive key polarity binder)
            binder)))))
    (define (go type polarity in-process)
      (cond
       ((tvar? type)
        (cond
         ((any-of (lambda (entry)
                    (and (eq? (car entry) type) (eq? (cdr entry) polarity)))
                  in-process)
          (make-pvar (binder-for type polarity #t)))
         ((pair-table-ref finished type polarity))
         (else
          (let* ((bounds (bound-types
                          (if polarity (tvar-lower type) (tvar-upper type))))
                 (inner (cons (cons type polarity) in-process))
                 (parts (map (lambda (bound) (go bound polarity inner)) bounds))
                 (merged (if polarity
                             (make-pjoin (cons (make-pvar type) parts))
                             (make-pmeet (cons (make-pvar type) parts))))
                 (binder (binder-for type polarity #f))
                 (result (if binder (make-prec binder merged) merged)))
            (pair-table-set! finished type polarity result)
            result))))
       ((func? type)
        (make-parrow (map (lambda (domain)
                            (go domain (not polarity) in-process))
                          (func-domains type))
                     (go (func-codomain type) polarity in-process)))
       ((record-type? type)
        (make-precord (map (lambda (field)
                             (cons (car field) (go (cdr field) polarity in-process)))
                           (record-type-fields type))))
       ((tuple-type? type)
        (make-ptuple (map (lambda (item) (go item polarity in-process))
                          (tuple-type-items type))))
       ((variant-type? type)
        (make-pvariant (map (lambda (variant)
                              (cons (car variant)
                                    (map (lambda (payload)
                                           (go payload polarity in-process))
                                         (cdr variant))))
                            (variant-type-cases type))))
       ((prim? type) (make-patom (symbol->string (prim-name type))))
       ((nominal? type)
        (if (null? (nominal-args type))
            (make-patom (display-name (nominal-name type)))
            (make-papply (display-name (nominal-name type))
                         (map (lambda (arg) (go arg polarity in-process))
                              (nominal-args type)))))
       (else (make-patom "?"))))
    (go type start '())))

;; Count variable occurrences per polarity. The prepared ptype is a DAG
;; (coalescing memoizes shared nodes) and rendering prints a shared node
;; at every reference, so counting must too: each node's contribution
;; list is computed once and memoized, then replayed per reference.
(define (polarities-at ptype start)
  (let ((table (vector '()))
        (visited (vector '())))
    (define (note! key polarity)
      (let ((known (assq key (vector-ref table 0))))
        (if known
            (vector-set! (cdr known) (if polarity 0 1)
                         (+ 1 (vector-ref (cdr known) (if polarity 0 1))))
            (let ((counts (vector 0 0)))
              (vector-set! counts (if polarity 0 1) 1)
              (vector-set! table 0
                           (cons (cons key counts) (vector-ref table 0)))))))
    (define (contributions-list ps polarity)
      (if (null? ps)
          '()
          (append (contributions (car ps) polarity)
                  (contributions-list (cdr ps) polarity))))
    (define (contributions p polarity)
      (or (pair-table-ref visited p polarity)
          (begin
            (pair-table-set! visited p polarity '())
            (let ((found
                   (cond
                    ((pvar? p) (list (cons (pvar-key p) polarity)))
                    ((parrow? p)
                     (append (contributions-list (parrow-domains p)
                                                 (not polarity))
                             (contributions (parrow-codomain p) polarity)))
                    ((precord? p)
                     (contributions-list (map cdr (precord-fields p))
                                         polarity))
                    ((ptuple? p)
                     (contributions-list (ptuple-items p) polarity))
                    ((pvariant? p)
                     (contributions-list
                      (apply append (map cdr (pvariant-cases p)))
                      polarity))
                    ((pjoin? p)
                     (contributions-list (pjoin-parts p) polarity))
                    ((pmeet? p)
                     (contributions-list (pmeet-parts p) polarity))
                    ((prec? p) (contributions (prec-body p) polarity))
                    ((papply? p)
                     (contributions-list (papply-args p) polarity))
                    (else '()))))
              (pair-table-set! visited p polarity found)
              found))))
    (for-each (lambda (entry) (note! (car entry) (cdr entry)))
              (contributions ptype start))
    (vector-ref table 0)))

(define (occurs-once? table key)
  (let ((known (assq key table)))
    (and known
         (= 1 (+ (vector-ref (cdr known) 0)
                 (vector-ref (cdr known) 1))))))

(define (concrete-parts parts)
  (let loop ((rest parts) (kept '()))
    (cond ((null? rest) (reverse kept))
          ((pvar? (car rest)) (loop (cdr rest) kept))
          (else (loop (cdr rest) (cons (car rest) kept))))))

;; For each variable, the non-variable parts it sits beside in its join or
;; meet, recorded once per occurrence. A variable that sits beside the very
;; same concrete type at every occurrence, all at one polarity, adds
;; nothing a reader can act on: replacing it with that type preserves the
;; meaning, equalities included, because the positions that shared the
;; variable then hold the same concrete type. A variable beside nothing
;; concrete, or beside different types, is carrying real information.
(define (co-occurrences-at ptype start)
  (let ((table (vector '()))
        (visited (vector '())))
    (define (note! entry)
      (let* ((key (car entry))
             (siblings (cadr entry))
             (polarity (caddr entry))
             (known (assq key (vector-ref table 0))))
        (if known
            (let ((seen (cdr known)))
              (unless (and (vector-ref seen 0)
                           (eq? polarity (vector-ref seen 2))
                           (= (length siblings) (length (vector-ref seen 1)))
                           (list=? siblings (vector-ref seen 1)))
                (vector-set! seen 0 #f)))
            (vector-set! table 0
                         (cons (cons key (vector (pair? siblings)
                                                 siblings polarity))
                               (vector-ref table 0))))))
    (define (beside parts polarity)
      (let ((concrete (concrete-parts parts)))
        (let loop ((rest parts) (acc '()))
          (cond
           ((null? rest) (append (reverse acc)
                                 (contributions-list concrete polarity)))
           ((pvar? (car rest))
            (loop (cdr rest)
                  (cons (list (pvar-key (car rest)) concrete polarity) acc)))
           (else (loop (cdr rest) acc))))))
    (define (contributions-list ps polarity)
      (if (null? ps)
          '()
          (append (contributions (car ps) polarity)
                  (contributions-list (cdr ps) polarity))))
    (define (contributions p polarity)
      (or (pair-table-ref visited p polarity)
          (begin
            (pair-table-set! visited p polarity '())
            (let ((found
                   (cond
                    ((pvar? p) (list (list (pvar-key p) '() polarity)))
                    ((pjoin? p) (beside (pjoin-parts p) polarity))
                    ((pmeet? p) (beside (pmeet-parts p) polarity))
                    ((parrow? p)
                     (append (contributions-list (parrow-domains p)
                                                 (not polarity))
                             (contributions (parrow-codomain p) polarity)))
                    ((precord? p)
                     (contributions-list (map cdr (precord-fields p))
                                         polarity))
                    ((ptuple? p)
                     (contributions-list (ptuple-items p) polarity))
                    ((pvariant? p)
                     (contributions-list
                      (apply append (map cdr (pvariant-cases p)))
                      polarity))
                    ((prec? p) (contributions (prec-body p) polarity))
                    ((papply? p)
                     (contributions-list (papply-args p) polarity))
                    (else '()))))
              (pair-table-set! visited p polarity found)
              found))))
    (for-each note! (contributions ptype start))
    (vector-ref table 0)))

;; The variables a printed type can lose: those occurring once, and those
;; standing beside the same concrete type everywhere.
(define (droppable-keys coalesced polarity)
  (let ((counts (polarities-at coalesced polarity))
        (beside (co-occurrences-at coalesced polarity)))
    (let loop ((entries beside) (keys '()))
      (cond
       ((null? entries)
        (let once ((rest counts) (keys keys))
          (cond ((null? rest) keys)
                ((and (tvar? (caar rest))
                      (occurs-once? counts (caar rest))
                      (not (memq (caar rest) keys)))
                 (once (cdr rest) (cons (caar rest) keys)))
                (else (once (cdr rest) keys)))))
       ((and (tvar? (caar entries)) (vector-ref (cdar entries) 0))
        (loop (cdr entries) (cons (caar entries) keys)))
       (else (loop (cdr entries) keys))))))

;; Droppable variables are removed from the join or meet they sit in —
;; `(∪ 'a int)` is just `int`. When every part is droppable there is
;; nothing left to stand for them, and emptying the join or meet would
;; print `nothing` or `any`, discarding the name and every equality it
;; carried. In that case keep the variable, which coalescing puts first.
(define (erasable? part drops)
  (and (pvar? part)
       (tvar? (pvar-key part))
       (memq (pvar-key part) drops)
       #t))

(define (prune parts drops)
  (let loop ((rest parts) (kept '()))
    (cond
     ((null? rest)
      (cond ((pair? kept) (reverse kept))
            ((pair? parts) (list (car parts)))
            (else '())))
     ((erasable? (car rest) drops) (loop (cdr rest) kept))
     (else (loop (cdr rest) (cons (car rest) kept))))))

(define (add-unique part parts)
  (if (any-of (lambda (known) (ptype=? known part)) parts)
      parts
      (append parts (list part))))

(define (partition-kind pred parts)
  (let loop ((parts parts) (matching '()) (rest '()))
    (cond
     ((null? parts) (cons (reverse matching) (reverse rest)))
     ((pred (car parts)) (loop (cdr parts) (cons (car parts) matching) rest))
     (else (loop (cdr parts) matching (cons (car parts) rest))))))

(define (flatten-join parts)
  (let loop ((parts parts) (flat '()))
    (cond
     ((null? parts) flat)
     ((pjoin? (car parts)) (loop (cdr parts) (loop (pjoin-parts (car parts)) flat)))
     ((bottom? (car parts)) (loop (cdr parts) flat))
     (else (loop (cdr parts) (add-unique (car parts) flat))))))

(define (flatten-meet parts)
  (let loop ((parts parts) (flat '()))
    (cond
     ((null? parts) flat)
     ((pmeet? (car parts)) (loop (cdr parts) (loop (pmeet-parts (car parts)) flat)))
     ((top? (car parts)) (loop (cdr parts) flat))
     (else (loop (cdr parts) (add-unique (car parts) flat))))))

(define (merge-record-join records)
  (make-precord
   (sort-by-label
    (let loop ((labels (map car (precord-fields (car records)))) (fields '()))
     (if (null? labels)
         (reverse fields)
         (let* ((label (car labels))
                (found (map (lambda (record) (assq label (precord-fields record)))
                            records)))
           (if (any-of (lambda (entry) (not entry)) found)
               (loop (cdr labels) fields)
               (loop (cdr labels)
                     (cons (cons label (build-join (map cdr found))) fields)))))))))

(define (merge-record-meet records)
  (make-precord
   (sort-by-label
    (let loop ((remaining records) (fields '()))
     (if (null? remaining)
         fields
         (loop (cdr remaining)
               (let merge ((incoming (precord-fields (car remaining)))
                           (fields fields))
                 (if (null? incoming)
                     fields
                     (let ((known (assq (caar incoming) fields)))
                       (merge (cdr incoming)
                              (if known
                                  (map (lambda (field)
                                         (if (eq? (car field) (caar incoming))
                                             (cons (car field)
                                                   (build-meet (list (cdr field)
                                                                     (cdar incoming))))
                                             field))
                                       fields)
                                  (append fields (list (car incoming))))))))))))))

(define (merge-variant-join variants)
  (make-pvariant
   (sort-by-label
    (let loop ((remaining variants) (cases '()))
     (if (null? remaining)
         cases
         (loop (cdr remaining)
               (let merge ((incoming (pvariant-cases (car remaining))) (cases cases))
                 (if (null? incoming)
                     cases
                     (let ((known (assq (caar incoming) cases)))
                       (merge (cdr incoming)
                              (cond
                               ((not known) (append cases (list (car incoming))))
                               ((= (length (cdr known)) (length (cdar incoming)))
                                (map (lambda (variant)
                                       (if (eq? (car variant) (caar incoming))
                                           (cons (car variant)
                                                 (map (lambda (a b) (build-join (list a b)))
                                                      (cdr variant)
                                                      (cdar incoming)))
                                           variant))
                                     cases))
                               (else cases))))))))))))

(define (same-width? tuples)
  (let ((width (length (ptuple-items (car tuples)))))
    (let loop ((rest (cdr tuples)))
      (cond ((null? rest) #t)
            ((= (length (ptuple-items (car rest))) width) (loop (cdr rest)))
            (else #f)))))

(define (merge-tuples tuples combine)
  (make-ptuple
   (let loop ((columns (map ptuple-items tuples)) (items '()))
     (if (null? (car columns))
         (reverse items)
         (loop (map cdr columns)
               (cons (combine (map car columns)) items))))))

(define (merge-tuple-join tuples) (merge-tuples tuples build-join))

(define (merge-tuple-meet tuples) (merge-tuples tuples build-meet))

(define (same-arity? arrows)
  (let ((width (length (parrow-domains (car arrows)))))
    (let loop ((arrows (cdr arrows)))
      (cond ((null? arrows) #t)
            ((= (length (parrow-domains (car arrows))) width)
             (loop (cdr arrows)))
            (else #f)))))

(define (merge-arrow-meet arrows)
  (make-parrow
   (let loop ((columns (map parrow-domains arrows)) (domains '()))
     (if (null? (car columns))
         (reverse domains)
         (loop (map cdr columns)
               (cons (build-join (map car columns)) domains))))
   (build-meet (map parrow-codomain arrows))))

(define (build-join raw)
  (let* ((parts (flatten-join raw))
         (records (partition-kind precord? parts))
         (parts (if (> (length (car records)) 1)
                    (add-unique (merge-record-join (car records)) (cdr records))
                    parts))
         (variants (partition-kind pvariant? parts))
         (parts (if (> (length (car variants)) 1)
                    (add-unique (merge-variant-join (car variants)) (cdr variants))
                    parts))
         (tuples (partition-kind ptuple? parts))
         (parts (if (and (> (length (car tuples)) 1) (same-width? (car tuples)))
                    (add-unique (merge-tuple-join (car tuples)) (cdr tuples))
                    parts)))
    (cond
     ((null? parts) (bottom))
     ((null? (cdr parts)) (car parts))
     (else (make-pjoin parts)))))

(define (build-meet raw)
  (let* ((parts (flatten-meet raw))
         (arrows (partition-kind parrow? parts))
         (parts (if (and (> (length (car arrows)) 1)
                         (same-arity? (car arrows)))
                    (add-unique (merge-arrow-meet (car arrows)) (cdr arrows))
                    parts))
         (records (partition-kind precord? parts))
         (parts (if (> (length (car records)) 1)
                    (add-unique (merge-record-meet (car records)) (cdr records))
                    parts))
         (tuples (partition-kind ptuple? parts))
         (parts (if (and (> (length (car tuples)) 1) (same-width? (car tuples)))
                    (add-unique (merge-tuple-meet (car tuples)) (cdr tuples))
                    parts)))
    (cond
     ((null? parts) (top))
     ((null? (cdr parts)) (car parts))
     (else (make-pmeet parts)))))

(define (sort-by-label entries)
  (let loop ((remaining entries) (sorted '()))
    (if (null? remaining)
        sorted
        (loop (cdr remaining) (insert-by-label (car remaining) sorted)))))

(define (insert-by-label entry sorted)
  (cond
   ((null? sorted) (list entry))
   ((string<? (symbol->string (car entry)) (symbol->string (caar sorted)))
    (cons entry sorted))
   (else (cons (car sorted) (insert-by-label entry (cdr sorted))))))

;; A variable prints as itself; redundant ones are dropped from the join
;; or meet they sit in, by `prune`, which keeps the name when there is
;; nothing else left to stand for it.
(define (simplify ptype polarity table cache)
  (or (pair-table-ref cache ptype polarity)
      (let ((result
             (cond
              ((pvar? ptype) ptype)
              ((pjoin? ptype)
               (build-join (prune (map (lambda (part)
                                         (simplify part polarity table cache))
                                       (pjoin-parts ptype))
                                  table)))
              ((pmeet? ptype)
               (build-meet (prune (map (lambda (part)
                                         (simplify part polarity table cache))
                                       (pmeet-parts ptype))
                                  table)))
              ((parrow? ptype)
               (make-parrow (map (lambda (domain)
                                   (simplify domain (not polarity)
                                             table cache))
                                 (parrow-domains ptype))
                            (simplify (parrow-codomain ptype)
                                      polarity table cache)))
              ((papply? ptype)
               (make-papply (papply-name ptype)
                            (map (lambda (arg)
                                   (simplify arg polarity table cache))
                                 (papply-args ptype))))
              ((precord? ptype)
               (make-precord
                (sort-by-label
                 (map (lambda (field)
                        (cons (car field)
                              (simplify (cdr field) polarity table cache)))
                      (precord-fields ptype)))))
              ((ptuple? ptype)
               (make-ptuple (map (lambda (item)
                                   (simplify item polarity table cache))
                                 (ptuple-items ptype))))
              ((pvariant? ptype)
               (make-pvariant
                (sort-by-label
                 (map (lambda (variant)
                        (cons (car variant)
                              (map (lambda (payload)
                                     (simplify payload polarity table cache))
                                   (cdr variant))))
                      (pvariant-cases ptype)))))
              ((prec? ptype)
               (let ((body (simplify (prec-body ptype) polarity table cache)))
                 (if (mentions? body (prec-key ptype))
                     (make-prec (prec-key ptype) body)
                     body)))
              (else ptype))))
        (pair-table-set! cache ptype polarity result)
        result)))

(define (mentions? ptype key)
  (cond
   ((pvar? ptype) (eq? (pvar-key ptype) key))
   ((parrow? ptype) (or (any-of (lambda (domain) (mentions? domain key))
                                (parrow-domains ptype))
                        (mentions? (parrow-codomain ptype) key)))
   ((precord? ptype)
    (any-of (lambda (field) (mentions? (cdr field) key)) (precord-fields ptype)))
   ((ptuple? ptype)
    (any-of (lambda (item) (mentions? item key)) (ptuple-items ptype)))
   ((pvariant? ptype)
    (any-of (lambda (variant)
              (any-of (lambda (payload) (mentions? payload key)) (cdr variant)))
            (pvariant-cases ptype)))
   ((pjoin? ptype) (any-of (lambda (part) (mentions? part key)) (pjoin-parts ptype)))
   ((pmeet? ptype) (any-of (lambda (part) (mentions? part key)) (pmeet-parts ptype)))
   ((prec? ptype) (mentions? (prec-body ptype) key))
   ((papply? ptype)
    (any-of (lambda (arg) (mentions? arg key)) (papply-args ptype)))
   (else #f)))

;; Drop the bounds printed beside a variable that survived pruning.
;; Coalescing emits each variable as itself meet its upper bounds (or join
;; its lower bounds), so those neighbours are the variable's own bounds and
;; meeting a variable with its own upper bound narrows nothing. A caller
;; that wants the declared shape reports them separately instead.
(define (strip-bounds ptype)
  (let ((cache (vector '())))
    (define (variables parts)
      (let loop ((rest parts) (kept '()))
        (cond ((null? rest) (reverse kept))
              ((and (pvar? (car rest)) (tvar? (pvar-key (car rest))))
               (loop (cdr rest) (cons (car rest) kept)))
              (else (loop (cdr rest) kept)))))
    (define (go p)
      (or (pair-table-ref cache p #t)
          (let ((result
                 (cond
                  ((pjoin? p)
                   (let ((bare (variables (pjoin-parts p))))
                     (build-join (if (pair? bare)
                                     bare
                                     (map go (pjoin-parts p))))))
                  ((pmeet? p)
                   (let ((bare (variables (pmeet-parts p))))
                     (build-meet (if (pair? bare)
                                     bare
                                     (map go (pmeet-parts p))))))
                  ((parrow? p)
                   (make-parrow (map go (parrow-domains p))
                                (go (parrow-codomain p))))
                  ((precord? p)
                   (make-precord (map (lambda (field)
                                        (cons (car field) (go (cdr field))))
                                      (precord-fields p))))
                  ((ptuple? p) (make-ptuple (map go (ptuple-items p))))
                  ((pvariant? p)
                   (make-pvariant (map (lambda (variant)
                                         (cons (car variant)
                                               (map go (cdr variant))))
                                       (pvariant-cases p))))
                  ((prec? p) (make-prec (prec-key p) (go (prec-body p))))
                  ((papply? p)
                   (make-papply (papply-name p) (map go (papply-args p))))
                  (else p))))
            (pair-table-set! cache p #t result)
            result)))
    (go ptype)))

;; The variables left standing in a stripped type — the only ones a
;; where-clause should speak about.
(define (bare-variables ptype)
  (let ((seen (vector '()))
        (found (vector '())))
    (define (go p)
      (unless (pair-table-ref seen p #t)
        (pair-table-set! seen p #t #t)
        (cond
         ((pvar? p)
          (when (and (tvar? (pvar-key p))
                     (not (memq (pvar-key p) (vector-ref found 0))))
            (vector-set! found 0 (cons (pvar-key p) (vector-ref found 0)))))
         ((pjoin? p) (for-each go (pjoin-parts p)))
         ((pmeet? p) (for-each go (pmeet-parts p)))
         ((parrow? p)
          (for-each go (parrow-domains p))
          (go (parrow-codomain p)))
         ((precord? p) (for-each (lambda (f) (go (cdr f))) (precord-fields p)))
         ((ptuple? p) (for-each go (ptuple-items p)))
         ((pvariant? p)
          (for-each (lambda (v) (for-each go (cdr v))) (pvariant-cases p)))
         ((prec? p) (go (prec-body p)))
         ((papply? p) (for-each go (papply-args p))))))
    (go ptype)
    (reverse (vector-ref found 0))))

(define (prepare type) (prepare-at type #t))

(define (prepare-at type polarity)
  (let ((coalesced (coalesce-at type polarity)))
    (simplify coalesced polarity (droppable-keys coalesced polarity)
              (vector '()))))

(define (variable-name index)
  (let ((letter (string (integer->char (+ (char->integer #\a) (remainder index 26)))))
        (cycle (quotient index 26)))
    (string-append "'" letter (if (= cycle 0) "" (number->string cycle)))))

(define (name-for names key)
  (let ((known (assq key (vector-ref names 0))))
    (if known
        (cdr known)
        (let ((name (variable-name (length (vector-ref names 0)))))
          (vector-set! names 0 (cons (cons key name) (vector-ref names 0)))
          name))))


(define (render ptype names)
  (cond
   ((pvar? ptype) (name-for names (pvar-key ptype)))
   ((patom? ptype) (patom-text ptype))
   ((papply? ptype)
    (string-append "(" (papply-name ptype)
                   (apply string-append
                          (map (lambda (arg)
                                 (string-append " " (render arg names)))
                               (papply-args ptype)))
                   ")"))
   ((bottom? ptype) "nothing")
   ((top? ptype) "any")
   ((parrow? ptype)
    (string-append "(-> ("
                   (string-join (map (lambda (part) (render part names))
                                     (parrow-domains ptype))
                                " ")
                   ") "
                   (render (parrow-codomain ptype) names)
                   ")"))
   ((precord? ptype)
    (string-append "(record"
                   (apply string-append
                          (map (lambda (field)
                                 (string-append " (" (symbol->string (car field))
                                                " " (render (cdr field) names) ")"))
                               (precord-fields ptype)))
                   ")"))
   ((ptuple? ptype)
    (string-append "(tuple"
                   (apply string-append
                          (map (lambda (item)
                                 (string-append " " (render item names)))
                               (ptuple-items ptype)))
                   ")"))
   ((pvariant? ptype)
    (string-append "(enum"
                   (apply string-append
                          (map (lambda (variant)
                                 (if (null? (cdr variant))
                                     (string-append " " (symbol->string (car variant)))
                                     (string-append
                                      " (" (symbol->string (car variant))
                                      (apply string-append
                                             (map (lambda (payload)
                                                    (string-append
                                                     " " (render payload names)))
                                                  (cdr variant)))
                                      ")")))
                               (pvariant-cases ptype)))
                   ")"))
   ((pjoin? ptype)
    (string-append "(∪ "
                   (string-join (map (lambda (part) (render part names))
                                     (pjoin-parts ptype))
                                " ")
                   ")"))
   ((pmeet? ptype)
    (string-append "(∩ "
                   (string-join (map (lambda (part) (render part names))
                                     (pmeet-parts ptype))
                                " ")
                   ")"))
   ((prec? ptype)
    (string-append "(rec " (name-for names (prec-key ptype)) " "
                   (render (prec-body ptype) names) ")"))
   (else "?")))

(define (type->text type)
  (render (prepare type) (vector '())))

(define (entry->text entry)
  (type->text (if (poly? entry) (poly-body entry) entry)))

(define (render-brief type names)
  (cond
   ((tvar? type) (name-for names type))
   ((prim? type) (symbol->string (prim-name type)))
   ((nominal? type)
    (if (null? (nominal-args type))
        (display-name (nominal-name type))
        (string-append "(" (display-name (nominal-name type))
                       (apply string-append
                              (map (lambda (arg)
                                     (string-append " " (render-brief arg names)))
                                   (nominal-args type)))
                       ")")))
   ((func? type)
    (string-append "(-> ("
                   (string-join (map (lambda (domain)
                                       (render-brief domain names))
                                     (func-domains type))
                                " ")
                   ") " (render-brief (func-codomain type) names) ")"))
   ((record-type? type)
    (string-append "(record"
                   (apply string-append
                          (map (lambda (field)
                                 (string-append " (" (symbol->string (car field))
                                                " "
                                                (render-brief (cdr field) names)
                                                ")"))
                               (record-type-fields type)))
                   ")"))
   ((tuple-type? type)
    (string-append "(tuple"
                   (apply string-append
                          (map (lambda (item)
                                 (string-append " " (render-brief item names)))
                               (tuple-type-items type)))
                   ")"))
   ((variant-type? type)
    (string-append "(enum"
                   (apply string-append
                          (map (lambda (variant)
                                 (if (null? (cdr variant))
                                     (string-append " " (symbol->string (car variant)))
                                     (string-append
                                      " (" (symbol->string (car variant))
                                      (apply string-append
                                             (map (lambda (payload)
                                                    (string-append
                                                     " "
                                                     (render-brief payload names)))
                                                  (cdr variant)))
                                      ")")))
                               (variant-type-cases type)))
                   ")"))
   (else "?")))

;; Three ways to name a type, sharing one table so that a variable keeps
;; the same name everywhere one printer is used: by bare name, by
;; structural skeleton, and in full with its bounds folded in at a given
;; polarity. The last also has a scoped form, below.
(define (make-type-printer)
  (let ((names (vector '())))
    (vector (lambda (variable) (name-for names variable))
            (lambda (type) (render-brief type names))
            (lambda (type polarity) (render (prepare-at type polarity) names))
            ;; Printing scoped to a surrounding type: occurrence counts
            ;; come from the context rather than from the fragment, so a
            ;; variable shown on its own is pruned — and therefore named —
            ;; exactly as it is inside the larger type it belongs to.
            (lambda (context context-polarity)
              (let* ((coalesced (coalesce-at context context-polarity))
                     (drops (droppable-keys coalesced context-polarity)))
                (lambda (type polarity)
                  (render (simplify (coalesce-at type polarity) polarity
                                    drops (vector '()))
                          names))))
            ;; The declared shape: surviving variables printed bare, with
            ;; the variables themselves returned so the caller can report
            ;; their bounds. Returns (text . variables).
            (lambda (context context-polarity)
              (let* ((coalesced (coalesce-at context context-polarity))
                     (drops (droppable-keys coalesced context-polarity)))
                (lambda (type polarity)
                  (let ((stripped
                         (strip-bounds
                          (simplify (coalesce-at type polarity) polarity
                                    drops (vector '())))))
                    (cons (render stripped names)
                          (bare-variables stripped)))))))))

(define (variable-printer printer) (vector-ref printer 0))

(define (brief-printer printer) (vector-ref printer 1))

(define (clean-printer printer) (vector-ref printer 2))

(define (scoped-printer printer) (vector-ref printer 3))

(define (bare-printer printer) (vector-ref printer 4))

(define (render-group parts names)
  (let loop ((parts parts) (text ""))
    (if (null? parts)
        text
        (loop (cdr parts)
              (string-append text
                             (if (string? (car parts))
                                 (car parts)
                                 (render (prepare (car parts)) names)))))))

(define (render-groups groups)
  (let ((names (vector '())))
    (map (lambda (parts) (render-group parts names)) groups)))

(define (render-parts parts)
  (render-group parts (vector '())))
