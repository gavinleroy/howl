(define-module (render graph)
  #:pure
  #:use-module (scheme base)
  #:use-module (lang types)
  #:use-module (lang world)
  #:use-module (render explain)
  #:use-module (render base)
  #:export (constraint-graph
            cgraph? cgraph-nodes cgraph-edges
            cgraph-steps cgraph-causes cgraph-failing
            tedge? tedge-from tedge-to tedge-steps tedge-failing?
            cause? cause-parent cause-child cause-path
            keep-steps))

;; The constraint graph, two layers over the same collection:
;;
;; - The drawn projection: nodes are types, and a directed edge a -> b is
;;   the claim `a <: b` that some derivation step asserted. Concrete types
;;   only ever sit at the rim (sources flow in as lower bounds, sinks are
;;   the demands), so the picture reads as flow between types.
;; - The provenance layer: every <step> (one judgment `sub <: sup`) that
;;   contributed, linked by cause edges parent -> child recording that
;;   checking the parent forced the child. Each drawn edge keeps the steps
;;   that asserted its claim, so the full derivation stays reachable.

(define-record-type <cgraph>
  (make-cgraph cgraph-nodes cgraph-edges cgraph-steps cgraph-causes
               cgraph-failing)
  cgraph?
  (cgraph-nodes cgraph-nodes)
  (cgraph-edges cgraph-edges)
  (cgraph-steps cgraph-steps)
  (cgraph-causes cgraph-causes)
  (cgraph-failing cgraph-failing))

(define-record-type <tedge>
  (make-tedge tedge-from tedge-to tedge-steps tedge-failing?)
  tedge?
  (tedge-from tedge-from)
  (tedge-to tedge-to)
  (tedge-steps tedge-steps set-tedge-steps!)
  (tedge-failing? tedge-failing? set-tedge-failing!))

(define-record-type <cause>
  (make-cause cause-parent cause-child cause-path)
  cause?
  (cause-parent cause-parent)
  (cause-child cause-child)
  (cause-path cause-path))

(define (keep-steps items)
  (let loop ((items items) (kept '()))
    (cond ((null? items) (reverse kept))
          ((step? (car items)) (loop (cdr items) (cons (car items) kept)))
          (else (loop (cdr items) kept)))))

(define (bound-steps variable)
  (keep-steps (map bound-step (append (tvar-lower variable)
                                      (tvar-upper variable)))))

(define (step-path step)
  (let ((origin (step-origin step)))
    (if (origin? origin) (or (origin-path origin) '()) '())))

;; The child's origin-path relative to the parent's: the tail past the
;; shared prefix, or the child's whole path when the parent's is not a
;; prefix (propagation and copy steps restart the path).
(define (path-tail parent-path child-path)
  (let loop ((above parent-path) (below child-path))
    (cond ((null? above) below)
          ((null? below) child-path)
          ((equal? (car above) (car below)) (loop (cdr above) (cdr below)))
          (else child-path))))

(define (edge-path parent child)
  (path-tail (step-path parent) (step-path child)))

;; Fold the steps into one drawn edge per distinct (sub . sup) type pair,
;; keeping every step that asserted the pair and flagging the pair the
;; failing step belongs to.
(define (project-edges steps failing)
  (let loop ((steps steps) (edges '()))
    (if (null? steps)
        (reverse edges)
        (let* ((step (car steps))
               (found (find-first (lambda (edge)
                                    (and (eq? (tedge-from edge)
                                              (step-sub step))
                                         (eq? (tedge-to edge)
                                              (step-sup step))))
                                  edges)))
          (cond
           (found
            (set-tedge-steps! found (append (tedge-steps found) (list step)))
            (if (eq? step failing) (set-tedge-failing! found #t))
            (loop (cdr steps) edges))
           (else
            (loop (cdr steps)
                  (cons (make-tedge (step-sub step) (step-sup step)
                                    (list step) (eq? step failing))
                        edges))))))))

(define (project-nodes steps)
  (let ((table (vector '())))
    (let loop ((steps steps) (nodes '()))
      (if (null? steps)
          (reverse nodes)
          (let sides ((remaining (list (step-sub (car steps))
                                       (step-sup (car steps))))
                      (nodes nodes))
            (cond
             ((null? remaining) (loop (cdr steps) nodes))
             ((seen? table (car remaining)) (sides (cdr remaining) nodes))
             (else
              (remember! table (car remaining))
              (sides (cdr remaining) (cons (car remaining) nodes)))))))))

;; Seeds: the failing step plus the recording step of every bound on every
;; variable reachable from the diagnostic's types. The fixpoint then chases
;; step-causes upward and, for each variable a judgment mentions, the steps
;; behind that variable's own bounds.
(define (constraint-graph diagnostic)
  (let* ((actual (field diagnostic "actual"))
         (expected (field diagnostic "expected"))
         (constraint (diagnostic-constraint diagnostic))
         (roots (append (if actual (list actual) '())
                        (if expected (list expected) '())
                        (if constraint
                            (list (car constraint) (cdr constraint))
                            '())))
         (failing (let ((step (diagnostic-step diagnostic)))
                    (and (step? step) step)))
         (root-variables (keep-bounded (bounds-variables roots (vector '()) '())))
         (seeds (append (if failing (list failing) '())
                        (apply append (map bound-steps root-variables))))
         (steps-table (vector '()))
         (tvars-table (vector '())))
    (let loop ((queue seeds) (steps '()) (causes '()))
      (cond
       ((null? queue)
        (let ((steps (reverse steps)))
          (make-cgraph (project-nodes steps)
                       (project-edges steps failing)
                       steps causes failing)))
       ((seen? steps-table (car queue))
        (loop (cdr queue) steps causes))
       (else
        (let* ((step (car queue)))
          (remember! steps-table step)
          (let* ((parents (keep-steps (step-causes step)))
                 (edges (map (lambda (parent)
                               (make-cause parent step (edge-path parent step)))
                             parents)))
            (let sides ((remaining (list (step-sub step) (step-sup step)))
                        (queue (append (cdr queue) parents)))
              (cond
               ((null? remaining)
                (loop queue (cons step steps) (append causes edges)))
               ((and (tvar? (car remaining))
                     (not (seen? tvars-table (car remaining))))
                (remember! tvars-table (car remaining))
                (sides (cdr remaining)
                       (append queue (bound-steps (car remaining)))))
               (else (sides (cdr remaining) queue)))))))))))
