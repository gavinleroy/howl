(define-module (render flow)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme cxr)
  #:use-module (lang types)
  #:use-module (lang world)
  #:use-module (lang print)
  #:use-module (lang reader)
  #:use-module (render base)
  #:use-module (render explain)
  #:use-module (render graph)
  #:export (flow-outline
            flow? flow-source flow-sink flow-sink-spans
            flow-signatures flow-paths
            sig? sig-id sig-name sig-site sig-span sig-slots
            slot? slot-name slot-text
            fpath? fpath-type fpath-signature fpath-slot fpath-origin
            fpath-hops
            hop? hop-text hop-site hop-span hop-spans
            hop-relation hop-why hop-contra?
            hop-sub hop-sup hop-shape
            hop-sub-spans hop-sup-spans))

;; A flow starts at a concrete type sitting in some signature and travels
;; to the conflict, which is the one place two concrete types arrive and
;; cannot both hold. The constraint graph holds all of it: its edges are
;; `a <: b` claims, so a requirement reads from the supertype end to the
;; subtype end — `'c <: int` means int is required of 'c, so int flows
;; onto 'c. Walking out from the conflict and reversing each path gives
;; the story from its origin inward.

(define-record-type <flow>
  (make-flow flow-source flow-sink flow-sink-spans flow-signatures
             flow-paths)
  flow?
  (flow-source flow-source)
  (flow-sink flow-sink)
  (flow-sink-spans flow-sink-spans)
  (flow-signatures flow-signatures)
  (flow-paths flow-paths))

;; One row of the signature panel: a callee's type at one instantiation,
;; split into the slots an arrow can start from.
(define-record-type <sig>
  (make-sig sig-id sig-name sig-site sig-span sig-slots)
  sig?
  (sig-id sig-id)
  (sig-name sig-name)
  (sig-site sig-site)
  (sig-span sig-span)
  (sig-slots sig-slots))

(define-record-type <slot>
  (make-slot slot-name slot-text)
  slot?
  (slot-name slot-name)
  (slot-text slot-text))

(define-record-type <fpath>
  (make-fpath fpath-type fpath-signature fpath-slot fpath-origin fpath-hops)
  fpath?
  (fpath-type fpath-type)
  (fpath-signature fpath-signature)
  (fpath-slot fpath-slot)
  (fpath-origin fpath-origin)
  (fpath-hops fpath-hops))

(define-record-type <hop>
  (make-hop hop-text hop-site hop-span hop-spans hop-relation hop-why
            hop-contra? hop-sub hop-sup hop-shape
            hop-sub-spans hop-sup-spans)
  hop?
  (hop-text hop-text)
  (hop-site hop-site)
  (hop-span hop-span)
  (hop-spans hop-spans)
  (hop-relation hop-relation)
  (hop-why hop-why)
  (hop-contra? hop-contra?)
  ;; The two sides on their own, and the structure they were read off:
  ;; what the view shows, in place of a sentence about either.
  (hop-sub hop-sub)
  (hop-sup hop-sup)
  (hop-shape hop-shape)
  ;; Where each side is written, so a type in the tip can be lit together
  ;; with the code it came from. Either may be empty.
  (hop-sub-spans hop-sub-spans)
  (hop-sup-spans hop-sup-spans))

(define (ordinal n)
  (case n
    ((1) "first") ((2) "second") ((3) "third") ((4) "fourth")
    ((5) "fifth") ((6) "sixth") ((7) "seventh") ((8) "eighth")
    (else (string-append (number->string n) "th"))))

(define (quoted text) (string-append "`" text "`"))

(define (named thing)
  (cond ((symbol? thing) (symbol->string thing))
        ((string? thing) thing)
        ((not thing) "?")
        (else (form->string thing))))

;; Where inside a type the constraint sits. Covers every segment
;; `deeper-at` can produce in src/lang/types.scm.
(define (segment-phrase segment)
  (cond
   ((and (pair? segment) (eq? (car segment) 'domain))
    (string-append "the " (ordinal (cadr segment)) " parameter"))
   ((eq? segment 'result) "the result")
   ((and (pair? segment) (eq? (car segment) 'result)) "the result")
   ((and (pair? segment) (eq? (car segment) 'field))
    (string-append "the field " (quoted (named (cadr segment)))))
   ((and (pair? segment) (eq? (car segment) 'item))
    (string-append "the " (ordinal (cadr segment)) " component"))
   ((and (pair? segment) (eq? (car segment) 'case))
    (string-append "the payload of " (quoted (named (cadr segment)))))
   ((and (pair? segment) (eq? (car segment) 'argument-of))
    (string-append "a type argument of " (quoted (named (cadr segment)))))
   (else #f)))

(define (last-of items)
  (cond ((null? items) #f)
        ((null? (cdr items)) (car items))
        (else (last-of (cdr items)))))

;; A function domain flips variance; an odd number of them means the
;; constraint reads in the opposite direction from the flow.
(define (contravariant? path)
  (let loop ((rest path) (flips 0))
    (cond ((null? rest) (odd? flips))
          ((and (pair? (car rest)) (eq? (caar rest) 'domain))
           (loop (cdr rest) (+ flips 1)))
          (else (loop (cdr rest) flips)))))

;; A hop shown as structure rather than described in words: the type the
;; constraint came out of, printed whole, with the two pieces it relates
;; picked out. Parts concatenate to exactly what `brief` would print, so
;; the marks land on real sub-terms and nothing has to be searched for in
;; a finished string. Roles: 'plain, 'sub (the type that arrived) and
;; 'sup (what the position requires of it).
(define (plain text) (cons text 'plain))

(define (segment-index segment tag)
  (and (pair? segment) (eq? (car segment) tag) (pair? (cdr segment))
       (number? (cadr segment))
       (cadr segment)))

(define (segment-named segment tag)
  (and (pair? segment) (eq? (car segment) tag) (pair? (cdr segment))
       (cadr segment)))

(define (within? index items)
  (and index (>= index 1) (<= index (length items))))

;; The items of a sequence, separated; the index-th is descended into and
;; the rest print flat.
(define (sequence-parts items index rest role text brief separator)
  (let loop ((items items) (at 1) (parts '()))
    (if (null? items)
        parts
        (loop (cdr items) (+ at 1)
              (append parts
                      (if (= at 1) '() (list (plain separator)))
                      (if (= at index)
                          (parts-at (car items) rest role text brief)
                          (list (plain (brief (car items))))))))))

;; Mirrors `render-brief` in src/lang/print.scm, one constructor at a
;; time, so the text matches what the panels print. A position this does
;; not know how to descend marks the whole type instead of guessing.
;;
;; `text` is what to print once the position is reached. The enclosing
;; constraint is a parallel instantiation of the hop's own — same shape,
;; its own variables — so the marked position prints the hop's type
;; rather than the enclosing one's, and the two lines agree on names.
(define (parts-at type path role text brief)
  (let* ((segment (and (pair? path) (car path)))
         (rest (if (pair? path) (cdr path) '()))
         (domain (and segment (segment-index segment 'domain)))
         (item (and segment (segment-index segment 'item)))
         (field (and segment (segment-named segment 'field))))
    (cond
     ((not segment) (list (cons (or text (brief type)) role)))

     ((and (func? type) (within? domain (func-domains type)))
      (append (list (plain "(-> ("))
              (sequence-parts (func-domains type) domain rest role text brief " ")
              (list (plain ") "))
              (list (plain (brief (func-codomain type))))
              (list (plain ")"))))

     ((and (func? type) (or (eq? segment 'result)
                            (and (pair? segment) (eq? (car segment) 'result))))
      (append (list (plain "(-> ("))
              (sequence-parts (func-domains type) 0 '() role text brief " ")
              (list (plain ") "))
              (parts-at (func-codomain type) rest role text brief)
              (list (plain ")"))))

     ((and (tuple-type? type) (within? item (tuple-type-items type)))
      (append (list (plain "(tuple "))
              (sequence-parts (tuple-type-items type) item rest role text brief " ")
              (list (plain ")"))))

     ((and (record-type? type) field
           (assq field (record-type-fields type)))
      (let loop ((fields (record-type-fields type)) (parts (list (plain "(record"))))
        (if (null? fields)
            (append parts (list (plain ")")))
            (let* ((entry (car fields))
                   (head (plain (string-append " (" (symbol->string (car entry))
                                               " ")))
                   (body (if (eq? (car entry) field)
                             (parts-at (cdr entry) rest role text brief)
                             (list (plain (brief (cdr entry)))))))
              (loop (cdr fields)
                    (append parts (list head) body (list (plain ")"))))))))

     (else (list (cons (or text (brief type)) role))))))

;; An application, deconstructed: the callee's type standing where the
;; callee stands, applied to the types of its arguments. Inference states
;; it as `callee <: (-> (argument ...) result)`, so the parameter comes
;; from the left and the argument that disagrees with it from the right.
;;
;; A recursive callee's own type is still a variable, which standing where
;; a function type should be teaches nothing. What the call requires of it
;; is an arrow either way, so that is what prints — the emphasis says
;; which position is under discussion, and every other spot keeps its
;; variable.
(define (application-parts callee demand index sub-text sup-text brief)
  (append (list (plain "("))
          (parts-at (if (and (func? callee) (within? index (func-domains callee)))
                        callee
                        demand)
                    (list (list 'domain index)) 'sup sup-text brief)
          (list (plain " "))
          (sequence-parts (func-domains demand) index '() 'sub sub-text brief " ")
          (list (plain ")"))))

;; The structure the hop was read off, under the constraint itself.
;; Applications print in their surface form. Anything else prints the
;; constraint it was deconstructed from — but only when that is a bigger
;; type than the hop's own constraint, since otherwise the line above
;; already says it.
(define (shape-parts step brief)
  (let* ((origin (step-origin step))
         (left (and (origin? origin) (origin-left origin)))
         (right (and (origin? origin) (origin-right origin)))
         (reason (step-reason step))
         (tag (and reason (reason-tag reason)))
         (path (step-path step))
         (index (and (pair? path) (segment-index (car path) 'domain)))
         (sub-text (brief (step-sub step)))
         (sup-text (brief (step-sup step))))
    (cond
     ((and left right index (eq? tag 'application) (func? right)
           (within? index (func-domains right)))
      (application-parts left right index sub-text sup-text brief))
     ((and left right (pair? path))
      (append (parts-at left path 'sub sub-text brief)
              (list (plain " <: "))
              (parts-at right path 'sup sup-text brief)))
     ;; Taking a value apart has no structure to show beyond the
     ;; constraint itself — the pattern is the whole of it. One word, so
     ;; the hop still says why it exists.
     ((eq? tag 'pattern) (list (cons "deconstructed" 'note)))
     (else '()))))

;; Why the constraint exists. One phrase per reason tag emitted by
;; src/lang/infer.scm — all eleven, so nothing renders blank.
(define (why-phrase step)
  (let* ((reason (step-reason step))
         (tag (reason-tag reason))
         (path (step-path step))
         (tail (last-of path))
         (where (and tail (segment-phrase tail)))
         (site (step-site step))
         (shown (if site (quoted (form->string site)) "this expression")))
    (cond
     ((eq? tag 'application)
      (let ((head (reason-extra reason 0)))
        (cond
         ((and where (pair? tail) (eq? (car tail) 'domain))
          (string-append where " of " (quoted (named head))
                         ", applied at " shown))
         (where (string-append where " of " (quoted (named head))
                               ", applied at " shown))
         (else (string-append "applying " (quoted (named head))
                              " at " shown)))))
     ((eq? tag 'pattern)
      (string-append "destructured by the pattern "
                     (quoted (named (reason-extra reason 0)))))
     ((eq? tag 'clause)
      (string-append "the result of the "
                     (quoted (named (reason-extra reason 0)))
                     " arm of the match"))
     ((eq? tag 'branch)
      (string-append "the " (quoted (named (reason-extra reason 0)))
                     " branch of the if at " shown))
     ((eq? tag 'scrutinee)
      (string-append "matched against the cases of the match at " shown))
     ((eq? tag 'condition)
      (string-append "the condition of the if at " shown
                     ", which must be a bool"))
     ((eq? tag 'boolean-op)
      (string-append "an operand of the boolean operator at " shown))
     ((eq? tag 'recursive)
      (string-append "the function's own type, available inside its body at "
                     shown))
     ((eq? tag 'field)
      (string-append "a record field read at " shown))
     ((eq? tag 'signature)
      "checked against the module signature it is sealed with")
     ((eq? tag 'equivalent)
      "an invariant position, so the constraint is required both ways")
     (where (string-append where " at " shown))
     (else (string-append "constrained at " shown)))))

(define (keep pred items)
  (let loop ((items items) (kept '()))
    (cond ((null? items) (reverse kept))
          ((pred (car items)) (loop (cdr items) (cons (car items) kept)))
          (else (loop (cdr items) kept)))))

(define (step-path step)
  (let ((origin (step-origin step)))
    (if (origin? origin) (or (origin-path origin) '()) '())))

(define (step-site step)
  (let ((origin (step-origin step)))
    (and (origin? origin) (origin-expression origin))))

(define (step-span step)
  (let ((origin (step-origin step)))
    (and (origin? origin) (origin-span origin))))

(define (step-reason step)
  (let ((origin (step-origin step)))
    (and (origin? origin) (origin-reason origin))))

;; Variables sitting directly inside a structural type — where a flow
;; continues after passing through a tuple, record, or variant.
(define (inner-variables type)
  (cond
   ((tvar? type) (list type))
   ((tuple-type? type) (keep tvar? (tuple-type-items type)))
   ((record-type? type) (keep tvar? (map cdr (record-type-fields type))))
   ((variant-type? type)
    (keep tvar? (apply append (map cdr (variant-type-cases type)))))
   ((func? type)
    (keep tvar? (append (func-domains type) (list (func-codomain type)))))
   (else '())))

(define (concrete-leaf? type)
  (and (not (tvar? type)) (null? (inner-variables type))))

;; The signature a constraint came out of: the nearest cause whose own
;; subject is a function type. For `'c <: int` inside `(/ x 2)` that is
;; `/`'s type; for `'d <: float` inside `(Vec2.zip-with -. b a)` it is
;; `-.`'s type, because the float came from the argument, not the callee.
(define (signature-step step)
  (let loop ((queue (keep step? (step-causes step))) (seen '()))
    (cond
     ((null? queue) #f)
     ((memq (car queue) seen) (loop (cdr queue) seen))
     ((func? (step-sub (car queue))) (car queue))
     (else (loop (append (cdr queue) (keep step? (step-causes (car queue))))
                 (cons (car queue) seen))))))

;; What to call that signature: the callee when the step is the
;; application itself, otherwise the argument its path names.
(define (signature-name step)
  (let* ((reason (step-reason step))
         (path (step-path step))
         (head (reason-extra reason 0))
         (args (reason-extra reason 1))
         (lead (and (pair? path) (car path)))
         (index (and (pair? lead) (eq? (car lead) 'domain) (cadr lead))))
    (cond
     ((and index (proper-list? args) (<= index (length args)))
      (list-ref args (- index 1)))
     (head head)
     (else #f))))

(define (drop-prefix prefix path)
  (cond ((null? prefix) path)
        ((null? path) '())
        ((equal? (car prefix) (car path)) (drop-prefix (cdr prefix) (cdr path)))
        (else path)))

(define (slot-label segment)
  (cond
   ((and (pair? segment) (eq? (car segment) 'domain))
    (string-append "domain " (number->string (cadr segment))))
   ((eq? segment 'result) "result")
   ((and (pair? segment) (eq? (car segment) 'result)) "result")
   (else "result")))

;; The slots of an arrow: one per domain, then the result.
(define (arrow-slots type brief)
  (if (not (func? type))
      (list (make-slot "result" (brief type)))
      (let loop ((domains (func-domains type)) (index 1) (slots '()))
        (if (null? domains)
            (reverse (cons (make-slot "result" (brief (func-codomain type)))
                           slots))
            (loop (cdr domains) (+ index 1)
                  (cons (make-slot (string-append "domain "
                                                  (number->string index))
                                   (brief (car domains)))
                        slots))))))

;; The argument a (domain N) step is about, if the step is about one.
(define (argument-spans-of step)
  (let* ((reason (step-reason step))
         (path (step-path step))
         (site (step-site step))
         (args (reason-extra reason 1))
         (lead (and (pair? path) (car path)))
         (index (and (pair? lead) (eq? (car lead) 'domain) (cadr lead)))
         (arg (and index (proper-list? args) (<= index (length args))
                   (list-ref args (- index 1)))))
    (if (and arg site) (spans-within arg site) '())))

;; Where inference first wrote the variable down. A parameter's variable
;; is born at the parameter, so a type with no expression of its own at
;; this step can still be pointed at.
(define (variable-spans type)
  (if (and (tvar? type) (tvar-source-span type))
      (list (tvar-source-span type))
      '()))

;; Where each side of the constraint is written. The type that arrived is
;; written at the argument, or failing that wherever its variable was
;; born; what is required of it is written at the callee, whose type says
;; what its parameters take.
(define (sub-spans-of step)
  (let ((arg (argument-spans-of step)))
    (if (pair? arg) arg (variable-spans (step-sub step)))))

;; Spans are keyed by what a form looks like, so a head like `tuple`
;; matches in every pattern that spells it the same way. The step's own
;; span says which one this is.
(define (within-span spans bound)
  (if (not bound)
      spans
      (keep (lambda (span)
              (and (>= (car span) (car bound)) (<= (cdr span) (cdr bound))))
            spans)))

;; A pattern is both sides at once: `(tuple x y)` is where the value being
;; destructured is written AND what demands it be a tuple. Narrowing the
;; demand to the pattern's head keeps the two apart on screen — the word
;; `tuple` requires, the form around it receives.
(define (sup-spans-of step)
  (let* ((reason (step-reason step))
         (tag (and reason (reason-tag reason)))
         (path (step-path step))
         (site (step-site step))
         (head (reason-extra reason 0))
         (lead (and (pair? path) (car path)))
         (index (and (pair? lead) (eq? (car lead) 'domain) (cadr lead)))
         (shape (and (eq? tag 'pattern) (pair? site)
                     (within-span (spans-within (car site) site)
                                  (step-span step)))))
    (cond
     ((and index head site) (spans-within head site))
     ((pair? shape) shape)
     ((and site (step-span step)) (list (step-span step)))
     (else '()))))

;; The token a hop attaches to. Deliberately not the split above: a dot
;; sits where it always has, on the argument or on the step's own form.
(define (hop-spans-of step)
  (let ((arg (argument-spans-of step)))
    (cond
     ((pair? arg) arg)
     ((and (step-site step) (step-span step)) (list (step-span step)))
     (else '()))))

(define (flow-outline diagnostic)
  (let* ((world (diagnostic-world diagnostic))
         (source (and (world? world) (world-source world)))
         (cg (constraint-graph diagnostic))
         (edges (cgraph-edges cg))
         (failing (find-first tedge-failing? edges)))
    (and
     source failing
     (let* ((printer (make-type-printer))
            (brief (brief-printer printer))
            (sink (tedge-from failing))
            (rows (vector '()))     ; (step . sig)
            (counter (vector 0)))

       ;; Rows are keyed by what they display, so a polymorphic callee gets
       ;; one row per distinct instantiation while two calls to the same
       ;; monomorphic builtin share one.
       (define (row-key name slots)
         (let loop ((rest slots) (text name))
           (if (null? rest)
               text
               (loop (cdr rest)
                     (string-append text " " (slot-name (car rest))
                                    "=" (slot-text (car rest)))))))

       (define (row-for step)
         (let* ((named (signature-name step))
                (name (if named (form->string named) "?"))
                (slots (arrow-slots (step-sub step) brief))
                (key (row-key name slots))
                (known (assoc key (vector-ref rows 0))))
           (if known
               (cdr known)
               (let* ((id (begin (vector-set! counter 0
                                              (+ 1 (vector-ref counter 0)))
                                 (string-append "s"
                                                (number->string
                                                 (vector-ref counter 0)))))
                      (row (make-sig id name (step-site step)
                                     (step-span step) slots)))
                 (vector-set! rows 0 (cons (cons key row) (vector-ref rows 0)))
                 row))))

       (define (out-edges node)
         (keep (lambda (e) (eq? (tedge-from e) node)) edges))

       ;; Walk out from the sink; every path that ends at a concrete leaf
       ;; is a flow, reported from that leaf inward.
       (define (paths-from node seen)
         (if (memq node seen)
             '()
             (apply
              append
              (map
               (lambda (edge)
                 (let ((target (tedge-to edge)))
                   (cond
                    ((concrete-leaf? target) (list (list edge)))
                    (else
                     (map (lambda (tail) (cons edge tail))
                          (apply append
                                 (map (lambda (v)
                                        (paths-from v (cons node seen)))
                                      (inner-variables target))))))))
               (out-edges node)))))

       (define (path->flow edge-path)
         (let* ((last-edge (car (reverse edge-path)))
                (origin-type (tedge-to last-edge))
                (origin-step (car (tedge-steps last-edge)))
                (sig-step (signature-step origin-step))
                (row (and sig-step (row-for sig-step)))
                (slot (and sig-step
                           (let ((rest (drop-prefix (step-path sig-step)
                                                    (step-path origin-step))))
                             (if (pair? rest)
                                 (slot-label (car rest))
                                 "result"))))
                ;; reverse so the hops read from the origin inward
                (hops (map (lambda (edge)
                             (let ((step (car (tedge-steps edge))))
                               (make-hop (brief (tedge-from edge))
                                         (step-site step)
                                         (step-span step)
                                         (hop-spans-of step)
                                         (string-append
                                          (brief (step-sub step)) " <: "
                                          (brief (step-sup step)))
                                         (why-phrase step)
                                         (contravariant? (step-path step))
                                         (brief (step-sub step))
                                         (brief (step-sup step))
                                         (shape-parts step brief)
                                         (sub-spans-of step)
                                         (sup-spans-of step))))
                           (reverse edge-path))))
           (and row
                (make-fpath (brief origin-type) (sig-id row) slot
                            (string-append "annotated as "
                                           (quoted (brief origin-type)))
                            hops))))

       ;; Two tuple components can trace the same tokens by the same
       ;; route; that is one arrow, not two.
       (define (flow-key path)
         (let loop ((hops (fpath-hops path))
                    (text (string-append (fpath-type path) "|"
                                         (fpath-signature path) "|"
                                         (or (fpath-slot path) ""))))
           (if (null? hops)
               text
               (loop (cdr hops)
                     (let spans ((rest (hop-spans (car hops))) (text text))
                       (if (null? rest)
                           text
                           (spans (cdr rest)
                                  (string-append
                                   text " " (number->string (caar rest))
                                   "-" (number->string (cdar rest))))))))))

       (define (distinct paths)
         (let loop ((rest paths) (keys '()) (kept '()))
           (cond
            ((null? rest) (reverse kept))
            ((member (flow-key (car rest)) keys) (loop (cdr rest) keys kept))
            (else (loop (cdr rest)
                        (cons (flow-key (car rest)) keys)
                        (cons (car rest) kept))))))

       (let* ((found (distinct (keep (lambda (f) f)
                                     (map path->flow (paths-from sink '())))))
              (sink-step (car (tedge-steps failing))))
         (make-flow source
                    (if (and (tvar? sink) (tvar-source sink))
                        (form->string (tvar-source sink))
                        (brief sink))
                    (hop-spans-of sink-step)
                    (map cdr (reverse (vector-ref rows 0)))
                    found))))))
