(define-module (render tree)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme cxr)
  #:use-module (lang types)
  #:use-module (lang world)
  #:use-module (lang print)
  #:use-module (render base)
  #:use-module (render explain)
  #:use-module (render graph)
  #:export (provenance-tree
            pnode? pnode-id pnode-text pnode-site pnode-span pnode-ref
            pnode-marks pnode-children))

;; The provenance tree, read as expectations and requirements: the root
;; names the conflicting subject, its two children state the established
;; and the conflicting type, and every deeper line alternates between a
;; claim about a named thing ("`x` is inferred to have type `float`") and
;; the requirement that produced it ("because it's used as the first
;; argument to ..."), recurring through unannotated definitions until an
;; annotated type or an already-shown line ends the chain.

;; A line: text, the source site, an id, `ref` — unused now that every
;; line states its own facts rather than pointing at another line — and
;; `marks`, source links (form scope exact-span) the renderer highlights
;; in the editor on hover.
(define-record-type <pnode>
  (make-pnode pnode-id pnode-text pnode-site pnode-span pnode-ref
              pnode-marks pnode-children)
  pnode?
  (pnode-id pnode-id)
  (pnode-text pnode-text)
  (pnode-site pnode-site)
  (pnode-span pnode-span)
  (pnode-ref pnode-ref)
  (pnode-marks pnode-marks)
  (pnode-children pnode-children))

;; Line ids: a counter, so every line has a handle of its own.
(define (make-ids) (vector 0))

(define (next-id! ids)
  (vector-set! ids 0 (+ 1 (vector-ref ids 0)))
  (string-append "p" (number->string (vector-ref ids 0))))

;; Keys whose lines are currently being expanded, innermost first. A key
;; met again while it is open is a cycle — a recursive definition that
;; would explain itself forever — and its line is printed without
;; children. A key met on a different branch is not a cycle: it is the
;; same fact needed in two places, and a reader following one branch
;; should not have to go and find the other.
(define (make-open) (vector '()))

(define (open? open key) (and (memq key (vector-ref open 0)) #t))

(define (expand open key build)
  (vector-set! open 0 (cons key (vector-ref open 0)))
  (let ((children (build)))
    (vector-set! open 0 (cdr (vector-ref open 0)))
    children))

(define (keep pred items)
  (let loop ((items items) (kept '()))
    (cond ((null? items) (reverse kept))
          ((pred (car items)) (loop (cdr items) (cons (car items) kept)))
          (else (loop (cdr items) kept)))))

(define (last-of items)
  (cond ((null? items) #f)
        ((null? (cdr items)) (car items))
        (else (last-of (cdr items)))))

(define (head-form form)
  (if (and (pair? form) (proper-list? form)) (car form) form))

(define (tick text) (string-append "`" text "`"))

(define (ordinal n)
  (case n
    ((1) "first") ((2) "second") ((3) "third") ((4) "fourth") ((5) "fifth")
    (else (string-append (number->string n) "th"))))

(define (step-reason step)
  (let ((origin (step-origin step)))
    (and (origin? origin) (origin-reason origin))))

(define (step-path step)
  (let ((origin (step-origin step)))
    (if (origin? origin) (or (origin-path origin) '()) '())))

(define (step-site step)
  (let ((origin (step-origin step)))
    (and (origin? origin) (origin-expression origin))))

(define (step-span step)
  (let ((origin (step-origin step)))
    (and (origin? origin) (origin-span origin))))

;; A usage step: the constraint recording that a value flowed into some
;; argument position of an application.
(define (domain-tail step)
  (let ((tail (last-of (step-path step))))
    (and (pair? tail) (eq? (car tail) 'domain) tail)))

(define (usage-step? step)
  (and (eq? (reason-tag (step-reason step)) 'application)
       (domain-tail step)))

(define (upper-steps variable)
  (keep-steps (map bound-step (tvar-upper variable))))

;; The application constraint a usage step decomposed from: the nearest
;; cause with an application reason and a shorter path.
(define (app-of step)
  (let ((depth (length (step-path step))))
    (let loop ((queue (keep-steps (step-causes step))) (visited '()))
      (cond
       ((null? queue) #f)
       ((memq (car queue) visited) (loop (cdr queue) visited))
       ((and (eq? (reason-tag (step-reason (car queue))) 'application)
             (< (length (step-path (car queue))) depth))
        (car queue))
       (else (loop (append (cdr queue)
                           (keep-steps (step-causes (car queue))))
                   (cons (car queue) visited)))))))

;; Every step recorded behind a type: the bounds on each variable it
;; mentions. Empty means the type was declared, not inferred.
(define (steps-behind type)
  (let ((variables (keep-bounded (bounds-variables (list type)
                                                   (vector '()) '()))))
    (keep-steps
     (apply append
            (map (lambda (variable)
                   (map bound-step (append (tvar-lower variable)
                                           (tvar-upper variable))))
                 variables)))))

;; The variables sitting directly inside a pattern's type, in order.
(define (direct-tvars type)
  (cond
   ((tvar? type) (list type))
   ((tuple-type? type)
    (keep tvar? (tuple-type-items type)))
   ((record-type? type)
    (keep tvar? (map cdr (record-type-fields type))))
   ((variant-type? type)
    (keep tvar? (apply append (map cdr (variant-type-cases type)))))
   ((func? type)
    (keep tvar? (append (func-domains type) (list (func-codomain type)))))
   (else '())))

(define (provenance-tree diagnostic)
  (let* ((tag (diagnostic-tag diagnostic))
         (data (diagnostic-data diagnostic))
         (at (lambda (index) (and (< index (length data))
                                  (list-ref data index))))
         (failing (let ((step (diagnostic-step diagnostic)))
                    (and (step? step) step)))
         (printer (make-type-printer))
         (clean (clean-printer printer))
         (brief (brief-printer printer))
         ;; Claims about pieces of a definition are printed in the scope of
         ;; that definition's whole type, so a variable is named the same
         ;; way in a claim as in the header above it — which is what makes
         ;; the equalities between positions readable. The scope follows
         ;; the tree down: it changes whenever we descend into a callee.
         (scope (vector clean))
         (ids (make-ids))
         (open (make-open))
         (bindings (bindings-of diagnostic)))

    ;; A line and the children it would expand into, unless expanding
    ;; them would re-enter a line already open above this one.
    (define (line key text site span marks build)
      (make-pnode (next-id! ids) text site span #f marks
                  (if (open? open key) '() (expand open key build))))

    ;; because it's used as the Nth argument to `SITE`
    (define (usage-line step)
      (let* ((tail (domain-tail step))
             (site (step-site step))
             (site-text (if site (form->string site) "?"))
             (text (if (and tail (pair? (cdr tail)))
                       (string-append "because it's used as the "
                                      (ordinal (cadr tail))
                                      " argument to " (tick site-text))
                       (string-append "because it's used in "
                                      (tick site-text))))
             (marks (usage-marks step)))
        (line step text (step-site step) (step-span step) marks
              (lambda ()
                (let ((app (app-of step)))
                  (if app (list (head-claim app)) '()))))))

    ;; The variable's declaration and its occurrences inside the site.
    (define (usage-marks step)
      (let ((sub (step-sub step))
            (site (step-site step)))
        (if (and (tvar? sub) (tvar-source sub))
            (append (list (list (tvar-source sub) #f (tvar-source-span sub)))
                    (if site (list (list (tvar-source sub) site #f)) '()))
            (if site (list (list site #f (step-span step))) '()))))

    ;; because it's applied to N arguments in `SITE`
    (define (call-line step)
      (let* ((reason (step-reason step))
             (site (step-site step))
             (args (reason-extra reason 1))
             (text (string-append
                    "because it's applied to "
                    (number->string (if (proper-list? args)
                                        (length args) 1))
                    " argument(s) in "
                    (tick (if site (form->string site) "?")))))
        (make-pnode (next-id! ids) text site (step-span step) #f
                    (if site (list (list site #f (step-span step))) '())
                    '())))

    ;; A binding held as a variable resolves through the definition that
    ;; flowed into it (its function-shaped lower bound).
    (define (resolve-binding type)
      (if (tvar? type)
          (let ((found (find-first (lambda (bound) (func? (bound-type bound)))
                                   (tvar-lower type))))
            (if found (bound-type found) type))
          type))

    ;; Every concrete bound a variable reaches, following variable-valued
    ;; bounds through so that intermediates — which never appear in the
    ;; printed type — stay hidden. Carries the step that introduced each
    ;; concrete endpoint, which is what the where-line points at.
    (define (reachable-bounds variable lower?)
      (let ((seen (vector '()))
            (found (vector '())))
        (let walk ((v variable))
          (unless (seen? seen v)
            (remember! seen v)
            (for-each
             (lambda (bound)
               (let ((type (bound-type bound)))
                 (if (tvar? type)
                     (walk type)
                     (vector-set! found 0
                                  (cons (cons type (bound-step bound))
                                        (vector-ref found 0))))))
             (if lower? (tvar-lower v) (tvar-upper v)))))
        (reverse (vector-ref found 0))))

    ;; A step's ancestor at a given path: the comparison this one was
    ;; derived from, at the position asked for. Used to find the
    ;; argument-level comparison behind a failure deep inside an argument.
    (define (step-at-path step wanted)
      (let loop ((queue (list step)) (seen '()))
        (cond
         ((null? queue) #f)
         ((memq (car queue) seen) (loop (cdr queue) seen))
         ((equal? (step-path (car queue)) wanted) (car queue))
         (else (loop (append (cdr queue) (keep-steps (step-causes (car queue))))
                     (cons (car queue) seen))))))

    ;; because `-.` has type `(-> (float float) float)`
    ;;
    ;; What pinned a variable down. A bound recorded inside one of the
    ;; call's arguments is really a fact about that argument, so the line
    ;; names the argument and the type it brought — rather than restating
    ;; the bound against a variable, which says where the constraint
    ;; landed but never where it came from.
    (define (reason-line type step within)
      (let* ((site (and (step? step) (step-site step)))
             ;; Only a bound recorded inside this very call is a fact
             ;; about one of its arguments. One recorded elsewhere — in
             ;; the callee's own body, say — is a fact about the
             ;; expression it was recorded at, and naming an argument
             ;; position there would point at another call's arguments.
             (here (and site within (eq? site within)))
             (segment (and here (leading-domain (step-path step))))
             (arg (and segment (argument-form step)))
             (arg-step (and arg (step-at-path step (list segment))))
             (subject (cond (arg (form->string arg))
                            (site (form->string site))
                            (else #f)))
             (shown (if arg-step (clean (step-sub arg-step) #t) (brief type)))
             (marks (cond ((and arg site) (list (list arg site #f)))
                          (site (list (list site #f (step-span step))))
                          (else '()))))
        (and subject
             (make-pnode (next-id! ids)
                         (string-append "because " (tick subject)
                                        " has type " (tick shown))
                         site (and (step? step) (step-span step)) #f marks '()))))

    ;; One line per distinct thing that pinned a variable, in the order
    ;; the variables appear in the signature. Only the bounds that agree
    ;; with what the variable stands for are reasons for it: a bound that
    ;; disagrees is the error being reported, not an explanation of the
    ;; instantiation, and listing it here would have the line contradict
    ;; the type above it.
    (define (reason-lines app-step variables)
      (let ((within (step-site app-step)))
        (let loop ((rest variables) (texts '()) (lines '()))
          (if (null? rest)
              (reverse lines)
              (let* ((stands-for (witness (car rest)))
                     (shown (and stands-for (brief stands-for))))
                (let inner ((facts (append (reachable-bounds (car rest) #f)
                                           (reachable-bounds (car rest) #t)))
                            (texts texts)
                            (lines lines))
                  (if (null? facts)
                      (loop (cdr rest) texts lines)
                      (let* ((type (caar facts))
                             (agrees (and shown (string=? (brief type) shown)))
                             (line (and agrees
                                        (reason-line type (cdar facts)
                                                     within))))
                        (cond
                         ((not line) (inner (cdr facts) texts lines))
                         ((member (pnode-text line) texts)
                          (inner (cdr facts) texts lines))
                         (else (inner (cdr facts)
                                      (cons (pnode-text line) texts)
                                      (cons line lines))))))))))))

    ;; `f` has type `T` (inferred: recur) / `f` is annotated with type `T`,
    ;; printed as the declared shape with its instantiation stated beneath.
    (define (head-claim app-step)
      (let* ((reason (step-reason app-step))
             ;; Whose type this line is about. A step inside `(domain N)`
             ;; is about the Nth argument, not about the callee — and the
             ;; type printed below comes from that step's own subject, so
             ;; naming the callee here would caption one function's type
             ;; with another function's name.
             (head (or (argument-form app-step)
                       (reason-extra reason 0)
                       (head-form (step-site app-step))))
             (name (and (symbol? head) head))
             (entry (and name (assq name bindings)))
             (btype (resolve-binding
                     (cond ((and entry (poly? (cdr entry)))
                            (poly-body (cdr entry)))
                           (entry (cdr entry))
                           (else (step-sub app-step)))))
             (behind (steps-behind btype))
             (shown (((bare-printer printer) btype #t) btype #t))
             (head-text (form->string head))
             (text (string-append
                    (tick head-text)
                    (if (null? behind)
                        " is annotated with type "
                        " has type ")
                    (tick (car shown))))
             (site (step-site app-step))
             (marks (if site (list (list head site #f)) '())))
        (line btype text site (step-span app-step) marks
              (lambda ()
                (append (instantiation-lines app-step (cdr shown))
                        (if (null? behind) '() (definition-children btype)))))))

    ;; What a variable turned out to stand for. A position's requirement
    ;; is what the call had to satisfy, so an upper bound is preferred;
    ;; a variable only ever flowed into reads from its lower one.
    (define (witness variable)
      (let ((upper (reachable-bounds variable #f))
            (lower (reachable-bounds variable #t)))
        (cond ((pair? upper) (caar upper))
              ((pair? lower) (caar lower))
              (else #f))))

    ;; The type with every variable replaced by what it stands for here.
    ;; Rebuilding the type and handing it to the printer keeps one
    ;; rendering of types in the program: no second spelling to drift.
    ;; Types can be recursive, so the walk is depth-bounded.
    (define (instantiate type depth)
      (cond
       ((<= depth 0) type)
       ((tvar? type) (or (witness type) type))
       ((func? type)
        (make-func (map (lambda (d) (instantiate d (- depth 1)))
                        (func-domains type))
                   (instantiate (func-codomain type) (- depth 1))))
       ((tuple-type? type)
        (make-tuple-type (map (lambda (i) (instantiate i (- depth 1)))
                              (tuple-type-items type))))
       ((record-type? type)
        (make-record-type
         (map (lambda (field)
                (cons (car field) (instantiate (cdr field) (- depth 1))))
              (record-type-fields type))))
       ((variant-type? type)
        (make-variant-type
         (map (lambda (variant)
                (cons (car variant)
                      (map (lambda (p) (instantiate p (- depth 1)))
                           (cdr variant))))
              (variant-type-cases type))))
       ;; Nominals are left as they are: `make-nominal` takes only a name,
       ;; so rebuilding one would drop the builder and body it carries.
       (else type)))

    ;; A polymorphic signature says what the function accepts in general;
    ;; what matters at a call is the one copy made for it. State that
    ;; copy, and hang the facts that pinned it underneath. A signature
    ;; with no variables has nothing to instantiate and gets no line.
    (define (instantiation-lines app-step variables)
      (if (null? variables)
          '()
          (let ((text (string-append
                       "instantiated with type "
                       (tick (brief (instantiate (step-sub app-step) 12)))))
                (site (step-site app-step)))
            (list (make-pnode (next-id! ids) text site (step-span app-step) #f
                              '() (reason-lines app-step variables))))))

    ;; because its Nth argument is destructured into `PAT`, then a claim
    ;; per variable the pattern introduced. Everything below is about this
    ;; definition, so its type becomes the printing scope.
    (define (definition-children arrow)
      (if (not (func? arrow))
          '()
          (let ((was (vector-ref scope 0)))
            (vector-set! scope 0 ((scoped-printer printer) arrow #t))
            (let ((lines (definition-lines arrow)))
              (vector-set! scope 0 was)
              lines))))

    (define (definition-lines arrow)
      (let loop ((domains (func-domains arrow)) (index 1) (lines '()))
        (if (null? domains)
            (reverse lines)
            (loop (cdr domains) (+ index 1)
                  (let ((domain (car domains)))
                    (if (tvar? domain)
                        (let ((found (pattern-step domain)))
                          (cond
                           (found
                            (cons (destructure-line found index) lines))
                           ((pair? (tvar-upper domain))
                            (cons (var-claim domain) lines))
                           (else lines)))
                        lines))))))

    (define (pattern-step variable)
      (let loop ((steps (upper-steps variable)))
        (cond ((null? steps) #f)
              ((eq? (reason-tag (step-reason (car steps))) 'pattern)
               (car steps))
              (else (loop (cdr steps))))))

    (define (destructure-line step index)
      (let* ((reason (step-reason step))
             (pattern (or (reason-extra reason 0) (step-site step)))
             (text (string-append "because its " (ordinal index)
                                  " argument is destructured into "
                                  (tick (form->string pattern))))
             (site (step-site step))
             (marks (if site (list (list site #f (step-span step))) '())))
        (make-pnode (next-id! ids) text site (step-span step) #f marks
                    (map var-claim (direct-tvars (step-sup step))))))

    ;; because it's matched against cases `Nil`, `Cons` — then, per case
    ;; payload, the pattern that gave it shape
    (define (scrutinee-line step)
      (let* ((sup (step-sup step))
             (cases (if (variant-type? sup) (variant-type-cases sup) '()))
             (text (string-append
                    "because it's matched against cases "
                    (string-join (map (lambda (case)
                                        (tick (symbol->string (car case))))
                                      cases)
                                 ", ")))
             (site (step-site step))
             (marks (if site (list (list site #f (step-span step))) '())))
        (line step text site (step-span step) marks
              (lambda ()
                (apply append
                       (map (lambda (case) (case-children case)) cases))))))

    ;; `Cons` carries `(tuple f s)`
    (define (case-children case)
      (apply append
             (map (lambda (payload)
                    (if (tvar? payload)
                        (let ((found (pattern-step payload)))
                          (cond
                           (found (list (case-line (car case) found)))
                           ((pair? (tvar-upper payload))
                            (list (var-claim payload)))
                           (else '())))
                        '()))
                  (cdr case))))

    (define (case-line tag step)
      (let* ((reason (step-reason step))
             (pattern (or (reason-extra reason 0) (step-site step)))
             (text (string-append (tick (symbol->string tag)) " carries "
                                  (tick (form->string pattern))))
             (site (step-site step))
             (marks (if site (list (list site #f (step-span step))) '())))
        (make-pnode (next-id! ids) text site (step-span step) #f marks
                    (map var-claim (direct-tvars (step-sup step))))))

    ;; because it's the result of the `PAT` clause / the `then` branch
    (define (result-line step kind)
      (let* ((reason (step-reason step))
             (which (reason-extra reason 0))
             (text (string-append
                    "because it's the result of the "
                    (if which (string-append (tick (form->string which)) " ")
                        "")
                    kind))
             (site (step-site step))
             (marks (if site (list (list site #f (step-span step))) '())))
        (make-pnode (next-id! ids) text site (step-span step) #f marks '())))

    ;; every explainable requirement recorded on a variable
    (define (bound-line step)
      (let ((tag (reason-tag (step-reason step))))
        (cond
         ((usage-step? step) (usage-line step))
         ((eq? tag 'scrutinee) (scrutinee-line step))
         ((eq? tag 'clause) (result-line step "clause"))
         ((eq? tag 'branch) (result-line step "branch"))
         (else #f))))

    ;; `x` is inferred to have type `T`
    (define (var-claim variable)
      (let* ((subject (if (tvar-source variable)
                          (form->string (tvar-source variable))
                          (clean variable #t)))
             (text (string-append (tick subject)
                                  " is inferred to have type "
                                  (tick ((vector-ref scope 0) variable #f))))
             (marks (if (tvar-source variable)
                        (list (list (tvar-source variable) #f
                                    (tvar-source-span variable)))
                        '())))
        (line variable text #f #f marks
              (lambda ()
                (let loop ((steps (upper-steps variable)) (lines '()))
                  (cond
                   ((null? steps) (reverse lines))
                   ((bound-line (car steps))
                    => (lambda (found) (loop (cdr steps) (cons found lines))))
                   (else (loop (cdr steps) lines))))))))

    ;; The conflicting subject, and the usage chain of the failing demand.
    (define (conflict-variable)
      (and failing (tvar? (step-sub failing)) (step-sub failing)))

    ;; A path reads outside in: its first segment is a position in the
    ;; call being checked, and the rest descend inside that position. So
    ;; `(domain 2) (item 1)` is the first component of the second
    ;; argument, and `(domain 1) (domain 1)` is the first parameter of the
    ;; function passed as the first argument.
    (define (leading-domain path)
      (and (pair? path) (pair? (car path)) (eq? (caar path) 'domain)
           (car path)))

    ;; The failure is recorded at the deepest comparison that failed, so
    ;; its own path already names the argument it came through — there is
    ;; nothing to search for. What the causes are needed for is the
    ;; argument-level comparison, the one whose path is that segment
    ;; alone: `(tuple int int)` against the second parameter, rather than
    ;; `int` against `float` inside it.
    ;;
    ;; Walking the causes to look for an argument position instead would
    ;; be wrong: a failure is reached from both sides, and one of those
    ;; sides runs through the parameter of a function-typed argument,
    ;; which is where the requirement was established rather than where it
    ;; was violated.
    (define (blame-step)
      (let ((segment (and (step? failing) (leading-domain (step-path failing)))))
        (and segment
             (let loop ((queue (list failing)) (seen '()))
               (cond
                ((null? queue) #f)
                ((memq (car queue) seen) (loop (cdr queue) seen))
                ((equal? (step-path (car queue)) (list segment)) (car queue))
                (else (loop (append (cdr queue)
                                    (keep-steps (step-causes (car queue))))
                            (cons (car queue) seen))))))))

    ;; The argument a step is about: the one its leading `(domain N)`
    ;; names.
    (define (argument-form step)
      (let* ((reason (step-reason step))
             (args (reason-extra reason 1))
             (segment (leading-domain (step-path step)))
             (index (and segment (pair? (cdr segment)) (cadr segment))))
        (and index (proper-list? args) (<= index (length args))
             (list-ref args (- index 1)))))

    (define (subject-text)
      (let ((variable (conflict-variable)))
        (cond
         ((and variable (tvar-source variable))
          (form->string (tvar-source variable)))
         ((and failing (step-site failing))
          (form->string (head-form (step-site failing))))
         (else "this expression"))))

    (define (subject-marks)
      (let ((variable (conflict-variable)))
        (if (and variable (tvar-source variable))
            (list (list (tvar-source variable) #f
                        (tvar-source-span variable)))
            '())))

    ;; Provenance of the established side: the usage that recorded the
    ;; conflicting bound when the subject is a variable, the definition's
    ;; own structure when it is a function.
    (define (established-children type)
      (let ((variable (conflict-variable)))
        (cond
         ((and variable
               (find-first (lambda (bound) (eq? (bound-type bound) type))
                           (tvar-upper variable)))
          => (lambda (bound)
               (if (step? (bound-step bound))
                   (list (usage-line (bound-step bound)))
                   '())))
         ((func? type) (definition-children type))
         ((tvar? type) (definition-children type))
         (else '()))))

    ;; Provenance of the conflicting demand: the failing step's own usage,
    ;; or the nearest application among its causes.
    (define (demand-children)
      (cond
       ((not failing) '())
       ((usage-step? failing) (list (usage-line failing)))
       (else
        (let loop ((queue (list failing)) (visited '()))
          (cond
           ((null? queue) '())
           ((memq (car queue) visited) (loop (cdr queue) visited))
           ((and (eq? (reason-tag (step-reason (car queue))) 'application)
                 (not (usage-step? (car queue))))
            (list (call-line (car queue))))
           ((usage-step? (car queue)) (list (usage-line (car queue))))
           (else (loop (append (cdr queue)
                               (keep-steps (step-causes (car queue))))
                       (cons (car queue) visited))))))))

    (define (branch label type polarity children)
      (make-pnode (next-id! ids)
                  (string-append label " " (tick (clean type polarity)))
                  #f #f #f '() children))

    (define (conflict-root established-label established established-polarity
                           demand-label demand demand-polarity)
      (list
       (make-pnode (next-id! ids)
                   (string-append "Type conflict for "
                                  (tick (subject-text)))
                   (and failing (step-site failing))
                   (and failing (step-span failing))
                   #f (subject-marks)
                   (list (branch established-label established
                                 established-polarity
                                 (established-children established))
                         (branch demand-label demand demand-polarity
                                 (demand-children))))))

    ;; One shape for both tags. They differ in how inference noticed the
    ;; failure, not in what it is: in each, the first datum is the type
    ;; that just arrived and the second is what was already established —
    ;; `fail 'type-mismatch sub sup`, `fail 'unsatisfiable-demand new
    ;; existing` — so both read as inferred against expected. The types
    ;; come from the blamed step rather than from the data, which is
    ;; recorded at the depth the comparison failed and so can name a
    ;; component rather than the argument it sits in.
    (define (error-root step)
      (let* ((arg (argument-form step))
             (site (step-site step))
             (text (string-append
                    "Type error at "
                    (tick (form->string (or arg site)))))
             (marks (cond ((and arg site) (list (list arg site #f)))
                          (site (list (list site #f (step-span step))))
                          (else '())))
             ;; Two ways to arrive here. Either the expression carries a
             ;; type of its own and this position rejects it — then the
             ;; step's own two sides are the pair to show, and the usage
             ;; explains the demand. Or the expression is a variable being
             ;; constrained, and the step says only what this use wants of
             ;; it; the type it already had is the other recorded demand,
             ;; and the usage explains the new one.
             (constrains-variable (tvar? (step-sub step))))
        (list
         (make-pnode
          (next-id! ids) text site (step-span step) #f marks
          (if constrains-variable
              (list (branch "it's inferred to have type" (at 0) #f
                            (list (usage-line step)))
                    (branch "but it's expected to have type" (at 1) #f
                            (established-children (at 1))))
              (list (branch "it's inferred to have type" (step-sub step) #t
                            '())
                    (branch "but it's expected to have type" (step-sup step) #f
                            (list (usage-line step)))))))))

    (cond
     ((blame-step) => error-root)
     ;; No argument position to blame — keep naming the conflicting
     ;; subject until the provenance chains are rebuilt underneath.
     ((eq? tag 'unsatisfiable-demand)
      (conflict-root "it's expected to have type" (at 1) #f
                     "but it's inferred type is" (at 0) #f))
     ((eq? tag 'type-mismatch)
      (conflict-root "it's inferred to have type" (at 0) #t
                     "but it's expected to have type" (at 1) #f))
     (else #f))))
