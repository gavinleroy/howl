(define-module (lang world)
  #:pure
  #:use-module (scheme base)
  #:export (make-world
            world?
            world-phase
            world-form
            world-scope
            world-env
            world-source
            current-world
            with-world
            with-phase
            make-diagnostic
            diagnostic?
            diagnostic-tag
            diagnostic-data
            diagnostic-world
            diagnostic-expr
            set-diagnostic-expr!
            diagnostic-constraint
            diagnostic-reason
            diagnostic-context
            diagnostic-depth
            diagnostic-path
            diagnostic-span
            diagnostic-step
            blamed-expression
            current-constraint
            set-current-constraint!
            current-expression
            set-current-expression!
            current-span
            set-current-span!
            current-reason
            set-current-reason!
            current-context
            set-current-context!
            current-depth
            set-current-depth!
            current-path
            set-current-path!
            current-step
            set-current-step!
            fail))

(define-record-type <world>
  (make-world world-phase world-form world-scope world-env world-source)
  world?
  (world-phase world-phase)
  (world-form world-form)
  (world-scope world-scope)
  (world-env world-env)
  (world-source world-source))

(define current-world (make-parameter (make-world 'reading #f #f #f "")))

(define (with-world world thunk)
  (parameterize ((current-world world)) (thunk)))

(define (with-phase phase thunk)
  (let ((world (current-world)))
    (with-world (make-world phase
                            (world-form world)
                            (world-scope world)
                            (world-env world)
                            (world-source world))
                thunk)))

(define-record-type <diagnostic>
  (%make-diagnostic diagnostic-tag diagnostic-data diagnostic-world
                    diagnostic-expr diagnostic-constraint diagnostic-reason
                    diagnostic-context diagnostic-depth diagnostic-path
                    diagnostic-span diagnostic-step)
  diagnostic?
  (diagnostic-tag diagnostic-tag)
  (diagnostic-data diagnostic-data)
  (diagnostic-world diagnostic-world)
  (diagnostic-expr diagnostic-expr set-diagnostic-expr!)
  (diagnostic-constraint diagnostic-constraint)
  (diagnostic-reason diagnostic-reason)
  (diagnostic-context diagnostic-context)
  (diagnostic-depth diagnostic-depth)
  (diagnostic-path diagnostic-path)
  (diagnostic-span diagnostic-span)
  (diagnostic-step diagnostic-step))

(define (make-diagnostic tag data world expr)
  (%make-diagnostic tag data world expr (current-constraint) (current-reason)
                    (current-context) (current-depth) (current-path)
                    (current-span) (current-step)))

(define %constraint (vector #f))

(define (current-constraint) (vector-ref %constraint 0))

(define (set-current-constraint! pair) (vector-set! %constraint 0 pair))

(define %expression (vector #f))

(define (current-expression) (vector-ref %expression 0))

(define (set-current-expression! expr) (vector-set! %expression 0 expr))

(define (blamed-expression diagnostic)
  (if (and (> (diagnostic-depth diagnostic) 0)
           (diagnostic-context diagnostic))
      (diagnostic-context diagnostic)
      (diagnostic-expr diagnostic)))

(define %reason (vector #f))

(define (current-reason) (vector-ref %reason 0))

(define (set-current-reason! reason) (vector-set! %reason 0 reason))

(define %context (vector #f))

(define (current-context) (vector-ref %context 0))

(define (set-current-context! form) (vector-set! %context 0 form))

(define %step (vector #f))

(define (current-step) (vector-ref %step 0))

(define (set-current-step! step) (vector-set! %step 0 step))

(define %span (vector #f))

(define (current-span) (vector-ref %span 0))

(define (set-current-span! span) (vector-set! %span 0 span))

(define %path (vector '()))

(define (current-path) (vector-ref %path 0))

(define (set-current-path! path) (vector-set! %path 0 path))

(define %depth (vector 0))

(define (current-depth) (vector-ref %depth 0))

(define (set-current-depth! depth) (vector-set! %depth 0 depth))

(define (fail tag . data)
  (raise (%make-diagnostic tag data (current-world) (current-expression)
                           (current-constraint) (current-reason)
                           (current-context) (current-depth) (current-path)
                           (current-span) (current-step))))
