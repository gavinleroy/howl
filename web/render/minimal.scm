(define-module (render minimal)
  #:pure
  #:use-module (scheme base)
  #:use-module (ice-9 format)
  #:use-module (lang types)
  #:use-module (lang world)
  #:use-module (lang print)
  #:use-module (lang reader)
  #:export (minimal-text))

;; Description ----------------------------------------------------------------
;; A minimal error names the place and lays the essential facts out one per
;; line:
;;
;;   Type error at E
;;   actual: T1
;;   expected: T2
;;
;; Syntax and runtime errors carry only the minimum: the sentence, or the
;; place plus one line of detail.

(define (where-text diagnostic)
  (let ((expr (blamed-expression diagnostic))
        (world (diagnostic-world diagnostic)))
    (cond
     (expr (form->string expr))
     ((and (world? world) (world-form world)) (form->string (world-form world)))
     (else "this program"))))

(define (fact label content)
  (format #f "~a: ~a" label content))

(define (minimal-text diagnostic)
  (let* ((data (diagnostic-data diagnostic))
         (at (lambda (index) (list-ref data index)))
         (printer (make-type-printer))
         (value (lambda (type) ((clean-printer printer) type #t)))
         (demand (lambda (type) ((clean-printer printer) type #f)))
         (place (where-text diagnostic)))
    ;; A report is "<header> at <place>" followed by indented fact lines.
    (define (report header . facts)
      (string-join (cons (format #f "~a at ~a" header place) facts) "\n  "))
    (define (versus actual expected)
      (report "Type error" (fact "actual" actual) (fact "expected" expected)))
    (case (diagnostic-tag diagnostic)
      ((type-mismatch) (versus (value (at 0)) (demand (at 1))))
      ((unsatisfiable-demand) (versus (demand (at 1)) (demand (at 0))))
      ((unresolved-union)
       (versus ((variable-printer printer) (at 0)) (value (at 1))))
      ((type-arity)
       (report "Type error" (fact "type" (at 0))
               (fact "declared" (at 1)) (fact "used" (at 2))))
      ((type-variable-not-allowed)
       (report "Type error"
               (fact "variable not allowed" (format #f "'~a" (at 0)))))
      ((functor-misuse) (report "Type error" (fact "module" (at 0))))
      ((unbound) (report "Scope error" (fact "unbound" (at 0))))
      ((syntax) (format #f "Syntax error: ~a" (at 0)))
      ((division-by-zero) (report "Runtime error" "division by zero"))
      ((incomparable) (report "Runtime error" "incomparable values"))
      ((undefined-during-definition)
       (report "Runtime error" (fact "undefined" (at 0))))
      ((unreachable) (format #f "ICE at ~a" place)))))
