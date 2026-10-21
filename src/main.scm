(use-modules (hoot ffi)
             (lang program)
             (render serialize))

(define-foreign register!
  "app" "register" (ref string) (ref null extern) -> none)

(define-foreign report-transcript!
  "app" "transcript" (ref string) -> none)

(define-foreign report-diagnostic!
  "app" "diagnostic" (ref string) -> none)

(define (run-source source)
  (let ((result (run-program source)))
    (report-transcript! (result-transcript result))
    (for-each (lambda (diagnostic)
                (report-diagnostic! (diagnostic->json diagnostic)))
              (result-diagnostics result))))

(register! "run" (procedure->external run-source))
