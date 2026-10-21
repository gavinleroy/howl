(define-module (render terse)
  #:pure
  #:use-module (scheme base)
  #:use-module (ice-9 format)
  #:use-module (lang world)
  #:export (terse-text))

;; Description ----------------------------------------------------------------
;; A terse error says nothing other than "a XXX error occurred". Instead of
;; `XXX` we specify the category of error that occurs, but nothing else.


(define (terse-text diagnostic)
  (format #f "a~a error occurred"
          (case (diagnostic-tag diagnostic)
            ((syntax) " syntax")
            ((division-by-zero incomparable undefined-during-definition) 
             " runtime")
            ((unbound type-mismatch unresolved-union unsatisfiable-demand
                            type-arity type-variable-not-allowed functor-misuse)
             " type")
            ((unreachable) "n ICE"))))
