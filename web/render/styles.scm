(define-module (render styles)
  #:pure
  #:use-module (scheme base)
  #:use-module (render terse)
  #:use-module (render minimal)
  #:export (rendering-styles))

(define (rendering-styles diagnostic)
  (list (cons "terse" (terse-text diagnostic))
        (cons "minimal" (minimal-text diagnostic))))
