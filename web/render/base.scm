(define-module (render base)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme cxr)
  #:use-module (lang types)
  #:use-module (lang env)
  #:use-module (lang world)
  #:use-module (render explain)
  #:use-module (lang print)
  #:use-module (lang reader)
  #:export (seen? remember!
            bounds-variables keep-bounded
            concrete?
            trace meeting-point
            wrap-type
            reason-tag reason-extra))

(define (seen? table item)
  (let loop ((items (vector-ref table 0)))
    (cond ((null? items) #f)
          ((eq? (car items) item) #t)
          (else (loop (cdr items))))))

(define (remember! table item)
  (vector-set! table 0 (cons item (vector-ref table 0))))

(define (variables-of type table found)
  (cond
   ((or (not type) (seen? table type)) found)
   (else
    (remember! table type)
    (cond
     ((tvar? type)
      (let ((deeper (append found (list type))))
        (bounds-variables (append (bound-types (tvar-lower type))
                                  (bound-types (tvar-upper type)))
                          table deeper)))
     ((func? type)
      (variables-of (func-codomain type) table
                    (bounds-variables (func-domains type) table found)))
     ((record-type? type)
      (bounds-variables (map cdr (record-type-fields type)) table found))
     ((tuple-type? type)
      (bounds-variables (tuple-type-items type) table found))
     ((variant-type? type)
      (bounds-variables (apply append (map cdr (variant-type-cases type)))
                        table found))
     (else found)))))

(define (bounds-variables types table found)
  (if (null? types)
      found
      (bounds-variables (cdr types) table
                        (variables-of (car types) table found))))

(define (bounded? variable)
  (or (pair? (tvar-lower variable)) (pair? (tvar-upper variable))))

(define (keep-bounded variables)
  (let loop ((variables variables) (kept '()))
    (cond
     ((null? variables) (reverse kept))
     ((bounded? (car variables)) (loop (cdr variables) (cons (car variables) kept)))
     (else (loop (cdr variables) kept)))))

(define (concrete? type) (and type (not (tvar? type))))

(define (located? bound)
  (let ((origin (bound-origin bound)))
    (and (origin? origin) (origin-expression origin) #t)))

(define (pick-bound bounds)
  (or (find-first (lambda (bound)
                    (and (concrete? (bound-type bound)) (located? bound)))
                  bounds)
      (find-first (lambda (bound) (concrete? (bound-type bound))) bounds)
      (find-first located? bounds)
      (if (pair? bounds) (car bounds) #f)))

(define (trace variable downward? seen)
  (if (seen? seen variable)
      '()
      (begin
        (remember! seen variable)
        (let ((bound (pick-bound (if downward?
                                     (tvar-lower variable)
                                     (tvar-upper variable)))))
          (cond
           ((not bound) '())
           ((concrete? (bound-type bound)) (list bound))
           (else (cons bound (trace (bound-type bound) downward? seen))))))))

(define (holds? bounds type)
  (any-of (lambda (bound) (eq? (bound-type bound) type)) bounds))

(define (meeting-point variables actual expected)
  (or (find-first (lambda (variable)
                    (and (holds? (tvar-lower variable) actual)
                         (holds? (tvar-upper variable) expected)))
                  variables)
      (find-first (lambda (variable)
                    (or (holds? (tvar-lower variable) actual)
                        (holds? (tvar-upper variable) expected)))
                  variables)
      (find-first (lambda (variable)
                    (and (pair? (tvar-lower variable))
                         (pair? (tvar-upper variable))))
                  variables)
      (if (pair? variables) (car variables) #f)))

(define (wrap-type text)
  (if (and (> (string-length text) 0) (char=? (string-ref text 0) #\())
      text
      (string-append "(" text ")")))

(define (reason-tag reason)
  (cond ((pair? reason) (car reason))
        (else reason)))

(define (reason-extra reason index)
  (let loop ((parts (if (pair? reason) (cdr reason) '())) (index index))
    (cond ((null? parts) #f)
          ((= index 0) (car parts))
          (else (loop (cdr parts) (- index 1))))))
