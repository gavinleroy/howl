(use-modules (scheme base))

(define-record-type <spec>
  (make-spec spec-items spec-scope)
  spec?
  (spec-items spec-items)
  (spec-scope spec-scope))

(define-record-type <result>
  (make-result result-types result-modules)
  result?
  (result-types result-types)
  (result-modules result-modules))

(define (view spec table)
  (let loop ((items (spec-items spec))
             (types '())
             (modules '()))
    (if (null? items)
        (list (reverse types) (reverse modules))
        (let ((item (car items)))
          (cond
           ((eq? (car item) 'module)
            (let* ((name (cadr item))
                   (nested (make-spec '() '()))
                   (entry (if table
                              (cdr (assq name (result-modules table)))
                              (abstract nested))))
              (when table (check table nested))
              (loop (cdr items) types (cons (cons name entry) modules))))
           (else (loop (cdr items) types modules)))))))

(define (abstract spec)
  (let ((v (view spec #f)))
    (make-result (car v) (cadr v))))

(define (check table spec)
  (view spec table))

(define (seal table spec)
  (check table spec)
  (abstract spec))

(length (result-types (seal (make-result '() '())
                            (make-spec (list (list 'val 'f)) '()))))
