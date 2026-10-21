(define-module (render serialize)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme cxr)
  #:use-module (scheme write)
  #:use-module (lang types)
  #:use-module (lang env)
  #:use-module (lang eval)
  #:use-module (lang world)
  #:use-module (lang print)
  #:use-module (render base)
  #:use-module (render explain)
  #:use-module (render flow)
  #:use-module (render graph)
  #:use-module (render prose)
  #:use-module (render styles)
  #:use-module (render tree)
  #:use-module (lang reader)
  #:export (diagnostic->json))

(define (json-escape text)
  (let ((port (open-output-string)))
    (let loop ((index 0))
      (if (>= index (string-length text))
          (get-output-string port)
          (let ((character (string-ref text index)))
            (write-string
             (cond
              ((char=? character #\") "\\\"")
              ((char=? character #\\) "\\\\")
              ((char=? character #\newline) "\\n")
              ((char=? character #\return) "\\r")
              ((char=? character #\tab) "\\t")
              (else (string character)))
             port)
            (loop (+ index 1)))))))

(define (json-text text) (string-append "\"" (json-escape text) "\""))

(define (json-name name) (json-text (symbol->string name)))

(define (json-number number) (number->string number))

(define (json-boolean value) (if value "true" "false"))

(define (join-json port items)
  (let loop ((items items) (first? #t))
    (if (pair? items)
        (begin
          (if (not first?) (write-string "," port))
          (write-string (car items) port)
          (loop (cdr items) #f)))))

(define (json-array items)
  (let ((port (open-output-string)))
    (write-string "[" port)
    (join-json port items)
    (write-string "]" port)
    (get-output-string port)))

(define (json-object members)
  (let ((port (open-output-string)))
    (write-string "{" port)
    (join-json port
               (map (lambda (member)
                      (string-append (json-text (car member)) ":" (cdr member)))
                    members))
    (write-string "}" port)
    (get-output-string port)))

(define %counter 0)
(define %types 1)
(define %scopes 2)
(define %modules 3)
(define %signatures 4)
(define %values 5)
(define %environments 6)
(define %printer 7)
(define %lines 8)
(define %steps 9)

(define (make-graph)
  (vector 0 '() '() '() '() '() '() (make-type-printer) '() '()))

(define (line-starts text)
  (let loop ((index 0) (starts '()))
    (cond
     ((>= index (string-length text)) (reverse starts))
     ((char=? (string-ref text index) #\newline)
      (loop (+ index 1) (cons (+ index 1) starts)))
     (else (loop (+ index 1) starts)))))

(define (remember-source! graph text)
  (vector-set! graph %lines (line-starts (or text ""))))

(define (place graph offset)
  (let loop ((starts (vector-ref graph %lines)) (line 1) (start 0))
    (cond
     ((null? starts) (cons line (+ 1 (- offset start))))
     ((> (car starts) offset) (cons line (+ 1 (- offset start))))
     (else (loop (cdr starts) (+ line 1) (car starts))))))

(define (next-id! graph prefix)
  (vector-set! graph %counter (+ 1 (vector-ref graph %counter)))
  (string-append prefix (number->string (vector-ref graph %counter))))

(define (intern! graph slot prefix node build)
  (let ((known (find-first (lambda (entry) (eq? (vector-ref entry 0) node))
                           (vector-ref graph slot))))
    (if known
        (vector-ref known 1)
        (let ((entry (vector node (next-id! graph prefix) "null")))
          (vector-set! graph slot (cons entry (vector-ref graph slot)))
          (vector-set! entry 2 (build (vector-ref entry 1)))
          (vector-ref entry 1)))))

(define (table-json graph slot)
  (json-object (map (lambda (entry)
                      (cons (vector-ref entry 1) (vector-ref entry 2)))
                    (reverse (vector-ref graph slot)))))

(define (describe-type graph type)
  ((clean-printer (vector-ref graph %printer)) type #t))

(define (describe-variable graph variable)
  ((variable-printer (vector-ref graph %printer)) variable))

(define (describe-briefly graph type)
  ((brief-printer (vector-ref graph %printer)) type))

(define (describe-cleanly graph type positive?)
  ((clean-printer (vector-ref graph %printer)) type positive?))

(define (type-id graph type)
  (intern! graph %types "t" type
           (lambda (id)
             (json-object
              (append
               (list (cons "id" (json-text id))
                     (cons "text" (json-text (describe-type graph type)))
                     (cons "brief" (json-text (describe-briefly graph type))))
               (cond
                ((tvar? type)
                 (list (cons "kind" (json-text "var"))
                       (cons "level" (json-number (tvar-level type)))
                       (cons "source"
                             (if (tvar-source type)
                                 (link-json/scope graph (tvar-source type) #f
                                                  (tvar-source-span type))
                                 "null"))
                       (cons "lower" (bounds-json graph (tvar-lower type)))
                       (cons "upper" (bounds-json graph (tvar-upper type)))))
                ((prim? type)
                 (list (cons "kind" (json-text "prim"))
                       (cons "name" (json-name (prim-name type)))))
                ((func? type)
                 (list (cons "kind" (json-text "func"))
                       (cons "domains"
                             (json-array
                              (map (lambda (domain)
                                     (json-text (type-id graph domain)))
                                   (func-domains type))))
                       (cons "codomain"
                             (json-text (type-id graph (func-codomain type))))))
                ((record-type? type)
                 (list (cons "kind" (json-text "record"))
                       (cons "fields"
                             (json-array
                              (map (lambda (field)
                                     (json-object
                                      (list (cons "label" (json-name (car field)))
                                            (cons "type"
                                                  (json-text
                                                   (type-id graph (cdr field)))))))
                                   (record-type-fields type))))))
                ((tuple-type? type)
                 (list (cons "kind" (json-text "tuple"))
                       (cons "items"
                             (json-array
                              (map (lambda (item)
                                     (json-text (type-id graph item)))
                                   (tuple-type-items type))))))
                ((variant-type? type)
                 (list (cons "kind" (json-text "variant"))
                       (cons "cases"
                             (json-array
                              (map (lambda (variant)
                                     (json-object
                                      (list (cons "tag" (json-name (car variant)))
                                            (cons "payload"
                                                  (json-array
                                                   (map (lambda (payload)
                                                          (json-text
                                                           (type-id graph payload)))
                                                        (cdr variant)))))))
                                   (variant-type-cases type))))))
                ((nominal? type)
                 (list (cons "kind" (json-text "nominal"))
                       (cons "name"
                             (json-text (display-name (nominal-name type))))
                       (cons "abstract" (json-boolean (not (nominal-body type))))
                       (cons "body"
                             (if (nominal-body type)
                                 (json-text (type-id graph (nominal-body type)))
                                 "null"))))
                (else (list (cons "kind" (json-text "unknown"))))))))))

(define (origin-json graph origin)
  (if (not (origin? origin))
      "null"
      (json-object
       (list (cons "expression"
                   (if (origin-expression origin)
                       (link-json/scope graph (origin-expression origin) #f
                                        (origin-span origin))
                       "null"))
             (cons "left"
                   (if (origin-left origin)
                       (json-text (type-id graph (origin-left origin)))
                       "null"))
             (cons "right"
                   (if (origin-right origin)
                       (json-text (type-id graph (origin-right origin)))
                       "null"))))))

(define (bounds-json graph bounds)
  (json-array
   (map (lambda (bound)
          (json-object
           (list (cons "type" (json-text (type-id graph (bound-type bound))))
                 (cons "origin" (origin-json graph (bound-origin bound))))))
        bounds)))

(define (binding-json graph name entry)
  (let* ((polymorphic (poly? entry))
         (type (if polymorphic (poly-body entry) entry)))
    (json-object
     (list (cons "name" (json-name name))
           (cons "polymorphic" (json-boolean polymorphic))
           (cons "level" (if polymorphic (json-number (poly-level entry)) "null"))
           (cons "type" (json-text (type-id graph type)))
           (cons "text" (json-text (describe-type graph type)))))))

(define (named-type-json graph pair)
  (if (tycon? (cdr pair))
      (json-object
       (list (cons "name" (json-name (car pair)))
             (cons "kind" (json-text "constructor"))
             (cons "parameters"
                   (json-array (map json-name (tycon-params (cdr pair)))))))
      (json-object (list (cons "name" (json-name (car pair)))
                          (cons "type" (json-text (type-id graph (cdr pair))))
                          (cons "text"
                                (json-text (describe-type graph (cdr pair))))))))

(define (form-json form)
  (cond
   ((symbol? form)
    (json-object (list (cons "kind" (json-text "symbol"))
                       (cons "name" (json-name form)))))
   ((string? form)
    (json-object (list (cons "kind" (json-text "string"))
                       (cons "value" (json-text form)))))
   ((boolean? form)
    (json-object (list (cons "kind" (json-text "bool"))
                       (cons "value" (json-boolean form)))))
   ((exact-integer? form)
    (json-object (list (cons "kind" (json-text "int"))
                       (cons "value" (json-number form)))))
   ((number? form)
    (json-object (list (cons "kind" (json-text "float"))
                       (cons "value" (json-number form)))))
   ((null? form)
    (json-object (list (cons "kind" (json-text "list"))
                       (cons "items" "[]"))))
   ((and (pair? form) (proper-list? form))
    (json-object (list (cons "kind" (json-text "list"))
                       (cons "items" (json-array (map form-json form))))))
   (else
    (json-object (list (cons "kind" (json-text "opaque"))
                       (cons "text" (json-text (form->string form))))))))

(define (spans-json graph form) (spans-json/scope graph form #f #f))

(define (spans-json/scope graph form scope exact)
  (json-array
   (map (lambda (span)
          (let ((start (place graph (car span)))
                (end (place graph (cdr span))))
            (json-object (list (cons "from" (json-number (car span)))
                               (cons "to" (json-number (cdr span)))
                               (cons "line" (json-number (car start)))
                               (cons "column" (json-number (cdr start)))
                               (cons "endLine" (json-number (car end)))
                               (cons "endColumn" (json-number (cdr end)))))))
        (if exact (list exact) (spans-within form scope)))))

(define (located-form-json graph form)
  (json-object (list (cons "text" (json-text (form->string form)))
                     (cons "spans" (spans-json graph form))
                     (cons "form" (form-json form)))))

(define (link-json graph form) (link-json/scope graph form #f #f))

(define (link-json/scope graph form scope exact)
  (if form
      (json-object (list (cons "text" (json-text (form->string form)))
                         (cons "spans"
                               (spans-json/scope graph form scope exact))))
      "null"))

(define (constrained-json graph type)
  (if type
      (json-object (list (cons "id" (json-text (type-id graph type)))
                         (cons "text" (json-text (describe-briefly graph type)))))
      "null"))

(define (offsets-json spans)
  (json-array (map (lambda (span)
                     (json-object (list (cons "from" (json-number (car span)))
                                        (cons "to" (json-number (cdr span))))))
                   spans)))

(define (slot-json slot)
  (json-object (list (cons "slot" (json-text (slot-name slot)))
                     (cons "text" (json-text (slot-text slot))))))

(define (signature-json graph row)
  (json-object
   (list (cons "id" (json-text (sig-id row)))
         (cons "name" (json-text (sig-name row)))
         (cons "site" (if (sig-site row)
                          (link-json/scope graph (sig-site row) #f
                                           (sig-span row))
                          "null"))
         (cons "slots" (json-array (map slot-json (sig-slots row)))))))

;; One piece of a hop's structure line. "plain" is scaffolding; "sub" and
;; "sup" are the two sides of the constraint, marked where they sit.
(define (shape-part-json part)
  (json-object (list (cons "text" (json-text (car part)))
                     (cons "role" (json-name (cdr part))))))

(define (hop-json graph hop)
  (json-object
   (list (cons "text" (json-text (hop-text hop)))
         (cons "relation" (json-text (hop-relation hop)))
         (cons "sub" (json-text (hop-sub hop)))
         (cons "sup" (json-text (hop-sup hop)))
         (cons "shape" (json-array (map shape-part-json (hop-shape hop))))
         (cons "subSpans" (offsets-json (hop-sub-spans hop)))
         (cons "supSpans" (offsets-json (hop-sup-spans hop)))
         (cons "why" (json-text (hop-why hop)))
         (cons "contra" (json-boolean (hop-contra? hop)))
         (cons "site" (if (hop-site hop)
                          (link-json/scope graph (hop-site hop) #f
                                           (hop-span hop))
                          "null"))
         (cons "spans" (offsets-json (hop-spans hop))))))

(define (fpath-json graph path)
  (json-object
   (list (cons "type" (json-text (fpath-type path)))
         (cons "signature" (json-text (fpath-signature path)))
         (cons "slot" (json-text (or (fpath-slot path) "result")))
         (cons "origin" (json-text (fpath-origin path)))
         (cons "hops"
               (json-array (map (lambda (hop) (hop-json graph hop))
                                (fpath-hops path)))))))

(define (segment-part-text part)
  (cond ((symbol? part) (symbol->string part))
        ((number? part) (number->string part))
        ((string? part) part)
        (else "")))

(define (path-segment-text segment)
  (if (pair? segment)
      (let loop ((parts segment) (text ""))
        (if (null? parts)
            text
            (loop (cdr parts)
                  (if (string=? text "")
                      (segment-part-text (car parts))
                      (string-append text " "
                                     (segment-part-text (car parts)))))))
      (segment-part-text segment)))

(define (path-text segments)
  (let loop ((segments segments) (text ""))
    (if (null? segments)
        text
        (loop (cdr segments)
              (if (string=? text "")
                  (path-segment-text (car segments))
                  (string-append text " · "
                                 (path-segment-text (car segments))))))))

(define (step-id graph step)
  (intern! graph %steps "s" step
           (lambda (id)
             (let ((origin (step-origin step)))
               (json-object
                (list (cons "id" (json-text id))
                      (cons "sub" (json-text (type-id graph (step-sub step))))
                      (cons "sup" (json-text (type-id graph (step-sup step))))
                      (cons "site"
                            (if (and (origin? origin)
                                     (origin-expression origin))
                                (link-json/scope graph
                                                 (origin-expression origin)
                                                 #f
                                                 (origin-span origin))
                                "null"))
                      (cons "path"
                            (json-array
                             (map (lambda (segment)
                                    (json-text (path-segment-text segment)))
                                  (if (origin? origin)
                                      (or (origin-path origin) '())
                                      '()))))
                      (cons "reason"
                            (if (and (origin? origin) (origin-reason origin))
                                (json-name
                                 (reason-tag (origin-reason origin)))
                                "null"))
                      (cons "left"
                            (if (and (origin? origin) (origin-left origin))
                                (json-text
                                 (type-id graph (origin-left origin)))
                                "null"))
                      (cons "right"
                            (if (and (origin? origin) (origin-right origin))
                                (json-text
                                 (type-id graph (origin-right origin)))
                                "null"))
                      (cons "causes"
                            (json-array
                             (map (lambda (parent)
                                    (json-text (step-id graph parent)))
                                  (keep-steps (step-causes step)))))))))))

(define (cause-json graph cause)
  (json-object
   (list (cons "from" (json-text (step-id graph (cause-parent cause))))
         (cons "to" (json-text (step-id graph (cause-child cause))))
         (cons "path"
               (if (null? (cause-path cause))
                   "null"
                   (json-text (path-text (cause-path cause))))))))

(define (tedge-json graph edge)
  (let ((located (find-first (lambda (step)
                               (let ((origin (step-origin step)))
                                 (and (origin? origin)
                                      (origin-expression origin))))
                             (tedge-steps edge))))
    (json-object
     (list (cons "from" (json-text (type-id graph (tedge-from edge))))
           (cons "to" (json-text (type-id graph (tedge-to edge))))
           (cons "failing" (json-boolean (tedge-failing? edge)))
           (cons "site"
                 (if located
                     (let ((origin (step-origin located)))
                       (link-json/scope graph (origin-expression origin) #f
                                        (origin-span origin)))
                     "null"))
           (cons "steps"
                 (json-array (map (lambda (step)
                                    (json-text (step-id graph step)))
                                  (tedge-steps edge))))))))

(define (constraints-json graph diagnostic)
  (let ((cg (constraint-graph diagnostic)))
    (json-object
     (list (cons "nodes"
                 (json-array (map (lambda (type)
                                    (json-text (type-id graph type)))
                                  (cgraph-nodes cg))))
           (cons "edges"
                 (json-array (map (lambda (edge) (tedge-json graph edge))
                                  (cgraph-edges cg))))
           (cons "steps"
                 (json-array (map (lambda (step)
                                    (json-text (step-id graph step)))
                                  (cgraph-steps cg))))
           (cons "causes"
                 (json-array (map (lambda (cause) (cause-json graph cause))
                                  (cgraph-causes cg))))
           (cons "failing"
                 (if (cgraph-failing cg)
                     (json-text (step-id graph (cgraph-failing cg)))
                     "null"))))))

(define (pnode-json graph node)
  (json-object
   (list (cons "id" (json-text (pnode-id node)))
         (cons "text" (json-text (pnode-text node)))
         (cons "ref"
               (if (pnode-ref node) (json-text (pnode-ref node)) "null"))
         (cons "site"
               (if (pnode-site node)
                   (link-json/scope graph (pnode-site node) #f
                                    (pnode-span node))
                   "null"))
         (cons "marks"
               (json-array
                (map (lambda (mark)
                       (link-json/scope graph (car mark) (cadr mark)
                                        (caddr mark)))
                     (pnode-marks node))))
         (cons "children"
               (json-array (map (lambda (child) (pnode-json graph child))
                               (pnode-children node)))))))

(define (tree-json graph diagnostic)
  (let ((lines (provenance-tree diagnostic)))
    (if lines
        (json-array (map (lambda (node) (pnode-json graph node)) lines))
        "null")))

(define (part-json graph part)
  (cond
   ((string=? (part-kind part) "text")
    (json-object (list (cons "kind" (json-text "text"))
                       (cons "value" (json-text (part-text part))))))
   ((string=? (part-kind part) "code")
    (json-object (list (cons "kind" (json-text "code"))
                       (cons "value" (json-text (part-text part)))
                       (cons "spans" (if (part-form part)
                                         (spans-json graph (part-form part))
                                         "[]")))))
   (else
    (json-object
     (list (cons "kind" (json-text "type"))
           (cons "id" (json-text (type-id graph (part-type part))))
           (cons "value"
                 (json-text (if (part-positive? part)
                                (describe-cleanly graph (part-type part) #t)
                                (describe-briefly graph (part-type part))))))))))

(define (parts-json graph parts)
  (json-array (map (lambda (part) (part-json graph part)) parts)))

(define (sentence-json graph sentence)
  (json-object
   (list (cons "parts" (parts-json graph (sentence-parts sentence)))
         (cons "site" (link-json/scope graph (sentence-site sentence)
                                       (sentence-scope sentence)
                                       (sentence-span sentence)))
         (cons "type" (constrained-json graph (sentence-type sentence)))
         (cons "children"
               (json-array (map (lambda (child) (sentence-json graph child))
                                (sentence-children sentence)))))))

(define (passage-json graph passage)
  (json-object
   (list (cons "line" (parts-json graph (passage-line passage)))
         (cons "type" (constrained-json graph (passage-type passage)))
         (cons "sentences"
               (json-array (map (lambda (sentence)
                                  (sentence-json graph sentence))
                                (passage-sentences passage)))))))

(define (prose-json graph diagnostic)
  (let ((story (prose-outline diagnostic)))
    (if (not (prose? story))
        "null"
        (json-object
         (list (cons "value" (link-json/scope graph (prose-value story)
                                              (prose-scope story)
                                              (prose-value-span story)))
               (cons "expected" (passage-json graph (prose-expected story)))
               (cons "actual" (passage-json graph (prose-actual story))))))))

(define (flow-json graph diagnostic)
  (let ((outline (flow-outline diagnostic)))
    (if (not (flow? outline))
        "null"
        (json-object
         (list (cons "source" (json-text (flow-source outline)))
               (cons "sink"
                     (json-object
                      (list (cons "label" (json-text (flow-sink outline)))
                            (cons "spans"
                                  (offsets-json (flow-sink-spans outline))))))
               (cons "signatures"
                     (json-array (map (lambda (row) (signature-json graph row))
                                      (flow-signatures outline))))
               (cons "flows"
                     (json-array (map (lambda (path) (fpath-json graph path))
                                      (flow-paths outline)))))))))

(define (signature-id graph signature)
  (intern! graph %signatures "g" signature
           (lambda (id)
             (json-object
              (list (cons "id" (json-text id))
                    (cons "kind" (json-text "signature"))
                    (cons "items"
                          (json-array (map (lambda (item)
                                             (located-form-json graph item))
                                           (signature-items signature))))
                    (cons "scope"
                          (json-text (scope-id graph
                                               (signature-scope signature)))))))))

(define (module-id graph entry)
  (intern! graph %modules "m" entry
           (lambda (id)
             (if (modtype? entry)
                 (json-object
                  (list (cons "id" (json-text id))
                        (cons "kind" (json-text "module"))
                        (cons "types"
                              (json-array (map (lambda (pair)
                                                 (named-type-json graph pair))
                                               (modtype-types entry))))
                        (cons "values"
                              (json-array
                               (map (lambda (pair)
                                      (binding-json graph (car pair) (cdr pair)))
                                    (modtype-values entry))))
                        (cons "modules"
                              (json-array
                               (map (lambda (pair)
                                      (json-object
                                       (list (cons "name" (json-name (car pair)))
                                             (cons "module"
                                                   (json-text
                                                    (module-id graph
                                                               (cdr pair)))))))
                                    (modtype-modules entry))))))
                 (json-object
                  (list (cons "id" (json-text id))
                        (cons "kind" (json-text "functor"))
                        (cons "parameter" (json-name (functor-param entry)))
                        (cons "signature"
                              (json-text (signature-id graph
                                                       (functor-signature entry))))
                        (cons "body" (located-form-json graph (functor-body entry)))
                        (cons "scope"
                              (json-text (scope-id graph
                                                   (functor-scope entry))))))))))

(define (scope-id graph scope)
  (intern! graph %scopes "e" scope
           (lambda (id)
             (json-object
              (list (cons "id" (json-text id))
                    (cons "kind" (json-text "scope"))
                    (cons "types"
                          (json-array (map (lambda (pair)
                                             (named-type-json graph pair))
                                           (scope-types scope))))
                    (cons "values"
                          (json-array
                           (map (lambda (pair)
                                  (binding-json graph (car pair) (cdr pair)))
                                (scope-values scope))))
                    (cons "modules"
                          (json-array
                           (map (lambda (pair)
                                  (json-object
                                   (list (cons "name" (json-name (car pair)))
                                         (cons "module"
                                               (json-text
                                                (module-id graph (cdr pair)))))))
                                (scope-modules scope))))
                    (cons "signatures"
                          (json-array
                           (map (lambda (pair)
                                  (json-object
                                   (list (cons "name" (json-name (car pair)))
                                         (cons "signature"
                                               (json-text
                                                (signature-id graph
                                                              (cdr pair)))))))
                                (scope-signatures scope)))))))))

(define (value-id graph value)
  (intern! graph %values "v" value
           (lambda (id)
             (json-object
              (append
               (list (cons "id" (json-text id))
                     (cons "text" (json-text (value->string value))))
               (cond
                ((boolean? value)
                 (list (cons "kind" (json-text "bool"))
                       (cons "value" (json-boolean value))))
                ((exact-integer? value)
                 (list (cons "kind" (json-text "int"))
                       (cons "value" (json-number value))))
                ((number? value)
                 (list (cons "kind" (json-text "float"))
                       (cons "value" (json-number value))))
                ((string? value)
                 (list (cons "kind" (json-text "string"))
                       (cons "value" (json-text value))))
                ((tuple-value? value)
                 (list (cons "kind" (json-text "tuple"))
                       (cons "items"
                             (json-array
                              (map (lambda (item)
                                     (json-text (value-id graph item)))
                                   (tuple-value-items value))))))
                ((record-value? value)
                 (list (cons "kind" (json-text "record"))
                       (cons "fields"
                             (json-array
                              (map (lambda (field)
                                     (json-object
                                      (list (cons "label" (json-name (car field)))
                                            (cons "value"
                                                  (json-text
                                                   (value-id graph (cdr field)))))))
                                   (record-value-fields value))))))
                ((variant? value)
                 (list (cons "kind" (json-text "variant"))
                       (cons "tag" (json-name (variant-name value)))
                       (cons "values"
                             (json-array (map (lambda (payload)
                                                (json-text
                                                 (value-id graph payload)))
                                              (variant-values value))))))
                ((closure? value)
                 (list (cons "kind" (json-text "closure"))
                       (cons "parameters"
                             (json-array (map (lambda (param)
                                                (json-text (form->string param)))
                                              (closure-params value))))
                       (cons "body" (located-form-json graph (closure-body value)))
                       (cons "environment"
                             (json-text (environment-id graph
                                                        (closure-env value))))))
                ((primitive? value)
                 (list (cons "kind" (json-text "primitive"))
                       (cons "name" (json-name (primitive-name value)))
                       (cons "arity" (json-number (primitive-arity value)))
                       (cons "applied"
                             (json-number (length (primitive-args value))))))
                ((module-value? value)
                 (list (cons "kind" (json-text "module"))
                       (cons "bindings"
                             (json-array
                              (map (lambda (binding)
                                     (json-object
                                      (list (cons "name" (json-name (car binding)))
                                            (cons "value"
                                                  (json-text
                                                   (value-id graph
                                                             (cdr binding)))))))
                                   (module-value-bindings value))))
                       (cons "modules"
                             (json-array
                              (map (lambda (binding)
                                     (json-object
                                      (list (cons "name" (json-name (car binding)))
                                            (cons "value"
                                                  (json-text
                                                   (value-id graph
                                                             (cdr binding)))))))
                                   (module-value-modules value))))))
                ((functor-value? value)
                 (list (cons "kind" (json-text "functor"))
                       (cons "parameter" (json-name (functor-value-param value)))
                       (cons "body" (located-form-json graph (functor-value-body value)))
                       (cons "environment"
                             (json-text (environment-id
                                         graph (functor-value-env value))))))
                (else (list (cons "kind" (json-text "unknown"))))))))))

(define (environment-id graph environment)
  (intern! graph %environments "r" environment
           (lambda (id)
             (json-object
              (list (cons "id" (json-text id))
                    (cons "kind" (json-text "environment"))
                    (cons "values"
                          (json-array
                           (map (lambda (pair)
                                  (json-object
                                   (list (cons "name" (json-name (car pair)))
                                         (cons "value"
                                               (json-text
                                                (value-id
                                                 graph
                                                 (vector-ref (cdr pair) 0)))))))
                                (renv-values environment))))
                    (cons "modules"
                          (json-array
                           (map (lambda (pair)
                                  (json-object
                                   (list (cons "name" (json-name (car pair)))
                                         (cons "value"
                                               (json-text
                                                (value-id graph (cdr pair)))))))
                                (renv-modules environment)))))))))

(define (view-json graph node)
  (let ((kind (car node)))
    (cond
     ((eq? kind 'text)
      (json-object (list (cons "kind" (json-text "text"))
                         (cons "value" (json-text (cadr node))))))
     ((eq? kind 'code)
      (json-object (list (cons "kind" (json-text "code"))
                         (cons "value" (json-text (cadr node))))))
     ((eq? kind 'type) (encode-kind graph 'type (cadr node)))
     ((eq? kind 'brief)
      (json-object (list (cons "kind" (json-text "type"))
                         (cons "id" (json-text (type-id graph (cadr node))))
                         (cons "text" (json-text (describe-briefly graph (cadr node))))
                         (cons "brief" "true"))))
     ((eq? kind 'variable)
      (json-object (list (cons "kind" (json-text "variable"))
                         (cons "name" (json-text (describe-variable graph (cadr node))))
                         (cons "id" (json-text (type-id graph (cadr node)))))))
     ((eq? kind 'expression) (encode-kind graph 'form (cadr node)))
     ((eq? kind 'value) (encode-kind graph 'value (cadr node)))
     ((eq? kind 'paragraph)
      (json-object (list (cons "kind" (json-text "paragraph"))
                         (cons "children" (view-array graph (cdr node))))))
     ((eq? kind 'note)
      (json-object (list (cons "kind" (json-text "note"))
                         (cons "children" (view-array graph (cdr node))))))
     ((eq? kind 'section)
      (json-object (list (cons "kind" (json-text "section"))
                         (cons "title" (json-text (cadr node)))
                         (cons "children" (view-array graph (cddr node))))))
     ((or (eq? kind 'bullets) (eq? kind 'steps))
      (json-object (list (cons "kind" (json-name kind))
                         (cons "items" (view-array graph (cdr node))))))
     ((eq? kind 'table)
      (json-object
       (list (cons "kind" (json-text "table"))
             (cons "columns" (json-array (map json-text (cadr node))))
             (cons "rows"
                   (json-array (map (lambda (row) (view-array graph row))
                                    (caddr node)))))))
     (else (json-object (list (cons "kind" (json-text "unknown"))))))))

(define (view-array graph nodes)
  (json-array (map (lambda (node) (view-json graph node)) nodes)))

(define (type-node? datum)
  (or (tvar? datum) (prim? datum) (func? datum) (record-type? datum)
      (variant-type? datum) (nominal? datum)))

(define (encode-kind graph kind datum)
  (cond
   ((eq? kind 'type)
    (json-object (list (cons "kind" (json-text "type"))
                       (cons "id" (json-text (type-id graph datum)))
                       (cons "text" (json-text (describe-type graph datum))))))
   ((eq? kind 'name)
    (json-object (list (cons "kind" (json-text "name"))
                       (cons "value" (json-name datum)))))
   ((eq? kind 'count)
    (json-object (list (cons "kind" (json-text "count"))
                       (cons "value" (json-number datum)))))
   ((eq? kind 'text)
    (json-object (list (cons "kind" (json-text "text"))
                       (cons "value" (json-text datum)))))
   ((eq? kind 'scope)
    (json-object (list (cons "kind" (json-text "scope"))
                       (cons "id" (json-text (scope-id graph datum))))))
   ((eq? kind 'value)
    (json-object (list (cons "kind" (json-text "value"))
                       (cons "id" (json-text (value-id graph datum)))
                       (cons "text" (json-text (value->string datum))))))
   ((eq? kind 'form)
    (json-object (list (cons "kind" (json-text "form"))
                       (cons "text" (json-text (form->string datum)))
                       (cons "form" (form-json datum)))))
   (else (encode-datum graph datum))))

(define (encode-datum graph datum)
  (cond
   ((type-node? datum) (encode-kind graph 'type datum))
   ((scope? datum) (encode-kind graph 'scope datum))
   ((symbol? datum) (encode-kind graph 'name datum))
   ((string? datum) (encode-kind graph 'text datum))
   ((and (number? datum) (exact-integer? datum)) (encode-kind graph 'count datum))
   ((pair? datum) (encode-kind graph 'form datum))
   (else (encode-kind graph 'value datum))))

(define (encode-fields graph tag data)
  (json-object
   (let loop ((schema (diagnostic-schema tag)) (members '()))
     (if (null? schema)
         (reverse members)
         (let ((field (car schema)))
           (if (< (caddr field) (length data))
               (loop (cdr schema)
                     (cons (cons (car field)
                                 (encode-kind graph
                                              (cadr field)
                                              (list-ref data (caddr field))))
                           members))
               (loop (cdr schema) members)))))))

(define (world-json graph world)
  (if (not (world? world))
      "null"
      (json-object
       (list (cons "phase" (json-name (world-phase world)))
             (cons "form"
                   (if (world-form world)
                       (located-form-json graph (world-form world))
                       "null"))
             (cons "scope"
                   (if (scope? (world-scope world))
                       (json-text (scope-id graph (world-scope world)))
                       "null"))
             (cons "environment"
                   (if (renv? (world-env world))
                       (json-text (environment-id graph (world-env world)))
                       "null"))
             (cons "source" (json-text (or (world-source world) "")))))))

(define (diagnostic->json diagnostic)
  (let* ((graph (make-graph))
         (world (diagnostic-world diagnostic))
         (ignored (remember-source! graph
                                    (and (world? world) (world-source world))))
         (expr (diagnostic-expr diagnostic))
         (fields (encode-fields graph
                                (diagnostic-tag diagnostic)
                                (diagnostic-data diagnostic)))
         (data (json-array (map (lambda (datum) (encode-datum graph datum))
                                (diagnostic-data diagnostic))))
         (expression (if expr (located-form-json graph expr) "null"))
         (explanation (view-json graph (explain diagnostic)))
         (renderings (json-object
                      (map (lambda (style)
                             (cons (car style) (json-text (cdr style))))
                           (rendering-styles diagnostic))))
         (constraint (diagnostic-constraint diagnostic))
         (state (world-json graph world)))
    (json-object
     (list (cons "tag" (json-name (diagnostic-tag diagnostic)))
           (cons "phase"
                 (if (world? world) (json-name (world-phase world)) "null"))
           (cons "fields" fields)
           (cons "data" data)
           (cons "expression" expression)
           (cons "explanation" explanation)
           (cons "renderings" renderings)
           (cons "flow" (flow-json graph diagnostic))
           (cons "constraints" (constraints-json graph diagnostic))
           (cons "tree" (tree-json graph diagnostic))
           (cons "prose" (prose-json graph diagnostic))
           (cons "constraint"
                 (if constraint
                     (json-object
                      (list (cons "left"
                                  (json-text (type-id graph (car constraint))))
                            (cons "right"
                                  (json-text (type-id graph (cdr constraint))))))
                     "null"))
           (cons "world" state)
           (cons "graph"
                 (json-object
                  (list (cons "types" (table-json graph %types))
                        (cons "steps" (table-json graph %steps))
                        (cons "scopes" (table-json graph %scopes))
                        (cons "modules" (table-json graph %modules))
                        (cons "signatures" (table-json graph %signatures))
                        (cons "values" (table-json graph %values))
                        (cons "environments"
                              (table-json graph %environments)))))))))
