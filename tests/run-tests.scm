(use-modules (lang program))

(set-port-encoding! (current-output-port) "UTF-8")

(define failures 0)

(define (report-failure name expected actual)
  (set! failures (+ failures 1))
  (display "FAIL ") (display name) (newline)
  (display "  expected: ") (write expected) (newline)
  (display "  actual:   ") (write actual) (newline))

(define (report-success name)
  (display "ok   ") (display name) (newline))

(define (check name expected actual)
  (if (equal? expected actual)
      (report-success name)
      (report-failure name expected actual)))

(define (run source)
  (let ((result (run-program-text source)))
    (string-append (if (car result) "" "FAILED: ") (cdr result))))

(define (last-line text)
  (let loop ((index (- (string-length text) 1)))
    (cond ((< index 0) text)
          ((char=? (string-ref text index) #\newline)
           (substring text (+ index 1) (string-length text)))
          (else (loop (- index 1))))))

(define (run-last source)
  (last-line (run source)))

(check "integer" "- : int = 42" (run "42"))
(check "float" "- : float = 2.5" (run "2.5"))
(check "string" "- : string = \"hi\"" (run "\"hi\""))
(check "boolean" "- : bool = #t" (run "#t"))
(check "integer arithmetic" "- : int = 20" (run "(* (+ 2 3) 4)"))
(check "integer division" "- : int = 3" (run "(/ 7 2)"))
(check "float arithmetic" "- : float = 7.5" (run "(*. 2.5 3.0)"))
(check "string concatenation" "- : string = \"ab\"" (run "(^ \"a\" \"b\")"))
(check "conversion" "- : string = \"3\"" (run "(string-of-int 3)"))
(check "and is short circuit" "- : bool = #f" (run "(and #f #t)"))

(check "identity is polymorphic" "- : (-> ('a) 'a) = <fun>" (run "(fun (x) x)"))
(check "an unused parameter is unconstrained" "- : (-> ('a) int) = <fun>"
       (run "(fun (x) 1)"))
(check "a produced result only" "- : (-> ((-> (int) 'a)) 'a) = <fun>"
       (run "(fun (f) (f 1))"))
(check "self application types" "- : (-> ((∩ 'a (-> ('a) 'b))) 'b) = <fun>"
       (run "(fun (f) (f f))"))
(check "twice has a principal subtyping type"
       "- : (-> ((-> ((∪ 'a 'b)) (∩ 'c 'a)) 'b) 'c) = <fun>"
       (run "(fun (f x) (f (f x)))"))
(check "curried application" "- : int = 7"
       (run-last "(define (add a b) (+ a b)) (add 3 4)"))
(check "partial application does not typecheck"
       #f
       (car (run-program-text "(define (add a b) (+ a b)) ((add 3) 4)")))
(check "let generalizes" "- : (record (a int) (b string)) = { a = 1; b = \"s\" }"
       (run "(let ((id (fun (v) v))) (record (a (id 1)) (b (id \"s\"))))"))
(check "recursion" "- : int = 3628800"
       (run-last "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) (fact 10)"))
(check "let is parallel" "- : int = 1"
       (run "(let ((x 1)) (let ((x 2) (y x)) y))"))

(check "mixed branches do not typecheck"
       #f
       (car (run-program-text "(fun (b) (if b 1 \"s\"))")))
(check "a mixed union cannot flow on"
       #f
       (car (run-program-text "(let ((v (if #t 1 \"s\"))) (if (= v 1) \"n\" \"s\"))")))

(check "record literals are structural"
       "- : (record (x int) (y float)) = { x = 1; y = 2.0 }"
       (run "(record (x 1) (y 2.0))"))
(check "field access needs only the field it uses"
       "- : (-> ((record (x 'a))) 'a) = <fun>"
       (run "(fun (p) p.x)"))
(check "field access form" "- : int = 1"
       (run "(dot (record (x 1) (y 2)) x)"))
(check "width subtyping" "- : int = 1"
       (run-last "(define (get-x p) p.x) (get-x (record (x 1) (y 2.0)))"))
(check "a record declaration names a structure"
       "type point = (record (x int) (y float))"
       (run "(type point (record (x int) (y float)))"))
(check "a declared record accepts a matching literal" "- : int = 3"
       (run-last "(type point (record (x int) (y float)))
                  (define (sum p) (+ p.x (int-of-float p.y)))
                  (sum (record (x 1) (y 2.0)))"))

(check "constructors need no declaration"
       "- : (enum (Circle float)) = Circle 1.5"
       (run "(Circle 1.5)"))
(check "a nullary constructor is a value" "- : (enum Origin) = Origin"
       (run "Origin"))
(check "match gives a closed variant"
       "val area : (-> ((enum (Circle float) (Rect float float))) float) = <fun>"
       (run "(define (area s)
               (match s
                 ((Circle r) (*. r r))
                 ((Rect w h) (*. w h))))"))
(check "a smaller variant flows into a bigger match" "- : float = 4.0"
       (run-last "(define (area s)
                    (match s
                      ((Circle r) (*. r r))
                      ((Rect w h) (*. w h))))
                  (area (Circle 2.0))"))
(check "an enum declaration names a variant"
       "type shape = (enum (Circle float) Origin (Rect float float))"
       (run "(type shape (enum (Circle float) (Rect float float) Origin))"))
(check "wildcard clause" "- : int = 1"
       (run "(match (Circle 1.0) ((Circle r) 1) (_ 0))"))
(check "recursion infers a recursive type"
       "val total : (-> ((rec 'a (enum Leaf (Node 'a int 'a)))) int) = <fun>"
       (run "(define (total t)
               (match t
                 (Leaf 0)
                 ((Node l v r) (+ (total l) (+ v (total r))))))"))
(check "recursive values compute" "- : int = 6"
       (run-last "(define (total t)
                    (match t
                      (Leaf 0)
                      ((Node l v r) (+ (total l) (+ v (total r))))))
                  (total (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)))"))

(check "module signature is inferred"
       "module M : sig val x : int; val twice : (-> ((-> ((∪ 'a 'b)) (∩ 'c 'a)) 'b) 'c) end"
       (run "(module M (struct (define x 1) (define (twice f v) (f (f v)))))"))
(check "module value access" "- : int = 2"
       (run-last "(module M (struct (define (twice f v) (f (f v)))))
                  (M.twice (fun (n) (+ n 1)) 0)"))
(check "module type declaration" "module type ORD"
       (run "(module-type ORD (sig (type t) (val compare (-> (t t) int))))"))
(check "sealing hides the representation"
       "module Counter : sig type t; val start : Counter.t; val bump : (-> (Counter.t) Counter.t); val value : (-> (Counter.t) int) end"
       (run-last "(module-type COUNTER
                    (sig (type t) (val start t) (val bump (-> (t) t)) (val value (-> (t) int))))
                  (module Counter : COUNTER
                    (struct (type t int)
                            (define start 0)
                            (define (bump c) (+ c 1))
                            (define (value c) c)))"))
(check "values cross the abstraction boundary" "- : int = 2"
       (run-last "(module-type COUNTER
                    (sig (type t) (val start t) (val bump (-> (t) t)) (val value (-> (t) int))))
                  (module Counter : COUNTER
                    (struct (type t int)
                            (define start 0)
                            (define (bump c) (+ c 1))
                            (define (value c) c)))
                  (Counter.value (Counter.bump (Counter.bump Counter.start)))"))
(check "a manifest type stays transparent"
       "module IntOrd : sig type t = int; val compare : (-> (int int) int) end"
       (run-last "(module-type ORD (sig (type t int) (val compare (-> (t t) int))))
                  (module IntOrd : ORD
                    (struct (type t int) (define (compare a b) (- a b))))"))
(check "inline signature" "module M : sig val x : int end"
       (run "(module M : (sig (val x int)) (struct (define x 1) (define y 2)))"))
(check "signature matching allows a more general value"
       "module M : sig val id : (-> (int) int) end"
       (run "(module M : (sig (val id (-> (int) int))) (struct (define (id v) v)))"))

(check "functor definition"
       "functor MakeBounds (M : ORD) -> sig val largest : (-> ((∩ 'a M.t) (∩ 'a M.t)) 'a) end"
       (run-last "(module-type ORD (sig (type t) (val compare (-> (t t) int))))
                  (functor MakeBounds (M : ORD)
                    (struct (define (largest a b) (if (< (M.compare a b) 0) b a))))"))
(check "functor application" "- : int = 7"
       (run-last "(module-type ORD (sig (type t int) (val compare (-> (t t) int))))
                  (module IntOrd : ORD
                    (struct (type t int) (define (compare a b) (- a b))))
                  (functor MakeBounds (M : ORD)
                    (struct (define (largest a b) (if (< (M.compare a b) 0) b a))))
                  (module IntBounds (MakeBounds IntOrd))
                  (IntBounds.largest 3 7)"))
(check "functor result follows the argument"
       "module StringBounds : sig val largest : (-> ((∩ 'a string) (∩ 'a string)) 'a) end"
       (run-last "(module-type ORD (sig (type t string) (val compare (-> (t t) int))))
                  (module StringOrd : ORD
                    (struct (type t string)
                            (define (compare a b) (if (< a b) (- 0 1) 1))))
                  (functor MakeBounds (M : ORD)
                    (struct (define (largest a b) (if (< (M.compare a b) 0) b a))))
                  (module StringBounds (MakeBounds StringOrd))"))

(check "a signature can describe a sub-module"
       "module B : sig module Key; val largest : (-> (B.Key.t B.Key.t) B.Key.t) end"
       (run-last "(module-type ORD
                    (sig (type t) (val of-int (-> (int) t)) (val compare (-> (t t) int))))
                  (module-type BOUNDS
                    (sig (module Key : ORD) (val largest (-> (Key.t Key.t) Key.t))))
                  (module B : BOUNDS
                    (struct
                      (module Key : ORD
                        (struct (type t int)
                                (define (of-int n) n)
                                (define (compare a b) (- a b))))
                      (define (largest a b) (if (< (Key.compare a b) 0) b a))))"))
(check "values reach through a nested module" "- : B.Key.t = 7"
       (run-last "(module-type ORD
                    (sig (type t) (val of-int (-> (int) t)) (val compare (-> (t t) int))))
                  (module-type BOUNDS
                    (sig (module Key : ORD) (val largest (-> (Key.t Key.t) Key.t))))
                  (module B : BOUNDS
                    (struct
                      (module Key : ORD
                        (struct (type t int)
                                (define (of-int n) n)
                                (define (compare a b) (- a b))))
                      (define (largest a b) (if (< (Key.compare a b) 0) b a))))
                  (B.largest (B.Key.of-int 3) (B.Key.of-int 7))"))
(check "a nested spec may take parameters"
       "- : (Boxed.Inner.t int) = Cons 42 Nil"
       (run-last "(module-type CONTAINER
                    (sig (type t 'a) (val empty (t 'a)) (val add (-> ('a (t 'a)) (t 'a)))))
                  (module-type BOX
                    (sig (module Inner : CONTAINER) (val single (-> ('a) (Inner.t 'a)))))
                  (module Boxed : BOX
                    (struct
                      (module Inner : CONTAINER
                        (struct (type t 'a (list 'a))
                                (define empty Nil)
                                (define (add x xs) (Cons x xs))))
                      (define (single x) (Inner.add x Inner.empty))))
                  (Boxed.single 42)"))

(define (diagnostics-of source)
  (result-diagnostics (run-program source)))

(check "checking stops at the first failure"
       1
       (length (diagnostics-of "(area \"circle\")
                                (define x (+ 1 2))
                                (totl 3)")))
(check "the transcript keeps the declarations before the failure"
       "val x : int = 3\n- : int = 4"
       (result-transcript (run-program "(define x (+ 1 2))
                                        (+ x 1)
                                        (area \"circle\")")))

(check "tuples carry their parts positionally"
       "- : (tuple int string bool) = (1, \"a\", #t)"
       (run-last "(tuple 1 \"a\" #t)"))
(check "a tuple pattern names each part"
       "- : (tuple string int) = (\"x\", 1)"
       (run-last "(define (swap (tuple a b)) (tuple b a)) (swap (tuple 1 \"x\"))"))
(check "let takes a tuple apart"
       "- : int = 5"
       (run-last "(let (((tuple f s) (tuple 2 3))) (+ f s))"))
(check "match takes a tuple apart"
       "- : int = 3"
       (run-last "(match (tuple 1 2) ((tuple a b) (+ a b)))"))
(check "tuple patterns nest"
       "- : int = 6"
       (run-last "(define (nest (tuple a (tuple b c))) (+ a (+ b c)))
                  (nest (tuple 1 (tuple 2 3)))"))
(check "tuple types are written like tuple values"
       "val pair : (tuple int string) = (1, \"a\")"
       (run "(define pair (tuple 1 \"a\"))"))
(check "tuples are compared part by part"
       "- : bool = #t"
       (run "(= (tuple 1 (tuple 2 3)) (tuple 1 (tuple 2 3)))"))
(check "tuples in a union collapse into one"
       "- : (tuple int string) = (1, \"a\")"
       (run-last "(if #t (tuple 1 \"a\") (tuple 2 \"b\"))"))

(check "several declarations"
       "val x : int = 1\nval y : int = 2\n- : int = 3"
       (run "(define x 1) (define y 2) (+ x y)"))
(check "empty program" "nothing to do" (run "   "))

(check "std is not opened by default"
       #f
       (car (run-program-text "(list-map (fun (x) x) Nil)")))
(check "std lists map and fold" "- : int = 12"
       (run-last "(define xs (Std.List.range 1 4))
                  (Std.List.fold + 0 (Std.List.map (fun (n) (* n 2)) xs))"))
(check "std list filter keeps the matching elements" "- : bool = #t"
       (run "(= (Std.List.filter (fun (n) (< n 2)) (Std.List.range 0 4))
                (Cons 0 (Cons 1 Nil)))"))
(check "std list append and reverse" "- : bool = #t"
       (run "(= (Std.List.reverse (Std.List.append (Cons 1 Nil) (Cons 2 Nil)))
                (Cons 2 (Cons 1 Nil)))"))
(check "std list sort" "- : bool = #t"
       (run "(= (Std.List.sort (Cons 3 (Cons 1 (Cons 2 Nil))))
                (Cons 1 (Cons 2 (Cons 3 Nil))))"))
(check "std list zip pairs elements" "- : int = 3"
       (run "(Std.Option.value 0
               (Std.Option.map Std.Pair.second
                 (Std.List.head (Std.List.zip (Cons 1 Nil) (Cons 3 Nil)))))"))
(check "std list find" "- : int = 4"
       (run "(Std.Option.value 0
               (Std.List.find (fun (n) (> n 3)) (Std.List.range 0 9)))"))

(check "std option map over Some" "- : int = 3"
       (run "(Std.Option.value 0 (Std.Option.map (fun (n) (+ n 1)) (Some 2)))"))
(check "std option value over None" "- : int = 0"
       (run "(Std.Option.value 0 None)"))
(check "std option bind" "- : bool = #t"
       (run "(Std.Option.is-none (Std.Option.bind (fun (n) None) (Some 1)))"))

(check "std result map over Ok" "- : int = 3"
       (run "(Std.Result.value 0 (Std.Result.map (fun (n) (+ n 1)) (Ok 2)))"))
(check "std result bind short circuits" "- : bool = #t"
       (run "(Std.Result.is-error
              (Std.Result.bind (fun (n) (Err \"boom\")) (Ok 1)))"))
(check "std results and options convert" "- : bool = #t"
       (run "(Std.Option.is-some (Std.Result.to-option (Ok 1)))"))

(check "std maps insert and find" "- : string = \"two\""
       (run-last "(define m (Std.Map.insert 2 \"two\"
                              (Std.Map.insert 1 \"one\" Std.Map.empty)))
                  (Std.Option.value \"\" (Std.Map.find 2 m))"))
(check "std maps insert replaces" "- : int = 2"
       (run-last "(define m (Std.Map.insert \"k\" 2
                              (Std.Map.insert \"k\" 1 Std.Map.empty)))
                  (Std.Option.value 0 (Std.Map.find \"k\" m))"))
(check "std maps stay balanced and complete" "- : bool = #t"
       (run-last "(define m (Std.List.fold (fun (t k) (Std.Map.insert k k t))
                                           Std.Map.empty
                                           (Std.List.range 0 20)))
                  (and (= (Std.Map.size m) 20)
                       (Std.List.for-all (fun (k) (Std.Map.member k m))
                                         (Std.List.range 0 20)))"))
(check "std maps remove" "- : bool = #t"
       (run-last "(define m (Std.List.fold (fun (t k) (Std.Map.insert k k t))
                                           Std.Map.empty
                                           (Std.List.range 0 20)))
                  (define m2 (Std.List.fold (fun (t k) (Std.Map.remove k t))
                                            m
                                            (Std.List.filter
                                             (fun (k) (= (mod k 2) 0))
                                             (Std.List.range 0 20))))
                  (and (= (Std.Map.size m2) 10)
                       (and (not (Std.Map.member 4 m2))
                            (Std.Map.member 5 m2)))"))
(check "std maps iterate in key order" "- : bool = #t"
       (run-last "(define m (Std.Map.of-list
                             (Cons (Pair 3 \"c\")
                                   (Cons (Pair 1 \"a\") (Cons (Pair 2 \"b\") Nil)))))
                  (= (Std.Map.keys m) (Cons 1 (Cons 2 (Cons 3 Nil))))"))

(check "string-code-at inside the string" "- : int = 104"
       (run "(match (string-code-at \"hi\" 0) ((Some c) c) (None 0))"))
(check "string-code-at out of range" "- : bool = #t"
       (run "(match (string-code-at \"hi\" 5) ((Some c) #f) (None #t))"))
(check "string-of-code" "- : string = \"A\"" (run "(string-of-code 65)"))
(check "string-of-code handles wide characters" "- : int = 1"
       (run "(string-length (string-of-code 955))"))

(check "std string sub" "- : string = \"ell\""
       (run "(Std.Str.sub 1 3 \"hello\")"))
(check "std string split and join" "- : string = \"a-b-c\""
       (run "(Std.Str.join \"-\" (Std.Str.split \",\" \"a,b,c\"))"))
(check "std string split keeps empty pieces" "- : int = 3"
       (run "(Std.List.length (Std.Str.split \",\" \"a,,b\"))"))
(check "std string contains" "- : bool = #t"
       (run "(Std.Str.contains \"ell\" \"hello\")"))
(check "std string index" "- : int = 2"
       (run "(Std.Option.value 9 (Std.Str.index-of \"ll\" \"hello\"))"))
(check "std string prefixes and suffixes" "- : bool = #t"
       (run "(and (Std.Str.starts-with \"he\" \"hello\")
                  (Std.Str.ends-with \"lo\" \"hello\"))"))
(check "std string case" "- : string = \"HELLO\""
       (run "(Std.Str.uppercase \"Hello\")"))
(check "std string trim" "- : string = \"hi\""
       (run "(Std.Str.trim \"  hi  \")"))
(check "std string reverse" "- : string = \"cba\""
       (run "(Std.Str.reverse \"abc\")"))
(check "std string repeat" "- : string = \"ababab\""
       (run "(Std.Str.repeat 3 \"ab\")"))
(check "std parses integers" "- : int = -42"
       (run "(Std.Option.value 0 (Std.Str.to-int \"-42\"))"))
(check "std rejects malformed integers" "- : bool = #t"
       (run "(Std.Option.is-none (Std.Str.to-int \"4x2\"))"))

(check "std arrays index" "- : int = 30"
       (run-last "(define a (Std.Array.of-list (Cons 10 (Cons 20 (Cons 30 Nil)))))
                  (Std.Option.value 0 (Std.Array.get 2 a))"))
(check "std arrays set" "- : int = 99"
       (run-last "(define a (Std.Array.set 1 99 (Std.Array.repeat 3 0)))
                  (Std.Option.value 0 (Std.Array.get 1 a))"))
(check "std arrays set out of range is a no-op" "- : bool = #t"
       (run-last "(define a (Std.Array.of-list (Cons 1 Nil)))
                  (= (Std.Array.to-list (Std.Array.set 5 9 a)) (Cons 1 Nil))"))
(check "std arrays roundtrip preserves order" "- : bool = #t"
       (run-last "(define xs (Std.List.range 0 10))
                  (= (Std.Array.to-list (Std.Array.of-list xs)) xs)"))
(check "std arrays init and length" "- : int = 4"
       (run "(Std.Array.length (Std.Array.init 4 (fun (i) (* i i))))"))
(check "std arrays init indexes" "- : int = 9"
       (run "(Std.Option.value 0
              (Std.Array.get 3 (Std.Array.init 4 (fun (i) (* i i)))))"))
(check "std arrays map" "- : int = 6"
       (run "(Std.Option.value 0
              (Std.Array.get 1 (Std.Array.map (fun (n) (* n 2))
                                              (Std.Array.of-list
                                               (Cons 1 (Cons 3 Nil))))))"))
(check "std arrays fold" "- : int = 6"
       (run "(Std.Array.fold + 0 (Std.Array.of-list (Cons 1 (Cons 2 (Cons 3 Nil)))))"))

(check "std compose" "- : string = \"3\""
       (run "((Std.Fn.compose string-of-int (fun (n) (+ n 1))) 2)"))

(newline)
(if (= failures 0)
    (begin (display "all tests passed") (newline))
    (begin (display failures) (display " test(s) failed") (newline) (exit 1)))
