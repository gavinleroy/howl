(define-module (lang std)
  #:pure
  #:use-module (scheme base)
  #:export (prelude-forms prelude-modules prelude-types prelude-signatures))

(define prelude-modules
  '(Fn Int Char Option Result Pair List Map Str Array))

(define prelude-types '(option result pair list))

(define prelude-signatures
  '(FN INT CHAR OPTION RESULT PAIR LIST MAP STR ARRAY))

(define prelude-forms
  '((type option 'a (enum None (Some 'a)))
    (type result 'ok 'err (enum (Ok 'ok) (Err 'err)))
    (type pair 'a 'b (enum (Pair 'a 'b)))
    (type list 'a (enum Nil (Cons 'a (list 'a))))

    (module Std
      (struct
    (module-type FN
          (sig (val identity (-> ('a) 'a))
               (val const (-> ('a 'b) 'a))
               (val compose (-> ((-> ('a) 'b) (-> ('c) 'a)) (-> ('c) 'b)))
               (val flip (-> ((-> ('a 'b) 'c)) (-> ('b 'a) 'c)))))

        (module Fn : FN
          (struct
            (define (identity x) x)
            (define (const x other) x)
            (define (compose f g) (fun (x) (f (g x))))
            (define (flip f) (fun (a b) (f b a)))))

        (module-type INT
          (sig (val min (-> (int int) int))
               (val max (-> (int int) int))
               (val abs (-> (int) int))
               (val to-string (-> (int) string))))

        (module Int : INT
          (struct
            (define (min a b) (if (< a b) a b))
            (define (max a b) (if (< b a) a b))
            (define (abs n) (if (< n 0) (- 0 n) n))
            (define (to-string n) (string-of-int n))))

        (module-type CHAR
          (sig (val is-space (-> (int) bool))
               (val is-digit (-> (int) bool))
               (val is-upper (-> (int) bool))
               (val is-lower (-> (int) bool))
               (val is-alpha (-> (int) bool))
               (val upper (-> (int) int))
               (val lower (-> (int) int))))

        (module Char : CHAR
          (struct
            (define (is-space c) (or (= c 32) (= c 9) (= c 10) (= c 13)))
            (define (is-digit c) (and (>= c 48) (<= c 57)))
            (define (is-upper c) (and (>= c 65) (<= c 90)))
            (define (is-lower c) (and (>= c 97) (<= c 122)))
            (define (is-alpha c) (or (is-upper c) (is-lower c)))
            (define (upper c) (if (is-lower c) (- c 32) c))
            (define (lower c) (if (is-upper c) (+ c 32) c))))

        (module-type OPTION
          (sig (type t 'a (option 'a))
               (val is-some (-> ((option 'a)) bool))
               (val is-none (-> ((option 'a)) bool))
               (val map (-> ((-> ('a) 'b) (option 'a)) (option 'b)))
               (val bind (-> ((-> ('a) (option 'b)) (option 'a)) (option 'b)))
               (val value (-> ('a (option 'a)) 'a))
               (val to-result (-> ('e (option 'a)) (result 'a 'e)))
               (val to-list (-> ((option 'a)) (list 'a)))))

        (module Option : OPTION
          (struct
            (type t 'a (option 'a))
            (define (is-some o) (match o ((Some x) #t) (None #f)))
            (define (is-none o) (match o ((Some x) #f) (None #t)))
            (define (map f o) (match o ((Some x) (Some (f x))) (None None)))
            (define (bind f o) (match o ((Some x) (f x)) (None None)))
            (define (value default o) (match o ((Some x) x) (None default)))
            (define (to-result error o)
              (match o ((Some x) (Ok x)) (None (Err error))))
            (define (to-list o) (match o ((Some x) (Cons x Nil)) (None Nil)))))

        (module-type RESULT
          (sig (type t 'ok 'err (result 'ok 'err))
               (val is-ok (-> ((result 'a 'e)) bool))
               (val is-error (-> ((result 'a 'e)) bool))
               (val map (-> ((-> ('a) 'b) (result 'a 'e)) (result 'b 'e)))
               (val map-error (-> ((-> ('e) 'f) (result 'a 'e)) (result 'a 'f)))
               (val bind (-> ((-> ('a) (result 'b 'e)) (result 'a 'e)) (result 'b 'e)))
               (val value (-> ('a (result 'a 'e)) 'a))
               (val to-option (-> ((result 'a 'e)) (option 'a)))))

        (module Result : RESULT
          (struct
            (type t 'ok 'err (result 'ok 'err))
            (define (is-ok r) (match r ((Ok x) #t) ((Err e) #f)))
            (define (is-error r) (match r ((Ok x) #f) ((Err e) #t)))
            (define (map f r) (match r ((Ok x) (Ok (f x))) ((Err e) (Err e))))
            (define (map-error f r)
              (match r ((Ok x) (Ok x)) ((Err e) (Err (f e)))))
            (define (bind f r) (match r ((Ok x) (f x)) ((Err e) (Err e))))
            (define (value default r) (match r ((Ok x) x) ((Err e) default)))
            (define (to-option r) (match r ((Ok x) (Some x)) ((Err e) None)))))

        (module-type PAIR
          (sig (type t 'a 'b (pair 'a 'b))
               (val make (-> ('a 'b) (pair 'a 'b)))
               (val first (-> ((pair 'a 'b)) 'a))
               (val second (-> ((pair 'a 'b)) 'b))))

        (module Pair : PAIR
          (struct
            (type t 'a 'b (pair 'a 'b))
            (define (make a b) (Pair a b))
            (define (first p) (match p ((Pair a b) a)))
            (define (second p) (match p ((Pair a b) b)))))

        (module-type LIST
          (sig (type t 'a (list 'a))
               (val empty (list 'a))
               (val is-empty (-> ((list 'a)) bool))
               (val head (-> ((list 'a)) (option 'a)))
               (val tail (-> ((list 'a)) (option (list 'a))))
               (val length (-> ((list 'a)) int))
               (val fold (-> ((-> ('b 'a) 'b) 'b (list 'a)) 'b))
               (val fold-right (-> ((-> ('a 'b) 'b) (list 'a) 'b) 'b))
               (val reverse (-> ((list 'a)) (list 'a)))
               (val append (-> ((list 'a) (list 'a)) (list 'a)))
               (val concat (-> ((list (list 'a))) (list 'a)))
               (val map (-> ((-> ('a) 'b) (list 'a)) (list 'b)))
               (val filter (-> ((-> ('a) bool) (list 'a)) (list 'a)))
               (val take-while (-> ((-> ('a) bool) (list 'a)) (list 'a)))
               (val drop-while (-> ((-> ('a) bool) (list 'a)) (list 'a)))
               (val for-all (-> ((-> ('a) bool) (list 'a)) bool))
               (val exists (-> ((-> ('a) bool) (list 'a)) bool))
               (val find (-> ((-> ('a) bool) (list 'a)) (option 'a)))
               (val member (-> ('a (list 'a)) bool))
               (val nth (-> (int (list 'a)) (option 'a)))
               (val take (-> (int (list 'a)) (list 'a)))
               (val drop (-> (int (list 'a)) (list 'a)))
               (val range (-> (int int) (list int)))
               (val zip (-> ((list 'a) (list 'b)) (list (pair 'a 'b))))
               (val sort-by (-> ((-> ('a 'a) bool) (list 'a)) (list 'a)))
               (val sort (-> ((list 'a)) (list 'a)))))

        (module List : LIST
          (struct
            (type t 'a (list 'a))
            (define empty Nil)
            (define (is-empty xs) (match xs (Nil #t) ((Cons h t) #f)))
            (define (head xs) (match xs (Nil None) ((Cons h t) (Some h))))
            (define (tail xs) (match xs (Nil None) ((Cons h t) (Some t))))
            (define (length xs)
              (match xs (Nil 0) ((Cons h t) (+ 1 (length t)))))
            (define (fold f acc xs)
              (match xs (Nil acc) ((Cons h t) (fold f (f acc h) t))))
            (define (fold-right f xs acc)
              (match xs (Nil acc) ((Cons h t) (f h (fold-right f t acc)))))
            (define (reverse xs) (fold (fun (acc x) (Cons x acc)) Nil xs))
            (define (append xs ys)
              (match xs (Nil ys) ((Cons h t) (Cons h (append t ys)))))
            (define (concat xss) (fold-right append xss Nil))
            (define (map f xs)
              (match xs (Nil Nil) ((Cons h t) (Cons (f h) (map f t)))))
            (define (filter keep xs)
              (match xs
                (Nil Nil)
                ((Cons h t) (if (keep h) (Cons h (filter keep t)) (filter keep t)))))
            (define (take-while pred xs)
              (match xs
                (Nil Nil)
                ((Cons h t) (if (pred h) (Cons h (take-while pred t)) Nil))))
            (define (drop-while pred xs)
              (match xs
                (Nil Nil)
                ((Cons h t) (if (pred h) (drop-while pred t) (Cons h t)))))
            (define (for-all pred xs)
              (match xs (Nil #t) ((Cons h t) (and (pred h) (for-all pred t)))))
            (define (exists pred xs)
              (match xs (Nil #f) ((Cons h t) (or (pred h) (exists pred t)))))
            (define (find pred xs)
              (match xs
                (Nil None)
                ((Cons h t) (if (pred h) (Some h) (find pred t)))))
            (define (member x xs) (exists (fun (y) (= x y)) xs))
            (define (nth i xs)
              (match xs
                (Nil None)
                ((Cons h t) (if (<= i 0) (Some h) (nth (- i 1) t)))))
            (define (take n xs)
              (if (<= n 0)
                  Nil
                  (match xs (Nil Nil) ((Cons h t) (Cons h (take (- n 1) t))))))
            (define (drop n xs)
              (if (<= n 0)
                  xs
                  (match xs (Nil Nil) ((Cons h t) (drop (- n 1) t)))))
            (define (range from to)
              (if (>= from to) Nil (Cons from (range (+ from 1) to))))
            (define (zip xs ys)
              (match xs
                (Nil Nil)
                ((Cons x xt)
                 (match ys
                   (Nil Nil)
                   ((Cons y yt) (Cons (Pair x y) (zip xt yt)))))))
            (define (merge before xs ys)
              (match xs
                (Nil ys)
                ((Cons x xt)
                 (match ys
                   (Nil xs)
                   ((Cons y yt)
                    (if (before y x)
                        (Cons y (merge before xs yt))
                        (Cons x (merge before xt ys))))))))
            (define (sort-by before xs)
              (match xs
                (Nil Nil)
                ((Cons h t)
                 (match t
                   (Nil xs)
                   ((Cons h2 t2)
                    (let ((half (/ (length xs) 2)))
                      (merge before
                             (sort-by before (take half xs))
                             (sort-by before (drop half xs)))))))))
            (define (sort xs) (sort-by < xs))))

        (module-type MAP
          (sig (type t 'k 'v)
               (val empty (t 'k 'v))
               (val is-empty (-> ((t 'k 'v)) bool))
               (val insert (-> ('k 'v (t 'k 'v)) (t 'k 'v)))
               (val find (-> ('k (t 'k 'v)) (option 'v)))
               (val member (-> ('k (t 'k 'v)) bool))
               (val remove (-> ('k (t 'k 'v)) (t 'k 'v)))
               (val min-binding (-> ((t 'k 'v)) (option (pair 'k 'v))))
               (val fold (-> ((-> ('acc 'k 'v) 'acc) 'acc (t 'k 'v)) 'acc))
               (val size (-> ((t 'k 'v)) int))
               (val to-list (-> ((t 'k 'v)) (list (pair 'k 'v))))
               (val keys (-> ((t 'k 'v)) (list 'k)))
               (val values (-> ((t 'k 'v)) (list 'v)))
               (val of-list (-> ((list (pair 'k 'v))) (t 'k 'v)))
               (val map (-> ((-> ('v) 'w) (t 'k 'v)) (t 'k 'w)))))

        (module Map : MAP
          (struct
            (type t 'k 'v (enum MapEmpty (MapNode (t 'k 'v) 'k 'v (t 'k 'v))))
            (define empty MapEmpty)
            (define (is-empty m)
              (match m (MapEmpty #t) ((MapNode l k v r) #f)))
            (define (insert k v m)
              (match m
                (MapEmpty (MapNode MapEmpty k v MapEmpty))
                ((MapNode l key value r)
                 (if (< k key)
                     (MapNode (insert k v l) key value r)
                     (if (< key k)
                         (MapNode l key value (insert k v r))
                         (MapNode l key v r))))))
            (define (find k m)
              (match m
                (MapEmpty None)
                ((MapNode l key value r)
                 (if (< k key)
                     (find k l)
                     (if (< key k) (find k r) (Some value))))))
            (define (member k m) (Option.is-some (find k m)))
            (define (min-binding m)
              (match m
                (MapEmpty None)
                ((MapNode l k v r)
                 (match l
                   (MapEmpty (Some (Pair k v)))
                   ((MapNode ll lk lv lr) (min-binding l))))))
            (define (remove k m)
              (match m
                (MapEmpty MapEmpty)
                ((MapNode l key value r)
                 (if (< k key)
                     (MapNode (remove k l) key value r)
                     (if (< key k)
                         (MapNode l key value (remove k r))
                         (match l
                           (MapEmpty r)
                           ((MapNode ll lk lv lr)
                            (MapNode ll lk lv
                                     (remove k (MapNode lr key value r))))))))))
            (define (fold f acc m)
              (match m
                (MapEmpty acc)
                ((MapNode l k v r) (fold f (f (fold f acc l) k v) r))))
            (define (size m) (fold (fun (acc k v) (+ acc 1)) 0 m))
            (define (to-list m)
              (List.reverse (fold (fun (acc k v) (Cons (Pair k v) acc)) Nil m)))
            (define (keys m) (List.map Pair.first (to-list m)))
            (define (values m) (List.map Pair.second (to-list m)))
            (define (of-list bindings)
              (List.fold (fun (m binding)
                              (insert (Pair.first binding) (Pair.second binding) m))
                         empty
                         bindings))
            (define (map f m)
              (match m
                (MapEmpty MapEmpty)
                ((MapNode l k v r) (MapNode (map f l) k (f v) (map f r)))))))

        (module-type STR
          (sig (val is-empty (-> (string) bool))
               (val length (-> (string) int))
               (val char-at (-> (int string) (option string)))
               (val sub (-> (int int string) string))
               (val codes (-> (string) (list int)))
               (val chars (-> (string) (list string)))
               (val of-codes (-> ((list int)) string))
               (val concat (-> ((list string)) string))
               (val uppercase (-> (string) string))
               (val lowercase (-> (string) string))
               (val starts-with (-> (string string) bool))
               (val ends-with (-> (string string) bool))
               (val index-of (-> (string string) (option int)))
               (val contains (-> (string string) bool))
               (val split (-> (string string) (list string)))
               (val join (-> (string (list string)) string))
               (val repeat (-> (int string) string))
               (val reverse (-> (string) string))
               (val trim (-> (string) string))
               (val to-int (-> (string) (option int)))))

        (module Str : STR
          (struct
            (define (is-empty s) (= (string-length s) 0))
            (define (length s) (string-length s))
            (define (char-at i s) (Option.map string-of-code (string-code-at s i)))
            (define (sub start len s)
              (if (<= len 0)
                  ""
                  (match (string-code-at s start)
                    ((Some c) (^ (string-of-code c) (sub (+ start 1) (- len 1) s)))
                    (None ""))))
            (define (codes-from i s)
              (match (string-code-at s i)
                ((Some c) (Cons c (codes-from (+ i 1) s)))
                (None Nil)))
            (define (codes s) (codes-from 0 s))
            (define (chars s) (List.map string-of-code (codes s)))
            (define (concat pieces) (List.fold ^ "" pieces))
            (define (of-codes cs) (concat (List.map string-of-code cs)))
            (define (map-codes f s) (of-codes (List.map f (codes s))))
            (define (uppercase s) (map-codes Char.upper s))
            (define (lowercase s) (map-codes Char.lower s))
            (define (matches-from j needle i s)
              (match (string-code-at needle j)
                ((Some nc)
                 (match (string-code-at s i)
                   ((Some sc) (and (= nc sc) (matches-from (+ j 1) needle (+ i 1) s)))
                   (None #f)))
                (None #t)))
            (define (starts-with prefix s) (matches-from 0 prefix 0 s))
            (define (ends-with suffix s)
              (let ((offset (- (string-length s) (string-length suffix))))
                (and (>= offset 0) (matches-from 0 suffix offset s))))
            (define (index-from i needle s)
              (if (> (+ i (string-length needle)) (string-length s))
                  None
                  (if (matches-from 0 needle i s)
                      (Some i)
                      (index-from (+ i 1) needle s))))
            (define (index-of needle s) (index-from 0 needle s))
            (define (contains needle s) (Option.is-some (index-of needle s)))
            (define (split sep s)
              (if (is-empty sep)
                  (chars s)
                  (match (index-of sep s)
                    ((Some i)
                     (Cons (sub 0 i s)
                           (split sep
                                  (sub (+ i (string-length sep))
                                       (string-length s)
                                       s))))
                    (None (Cons s Nil)))))
            (define (join sep pieces)
              (match pieces
                (Nil "")
                ((Cons h t) (List.fold (fun (acc piece) (^ acc (^ sep piece))) h t))))
            (define (repeat n s) (if (<= n 0) "" (^ s (repeat (- n 1) s))))
            (define (reverse s) (of-codes (List.reverse (codes s))))
            (define (trim s)
              (of-codes
               (List.reverse
                (List.drop-while Char.is-space
                                 (List.reverse
                                  (List.drop-while Char.is-space (codes s)))))))
            (define (digits-value acc cs)
              (match cs
                (Nil (Some acc))
                ((Cons c t)
                 (if (Char.is-digit c)
                     (digits-value (+ (* acc 10) (- c 48)) t)
                     None))))
            (define (to-int s)
              (match (codes s)
                (Nil None)
                ((Cons c t)
                 (if (= c 45)
                     (match t
                       (Nil None)
                       ((Cons c2 t2)
                        (Option.map (fun (n) (- 0 n)) (digits-value 0 t))))
                     (digits-value 0 (Cons c t))))))))

        (module-type ARRAY
          (sig (type t 'a)
               (val empty (t 'a))
               (val length (-> ((t 'a)) int))
               (val is-empty (-> ((t 'a)) bool))
               (val get (-> (int (t 'a)) (option 'a)))
               (val set (-> (int 'a (t 'a)) (t 'a)))
               (val push (-> ('a (t 'a)) (t 'a)))
               (val of-list (-> ((list 'a)) (t 'a)))
               (val to-list (-> ((t 'a)) (list 'a)))
               (val map (-> ((-> ('a) 'b) (t 'a)) (t 'b)))
               (val fold (-> ((-> ('b 'a) 'b) 'b (t 'a)) 'b))
               (val repeat (-> (int 'a) (t 'a)))
               (val init (-> (int (-> (int) 'a)) (t 'a)))))

        (module Array : ARRAY
          (struct
            (type tree 'a (enum ArrayLeaf (ArrayNode 'a (tree 'a) (tree 'a))))
            (type t 'a (record (size int) (tree (tree 'a))))
            (define empty (record (size 0) (tree ArrayLeaf)))
            (define (length a) a.size)
            (define (is-empty a) (= a.size 0))
            (define (tree-get i node)
              (match node
                (ArrayLeaf None)
                ((ArrayNode v odd even)
                 (if (= i 0)
                     (Some v)
                     (if (= (mod i 2) 1)
                         (tree-get (/ (- i 1) 2) odd)
                         (tree-get (- (/ i 2) 1) even))))))
            (define (get i a)
              (if (and (>= i 0) (< i a.size)) (tree-get i a.tree) None))
            (define (tree-set i x node)
              (match node
                (ArrayLeaf ArrayLeaf)
                ((ArrayNode v odd even)
                 (if (= i 0)
                     (ArrayNode x odd even)
                     (if (= (mod i 2) 1)
                         (ArrayNode v (tree-set (/ (- i 1) 2) x odd) even)
                         (ArrayNode v odd (tree-set (- (/ i 2) 1) x even)))))))
            (define (set i x a)
              (if (and (>= i 0) (< i a.size))
                  (record (size a.size) (tree (tree-set i x a.tree)))
                  a))
            (define (tree-add i x node)
              (match node
                (ArrayLeaf (ArrayNode x ArrayLeaf ArrayLeaf))
                ((ArrayNode v odd even)
                 (if (= (mod i 2) 1)
                     (ArrayNode v (tree-add (/ (- i 1) 2) x odd) even)
                     (ArrayNode v odd (tree-add (- (/ i 2) 1) x even))))))
            (define (push x a)
              (record (size (+ a.size 1)) (tree (tree-add a.size x a.tree))))
            (define (of-list xs) (List.fold (fun (a x) (push x a)) empty xs))
            (define (to-list-from i a)
              (match (get i a)
                ((Some x) (Cons x (to-list-from (+ i 1) a)))
                (None Nil)))
            (define (to-list a) (to-list-from 0 a))
            (define (tree-map f node)
              (match node
                (ArrayLeaf ArrayLeaf)
                ((ArrayNode v odd even)
                 (ArrayNode (f v) (tree-map f odd) (tree-map f even)))))
            (define (map f a)
              (record (size a.size) (tree (tree-map f a.tree))))
            (define (fold f acc a) (List.fold f acc (to-list a)))
            (define (repeat n x) (of-list (List.map (fun (i) x) (List.range 0 n))))
            (define (init n f) (of-list (List.map f (List.range 0 n))))))))))
