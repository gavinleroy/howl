export const EXAMPLES = [
  {
    name: "points",
    source: `(define (move (tuple x y))
  (tuple (/ x 2) (/ y 2)))

(define (dist (tuple x y))
  (+. (*. x x) (*. y y)))

(define (move-closer pos)
  (if (< (dist pos) 25.0)
      pos
      (move pos)))
`,
  },
  {
    name: "assoc",
    source: `(define (assoc (tuple d k l))
  (match l
    (Nil d)
    ((Cons (tuple f s) t)
     (if (= k f)
           s
           (assoc d k t)))))
`,
  },
];

export const INITIAL_SOURCE = EXAMPLES[0].source;
