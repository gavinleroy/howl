# A little ML, typed by subtyping alone

An ML-flavoured language with structural records, structural enums and a
module system of signatures, implementations and functors — written in
Guile Scheme and compiled to WebAssembly with
[Hoot](https://spritely.institute/hoot/). The host is a React page with a
CodeMirror editor: it hands the compiled module the text in the editor
and reports the type of every declaration and the type and value of every
expression.

Type inference uses **no unification**. Every typing rule emits a
subtyping constraint `t1 <: t2`, type variables carry lower and upper
bounds instead of a single link, and the type you see is coalesced out of
those bounds and simplified. This is Stephen Dolan's algebraic subtyping
(MLsub) in the formulation of Lionel Parreaux's *Simple-sub* — full
inference, principal types, and no annotations anywhere.

The whole program is one file of s-expressions, so `read` does the
parsing and the code spends its effort on constraints and modules.

## Using it

```sh
nix build      # compile to WebAssembly, run the test suites, produce ./result
nix run        # build and serve the demo on http://localhost:8080
nix run . -- 9000   # ...on another port
```

The demo loads with one failure, of the kind an error message alone
cannot explain. It is this OCaml, translated:

```ocaml
let rec assoc (d, k, l) = match l with
  | [] -> d
  | h :: t -> let (f, s) = h in if k = f then s else assoc d k t
```

The picker holds three programs, each one failing in a different way:
**points** (a four-line int/float clash), **assoc** (the one below), and
**shelf** (a module that does not match its signature — the mistake is
one word, three levels inside a nested module).

`assoc` takes one argument — a triple — and destructures it, but the
recursive call passes three arguments instead of one. Under subtyping the
definition itself is fine: the checker infers that `d` must *also* be a
triple, since that is what the recursive call demands of it. Nothing goes
wrong until a caller passes `0` for `d`. The message is then "type int is
not a tuple", pointing at the call, while the mistake is the recursive
call inside the definition. The provenance is what connects them.
Everything above the failure still runs, and its output appears under
stdout.

`./result` is a self-contained static site (seven files); any server that
sends `application/wasm` for `.wasm` will do. The page needs a browser
with WebAssembly GC: Chrome 119+, Firefox 120+, or Safari 18.2+.

`nix build` runs all three test suites as part of the build, so a failing
typechecker is a failing build:

- `tests/run-tests.scm` exercises the language under native Guile.
- `tests/wasm-e2e.cjs` loads the compiled module under Node and drives the
  same protocol the page uses, which is the only way to catch a broken FFI
  signature without opening a browser.
- `tests/renderers.test.jsx` renders diagnostics through the React
  renderer registry with `react-dom/server`.

## What subtyping buys

```scheme
(define (get-x p) p.x)
```
```
val get-x : (-> (record (x 'a)) 'a) = <fun>
```

`get-x` asks for a record with an `x` and nothing else, so any wider
record flows in — width subtyping, inferred, with no row variables and no
declarations.

```scheme
(fun (b) (if b 1 "text"))
```
```
- : (-> bool (∪ string int)) = <fun>
```

Branches do not have to agree. The result is their union.

```scheme
(define (area s)
  (match s
    ((Circle r) (*. r r))
    ((Rect w h) (*. w h))))

(area (Circle 2.0))
```
```
val area : (-> (enum (Circle float) (Rect float float)) float) = <fun>
- : float = 4.0
```

`(Circle 2.0)` has type `(enum (Circle float))`, a variant with one case, and
it flows into a match that handles more. Constructors need no prior
declaration.

Types print in the same s-expression syntax you write, and are read as a
lattice, so they mention `(∪ a b)` (union), `(∩ a b)` (intersection),
`any` (the top type, which accepts everything), `nothing` (the bottom
type) and `(rec 'a t)` for recursive shapes. Some
familiar types come out more general than Hindley-Milner would give:

```scheme
(fun (f x) (f (f x)))
```
```
- : (-> (-> (∪ 'a 'b) (∩ 'c 'a)) 'b 'c) = <fun>
```

where HM reports `(-> (-> 'a 'a) 'a 'a)`. The subtyping type is
principal and strictly more permissive: it accepts functions HM rejects,
such as one whose argument and result types differ.

Two consequences worth knowing. `(fun (f) (f f))` is well typed here —
`(-> (∩ 'a (-> 'a 'b)) 'b)` — because there is no occurs check to fail,
only a recursive type. And `(= 1 "a")` is accepted, since `=` has type
`(-> 'a 'a bool)` and `'a` can be `(∪ int string)`; it evaluates to `#f`.

## The language

```scheme
(type shape (enum (Circle float) (Rect float float)))

(define (area s)
  (match s
    ((Circle r) (*. 3.14159 (*. r r)))
    ((Rect w h) (*. w h))))

(define (describe s)
  (^ "area = " (string-of-float (area s))))

(describe (Rect 3.0 4.0))
```

```
type shape = (enum (Circle float) (Rect float float))
val area : (-> (enum (Circle float) (Rect float float)) float) = <fun>
val describe : (-> (enum (Circle float) (Rect float float)) string) = <fun>
- : string = "area = 12.0"
```

### Expressions

`(fun (x y) body)`, application, `(let ((x e) ...) body)`,
`(if test then else)`, `(and e ...)`, `(or e ...)`, and literals `42`,
`2.5`, `"text"`, `#t`, `#f`. Functions are curried, so `(f a b)` means
`((f a) b)` and partial application works. `let` binds in parallel and
generalizes each right-hand side.

### Declarations

`(define x e)` binds a value; `(define (f x) body)` binds a function that
may call itself. Both generalize.

### Records, tuples and enums

`(record (x 1) (y 2.0))` builds a record; `p.x` and `(dot p x)` read a
field. `(tuple 1 "a")` builds a tuple, which is positional and fixed-width
rather than named and extensible; it is taken apart by pattern, in a
parameter, a `let` binder or a match clause:

```scheme
(define (assoc (tuple d k l)) ...)
(let (((tuple f s) h)) ...)
(match p ((tuple a b) (+ a b)))
``` `(Circle 1.5)` builds a variant value, `Origin` a nullary one, and
`match` takes them apart with constructor, variable, literal and `_`
patterns. A match that lists constructors requires exactly those cases of
its scrutinee, which is what makes it exhaustive.

`(type point (record (x int) (y float)))` and
`(type shape (enum (Circle float) Origin))` give a structure a name for
use in signatures. A name and its structure are interchangeable — the
constraint solver expands the name when it needs to — and recursive
declarations are allowed.

### Modules

```scheme
(module-type COUNTER
  (sig (type t)
       (val start t)
       (val bump (-> t t))
       (val value (-> t int))))

(module Counter : COUNTER
  (struct
    (type t int)
    (define start 0)
    (define (bump c) (+ c 1))
    (define (value c) c)))

(Counter.value (Counter.bump Counter.start))
```

Sealing hides what the signature does not mention and makes a bare
`(type t)` abstract: `(Counter.bump 5)` is rejected with
`type int cannot be used where type Counter.t is expected`. Write
`(type t int)` in the signature to keep it visible. Signatures can be
named with `module-type` or written inline.

`val` types in a signature are monomorphic, but a module may define a
polymorphic value and seal it at a specific type — matching is a subtype
check, so an implementation only has to be at least as general.

### Nested modules

A `struct` may hold `module` and `module-type` items, and paths go as deep
as the nesting: `Std.List.map` for values, `(Std.Map.t 'k 'v)` for types.

A signature describes a sub-module the same way a module defines one:

```scheme
(module-type ORD
  (sig (type t) (val of-int (-> int t)) (val compare (-> t t int))))

(module-type BOUNDS
  (sig (module Key : ORD)
       (val largest (-> Key.t Key.t Key.t))))

(module B : BOUNDS
  (struct
    (module Key : ORD
      (struct (type t int)
              (define (of-int n) n)
              (define (compare a b) (- a b))))
    (define (largest a b) (if (< (Key.compare a b) 0) b a))))
```

```
module B : sig module Key; val largest : (-> B.Key.t B.Key.t B.Key.t) end
```

Later items in a signature can refer to an earlier `module` spec, which is
how `largest` names `Key.t`. Matching is recursive — a missing sub-module,
or one that does not satisfy its own signature, is reported at the seal —
and abstraction survives the nesting: `(B.largest 3 7)` is rejected
because `B.Key.t` is abstract, while `(B.largest (B.Key.of-int 3)
(B.Key.of-int 7))` is fine. Abstract types are named by their full path,
so errors say `B.Key.t` rather than `t`.

`Std` itself is deliberately left unsealed: its sub-modules are each
sealed, which is what keeps the library's internals out of your
diagnostics, but the umbrella stays transparent.

### Functors

```scheme
(module-type ORD (sig (type t) (val compare (-> t t int))))

(functor MakeBounds (M : ORD)
  (struct
    (define (largest a b) (if (< (M.compare a b) 0) b a))))

(module IntOrd : (sig (type t int) (val compare (-> t t int)))
  (struct (type t int) (define (compare a b) (- a b))))

(module IntBounds (MakeBounds IntOrd))

(IntBounds.largest 3 7)
```

### Operators

`+ - * / mod` on `int`, `+. -. *. /.` on `float`, `^` on `string`,
`= <> < > <= >=` at `(-> 'a 'a bool)` with structural comparison
(comparing two functions is a run-time error), plus `not`,
`float-of-int`, `int-of-float`, `string-of-int`, `string-of-float`,
`string-of-bool`, `string-length`, and the two primitives the string
library is built on: `string-code-at` at
`(-> string int (enum (Some int) None))`, the code point at an index,
and `string-of-code` at `(-> int string)`, the one-character string of a
code point (an invalid code point becomes U+FFFD).

### The standard library

Every program sees one module, `Std`, elaborated before the first form of
your program runs. It is written in the language itself and holds sealed
sub-modules — `Std.List`, `Std.Map`, `Std.Str`, `Std.Option`,
`Std.Result`, `Std.Pair`, `Std.Array`, `Std.Char`, `Std.Int`, `Std.Fn` —
so access is always qualified: `(Std.List.map f xs)`.

Four ambient type constructors come with it, usable in your own
signatures: `(list 'a)`, `(option 'a)`, `(result 'ok 'err)` and
`(pair 'a 'b)`.

Each sub-module declares its own `t`. `Std.List.t`, `Std.Option.t`,
`Std.Result.t` and `Std.Pair.t` are manifest aliases of those
constructors, so `Nil`, `Cons`, `Some`, `Ok` and `Pair` stay writable.
`Std.Map.t` and `Std.Array.t` are **abstract**: the balanced tree and the
indexed tree behind them are not reachable, and

```scheme
(Std.Map.insert 1 2 MapEmpty)
```

is rejected — you have to start from `Std.Map.empty`.

Sealing is what keeps the library's internals out of your diagnostics. A
value's exported type is rebuilt from its signature, so the constraint
graph of the implementation is never reachable from a use site, and a
mistake in your own code explains itself in terms of the interface:

```
The checker was asking whether (-> (-> 'a 'b) (-> (list 'a) (list 'b)))
can be used as (-> int 'c).
```

Before the library was sealed, that same mistake cited `(f h)` and
`(list-map f t)` — expressions from inside the library that the user
never wrote.

## How the checker works

### Constraints, not unification

`src/lang/types.scm` holds the type graph. A type variable is a mutable
record with a level and two bound lists, and `constrain` is the whole
solver:

```scheme
(constrain (func-domain sup) (func-domain sub) cache)
(constrain (func-codomain sub) (func-codomain sup) cache)
```

Function domains constrain contravariantly and codomains covariantly.
When one side is a variable, the constraint is recorded as a bound and
propagated against the bounds already there: `sub <: sup` adds `sup` to
`sub`'s upper bounds and then constrains every lower bound of `sub`
against `sup` — unless that exact bound is already recorded, in which
case the whole step is skipped, since its propagation already happened.
A cache of pairs already seen makes cyclic constraints terminate, which
is what lets recursive types work without an occurs check.

Records constrain by width — every field the target asks for must exist
in the source — and variants by case inclusion, which is the same rule
read in the other direction. Tuples constrain position by position and
must agree on width, so `(tuple 1 2 3)` is not a `(tuple 'a 'b)`; the
difference from records is the point of having both.

### Levels and extrusion

Generalization is by level, as in Simple-sub: a `let` or `define`
right-hand side is inferred one level deeper, and `(generalize type
level)` just records the level, with `instantiate` freshening every
variable above it. When a constraint would connect a variable to a type
from a deeper level, the type is *extruded* — copied down to the
variable's level with fresh variables that are wired back to the
originals in the right polarity — so no deep variable escapes into a
shallow scope.

### Coalescing and simplification

`src/lang/print.scm` turns the constraint graph back into something
readable. A variable is replaced by the union of its lower bounds in
positive position and the intersection of its upper bounds in negative
position, with recursion caught by a `rec` binder. Every pass is
memoized per variable and polarity — the bound graph is a dense DAG, and
expanding it as a tree re-expands shared variables exponentially; the
standard library's tree-manipulating functions made that cost visible
(a binding built by folding `Std.Map.remove` used to take the printer
minutes, and takes milliseconds shared). Then:

- A variable that occurs in only one polarity carries no information and
  is dropped — it becomes `nothing` in a union or `any` in an
  intersection. This is why `(fun (x) 1)` prints as `(-> any int)`.
- Intersections of functions collapse:
  `(∩ (-> A B) (-> C D))` becomes `(-> (∪ A C) (∩ B D))`. Intersections of
  records merge their fields, unions of records keep the common ones,
  unions of variants merge their cases, and tuples of equal width merge
  component by component.

Without this pass every inferred type is a wall of bounds; with it the
output above is what you get.

Messages use it too, but not everywhere. A type that describes a *value*
is simplified — "value `f` has type `(-> int int)`", not the bounds that
produced it. A type that describes a *demand* is printed as written —
"the pattern `(tuple d k l)` requires `(tuple 'a 'b 'c)`" — because
simplifying a demand in isolation replaces its unconstrained parts with
`any` and coalescing it pulls in every other demand made of the same
variable, which is true but not what the reader asked about. The payload
carries the raw form as well, under `graph.types`.

### Modules

- **Sealing** is generative. Each abstract `(type t)` gets a fresh opaque
  type with no structure and no subtyping relations but reflexivity,
  named after the module (`Counter.t`) so errors read well. Matching
  parses the signature twice: once with `t` bound to the implementation's
  real type, to check the values with `constrain`, and once with `t`
  bound to the fresh opaque type, to produce what the rest of the program
  sees.
- **Functors** are checked twice. At the definition the body is
  elaborated with the parameter bound to a skolem module built from the
  signature. At each application the body is re-elaborated with the
  parameter bound to the actual argument, which gives the result module
  precise types — `(MakeBounds IntOrd)` yields
  `(-> (∩ 'a int) (∩ 'a int) 'a)` rather than something phrased in terms
  of `M.t`.

Note that subtyping changes what the definition-time check catches. A
functor body that mixes an abstract type with a concrete one, such as
`(+ (M.compare a a) a)`, is no longer an error: it is given the type
`(-> (∩ int M.t) int)`, a function whose argument must be both, which no
caller can satisfy while `t` is abstract. The abstraction still holds; it
is enforced at the use site instead of the definition site.

`src/lang/eval.scm` is a separate call-by-value interpreter over the same
forms, so a program is checked and then run, and nothing about types is
consulted at run time.

### Attaching the expression

`fail` records the expression being processed at the moment of the raise.
`infer` and `evaluate` keep that in a single cell, saving and restoring it
around each node: on the way out of a successful node the previous value
is put back, and on a failure the cell still holds the innermost node,
which is exactly what the message wants.

The obvious alternative — wrapping every node in `guard` and re-raising
after filling in a mutable field — works under native Guile and breaks
under Hoot. R7RS says `guard` re-raises when no clause matches, and
Hoot's re-raise path expects a real exception object, so a diagnostic
record raised from a *nested* expression died inside the runtime rather
than reaching the handler. Any `guard` in this codebase therefore has a
`#t` clause and re-raises explicitly.

A second Hoot codegen bug turned up when signatures learned to describe
sub-modules. `signature-view` walks a signature's items, and a `module`
item has to elaborate the nested signature, which calls back into
`signature-view`. That cycle makes Hoot emit a broken function: the wasm
traps with `invalid cast` on *entry* to `signature-view`, before any item
is examined, even for a signature with no module items at all — while
native Guile runs it correctly. Removing the recursive call fixes it, and
so does routing that one edge through a cell:

```scheme
(define %module-entry (vector #f))

(define (signature-module-entry item scope modtype prefix)
  ((vector-ref %module-entry 0) item scope modtype prefix))

(vector-set! %module-entry 0 resolve-module-entry)
```

The indirection is load-bearing, not decoration: inline the call back and
the WebAssembly build stops working while every native test still
passes. `bugs/hoot-invalid-cast/` holds a 46-line standalone reproducer,
reduced from this code, along with a one-expression variant that compiles
correctly and a script that runs both under native Guile and under Hoot.

### Diagnostics

No module writes an error message except `diagnostics.scm`. Everywhere
else a failure is raised as a tag plus its data:

```scheme
(fail 'unbound-value head scope)
(fail 'type-mismatch sub sup)
(fail 'missing-signature-value (car binding))
```

`fail` lives in `world.scm` and attaches the state of the world at the
point of the raise: the phase (reading, typing or evaluating), the top
level form being processed, the typing scope, the runtime environment and
the program source. The innermost expression is filled in on the way out
by the guards in `infer` and `evaluate`, so the diagnostic knows both
where it happened and what was around it.

`diagnostics.scm` owns two things and no others. The first is the schema:
`diagnostic-schema` says what each tag carries — `actual` and `expected`
types for a mismatch, a name and a scope for an unbound value — which is
what the serializer uses to hand named references to the host. The second
is wording, for hosts that want text rather than data: one dispatch over
the tag, used by `run-program-text` and by the Scheme test suite. Nothing
else in the codebase writes a message.

Carrying the world is what lets even that text say more than the failure
itself knew:

```
Type error in totl: unbound value totl, did you mean total?
Type error in (module Bad : ORD (struct (type t int))): the signature
requires value compare, which the module does not define
Runtime error in (/ 1 0): /: division by zero
```

The first comes from the scope travelling with the diagnostic, the second
from the enclosing declaration in the world when no inner expression was
recorded, the third from the phase.

### Rendering is the host's job

Checking does not stop at the first failure. `run-program` walks the top
level forms, and a form that fails contributes a diagnostic and binds
nothing while the forms after it are still checked against the state that
did work. It returns both halves — `result-transcript`, the output of the
forms that succeeded, and `result-diagnostics`, everything that went
wrong — which is why the page can show stdout and a list of failures at
the same time.

Scheme never renders a diagnostic for the page. `src/main.scm` reports the
transcript once and then one `app.diagnostic` call per failure, and
`src/lang/serialize.scm` hands each of them across the boundary — not a message, and not
pre-rendered fields, but the objects the checker was holding when it
failed:

```json
{
  "tag": "type-mismatch",
  "phase": "typing",
  "fields": {
    "actual":   { "kind": "type", "id": "t1", "text": "string" },
    "expected": { "kind": "type", "id": "t2", "text": "int" }
  },
  "data": [ ... the raise arguments, positionally ... ],
  "expression": { "text": "(+ 1 \"a\")",
                  "form": { "kind": "list", "items": [ ... ] } },
  "world": { "phase": "typing", "form": {...}, "scope": "e3",
             "environment": "r51", "source": "(+ 1 \"a\")" },
  "graph": { "types": {...}, "scopes": {...}, "modules": {...},
             "signatures": {...}, "values": {...}, "environments": {...} }
}
```

`fields` and `world` hold *references* into `graph`, which is an interned
object graph: every type, scheme, scope, module, signature, runtime value
and environment reachable from the failure, each with an id, each pointing
at others by id. Interning is what makes cycles expressible — a recursive
type, or a closure whose environment contains the closure — and it means
nothing is duplicated or truncated.

A type variable carries the constraints that were on it, and each bound
carries where it came from:

```json
"t8": { "id": "t8", "kind": "var", "level": 1, "text": "'b",
        "lower": [],
        "upper": [{ "type": "t9",
                    "origin": { "expression": { "text": "(f x)", "form": {...} },
                                "left": "t14", "right": "t15" } }] }
```

so a renderer can reconstruct the constraint set *and its history*, not
just read a sentence about it. The diagnostic also carries
`constraint: {left, right}` — the root constraint the solver was working
on when it failed. The scope in `world.scope` lists every binding in
effect with its scheme, which is how the default renderer computes "did
you mean" itself rather than being handed one:

```js
suggestName(diagnostic)   // walks graph.scopes[...].values
constraintsOn(diagnostic, "t8")   // { lower: [...], upper: [...] }
fieldType(diagnostic, "actual")   // the type node, not its text
bindingsInScope(diagnostic)       // everything in scope at the failure
```

`text` on a node is a convenience, rendered once per payload so that
`'a` means the same variable everywhere in it. Nothing forces a renderer
to use it.

Payloads run about 10 KB for a small program and 20 KB for one with
modules; they are built with an output port rather than string
concatenation, so size is linear.

`web/src/language.js` collects those calls, so `run(source)` answers with
`{transcript, diagnostics}`. The page shows the transcript as **stdout**
and, when the list is not empty, a diagnostics box below the editor. That
box toggles between two layouts — every diagnostic stacked in a list, or
one panel with a tab per diagnostic — which is `web/src/panel.jsx`, and
either way each diagnostic goes through the renderer list:

```js
const renderer = {
  id: "expected-only",
  match: (diagnostic) => diagnostic.tag === "type-mismatch",
  render: (diagnostic) => <strong>{diagnostic.fields.expected.text}</strong>,
};

<App renderers={[renderer, ...defaultRenderers]} />
```

`selectRenderer` takes the first renderer whose `match` accepts the
diagnostic, and `DiagnosticView` wraps the result with the phase and the
offending expression. `DiagnosticBoundary` is a React error boundary
around the chosen renderer, so a custom renderer that throws shows a
renderer error instead of blanking the page.

Six renderers ship with the demo. Three are plain ASCII text produced in Guile, in
`web/render/` (one module per renderer), and shipped in the payload as
`renderings: {terse, minimal, flow}`. Their React side is one line each:

```jsx
render: (diagnostic) => <pre className="diagnostic-text">{diagnostic.renderings.flow}</pre>
```

**terse** says only what kind of failure it was:

```
a type error occurred
```

**minimal** states the one fact that matters:

```
expression `(+ 1 #t)` has type bool but was expected to have type int
expression `(use 1)` has type int but was expected to be a record
name `totl` is not defined here
```

**flow** follows the shape argued for in *Getting into the Flow*
(arXiv:2402.12637): name the two types, converge them on the variable
where they meet, then say where each side came from.

```
[ERROR] Type `int` does not match `(record (k 'a))`

        (int) ---> ('b) <--- (record (k 'a))

* (int) comes from
|  - (assoc (record (d 0) (k "b") (l entries)))
v
* ('b) is assumed for
^  - assoc
|
* (record (k 'a)) comes from
   - args.k
```

The summary line is the whole diagnosis: an `int` flowed in from the call,
a record was demanded by `args.k` inside the definition, and the two met
at the variable standing for `assoc`'s parameter. The gutter arrows point
the way each side travels — `v` down into the meeting variable, `^` up
from the side below it.

Making this work needed one addition to the solver: type variables record
the expression they were *assumed for* (`tvar-source`, captured from the
current expression when the variable is created), which is what the middle
bullet reports. Bound types and expressions are clipped, and the side
chains are followed through at most four hops, preferring bounds that
carry a source expression.

**linked** keeps that structure and hangs it on the source text. It is not
a string: the payload carries a `flow` object of chips, steps and sites,
each site holding the character ranges of the expression that placed the
bound and the two types of the constraint placed there.

```
Type `int` does not match `(record (k 'a))`
in (assoc (record (d 0) (k "b") (l entries)))

    (int)  ──▶  ('b)  ◀──  (record (k 'a))

  ● (int) comes from
  │ ↳ (assoc (record (d 0) (k "b")…    accepts  'b <: (-> (record (d int)…
  ▼
  ● ('b) is assumed for
  ▲ ↳ assoc
  │
  ● (record (k 'a)) comes from
    ↳ args.k                       must satisfy  'w <: (record (k 'x))
```

Hovering a site highlights exactly that expression in the editor; clicking
pins the highlight; hovering the `('b)` chip highlights the `assoc` it was
assumed for. Highlighting never scrolls the editor — a message that jumps
the page out from under you is worse than one you have to scroll to. The React side is
still small — `web/src/flow.jsx` walks the object and attaches four mouse
handlers — because the ranges arrive ready to use.

Positions come from `src/lang/reader.scm`, which replaced R7RS `read`. It
is an ordinary recursive-descent reader over the source string, and it
records three things: the span of every datum, the span of every **cons
cell's `car`**, and a structural table as a fallback. The middle one is
what makes locations exact. Cons cells are unique objects, so the cell
holding an occurrence of `x` identifies *that* `x`, even though the symbol
itself is interned and shared with every other `x` in the file.

That identity is only useful if it reaches the solver, so the span travels
with the expression through inference and evaluation:

```scheme
(define (infer expr where scope level) ...)      ; `where` is the span
(infer (car args) (car-span args) scope level)   ; the cell locates the child
(infer (cadr expr) (nth-span expr 1) scope level)
```

`infer` and `evaluate` set `current-expression` and `current-span`
together, `origin-here` captures both, and every bound the solver records
therefore carries the exact range of the code that placed it. Nothing is
reconstructed afterwards.

Where a decomposition path can be walked back through the syntax, the
sub-expression is located the same way: `(item 1)` of `(tuple 3.0 4.0)`
resolves to the cell holding `3.0`, so the sentence says `3.0` *and*
points at it.

Forms the reader never saw — the prelude, which is quoted data in
`std.scm` — have no spans and render as plain text: provenance that ends
inside `Std` stays unlinked, exactly as sealing intends.

Every located form in the payload carries `spans`, so this is not private
to one renderer:

```json
{"text": "x",
 "spans": [{"from": 37, "to": 38, "line": 2, "column": 13,
            "endLine": 2, "endColumn": 14}]}
```

Offsets are what CodeMirror wants for a decoration; the line and column
are what a sentence prints. In the demo every link in the payload — 42 of
them — resolves to exactly one range, and each one covers the code its
sentence names. Two sentences about `pos` in the same `if` point at
different `pos`es.

The editor side is one `StateField` of mark decorations fed by a small
pub/sub module (`web/src/highlight.js`), so a renderer asks for a
highlight without holding a reference to the editor.

**tree** is the derivation of the constraint that failed. Every line is
either a **constraint** — `float <: int`, nothing else — or a **constant**,
which is where something concrete enters: a literal's type, a primitive's
type, or the expression a type variable stands for. A line's children are
the constraints it was derived from.

```
▾ float <: int                                               12:1
  ▾ float <: 'f                                              12:1
    ·   x is 'f                                               1:22
    ▾ (tuple float float) <: (tuple 'f 'h)                   12:1
      ·   y is 'h                                             1:24
      ▾ (tuple float float) <: 'c                            12:1
        ·   (tuple x y) is 'c                                 1:15
        ▾ (tuple float float) <: 'b                          12:1
          ·   pos is 'b                                       7:22
          ▾ (-> 'b 'd) <: (-> (tuple float float) 'l)        12:1
            ▾ 'a <: (-> (tuple float float) 'l)              12:1
              ·   (tuple 3.0 4.0) is (tuple float float)     12:14
            ▾ (-> 'b 'd) <: 'a                                7:9
              ▾ (-> 'a1 'b1) <: 'w1                           7:9
                ·   (move-closer pos) is (-> 'a1 'b1)         7:9
  ▾ 'f <: int                                                12:2
    ▾ 'r <: int                                              10:8
      ▾ 'r1 <: int                                            2:10
        ▾ (-> int (-> int int)) <: (-> 'r1 'v1)               2:10
          ·   / is (-> int (-> int int))                      2:11
```

The root's two children are the two sides: how `float` came to be the
actual bound, and how `int` came to be the expected one. Both bottom out
in constants — the literal that was passed and the primitive that was
applied.

This is recorded by the solver, not reconstructed. `constrain` in
`src/lang/types.scm` builds a `<step>` for every constraint it processes:
its two sides, the origin, and the steps it was **derived from**. A rule
places a step with no causes; decomposing `(-> A B) <: (-> C D)` makes two
steps whose cause is the arrow step; propagating `L <: U` from `v <: U`
and `L <: v` makes a step with both as causes. `fail` captures the step it
was in, and `constraint-tree` transcribes that DAG — one node per step,
its children the step's causes, and a step reached twice marked rather
than expanded again.

Two things keep the chain readable:

- **Instantiation is a step of its own.** `freshen` and `extrude` copy a
  variable's bounds; the copy gets a step of its own whose cause is the
  original. Without it a copied bound's constraint is stated in terms of
  the *original* variables, and the chain appears to jump: `(tuple float
  float) <: 'c` followed by a constraint mentioning neither. With it the
  link `'b <: 'c` is there, and under it the constraint at the definition
  that it was copied from.
- **Every variable is introduced.** The first line that mentions a
  variable carries a constant naming the expression it stands for — `pos
  is 'b`, `(tuple x y) is 'c`, `(move-closer pos) is 'w1`. Variables are
  given that source where they are created (`fresh-tvar-at` for a
  parameter, a pattern part, a variant payload, a recursive definition's
  own name), and a copy inherits the original's, so an instantiated
  parameter still says `pos`. In the points program that names 24 of 24
  variables the tree mentions.

The React side (`web/src/tree.jsx`) holds one set of open paths, so
`Expand all` and `Collapse all` are one `setState` each, and a folded line
carries the count of what it hides. Every line links to the editor through
the same bus the linked renderer uses.

**prose** is the same provenance written as English. Collapsed, it is two
lines — the whole diagnosis:

```
▸ The value `0` was expected to have type (tuple 'a 'b 'c)   2
▸ but it has type int                                        1
```

Each line opens onto the sentences that explain it. The first is the
*expected* branch: how the expectation came to be. The second is the
*actual* branch: how the value came to have that type. Every sentence
carries the line and column it happened at, and hovering one highlights
that code in the editor.

```
▾ The value `0` was expected to have type (tuple 'a 'b 'c)
  ▾ `0` has type int                                                              14:15
    ▾ `(assoc (tuple d k l))` is defined recursively, so its own calls share …     1:9
      ▾ the pattern `(tuple d k l)` requires (tuple 'a 'b 'c)                      1:16
        ▾ `d` is the 1st argument of `(assoc d k t)`                               8:19
          ▾ `k` is the 2nd argument of `(assoc d k t)`                             8:21
            · `t` is the 3rd argument of `(assoc d k t)`                           8:23
▾ but it has type int
  · `0` has type int                                                              14:15
```

The value the collapsed line names is not the expression the failure was
raised at — that would be the whole call, and nothing constrains the call
itself. It is whatever the *actual* branch's first sentence is about,
resolved through the decomposition path down to the sub-expression that
really carries the type: `0`, not `(assoc (tuple 0 "b" entries))`. The
same rule fixes the int/float example, where the old root claimed
`(move-closer (tuple 3.0 4.0))` "was expected to have type int" — it names
`3.0` now.

One constraint, one sentence. A constraint with nothing to point at in the
source gets no sentence — the solver's own plumbing is not something that
happened *in the program*, so it is spliced out and its children rise to
take its place. A constraint that says nothing (`'e` must be `'f`) is
dropped the same way.

That needed three things from the solver, all recorded on the `<origin>`
of every bound:

- **the reason.** `constrain` calls in `src/lang/infer.scm` go through
  `constrain-because` / `constrain-at`, which name the typing rule:
  `'argument` (with the position and the argument form), `'field`,
  `'condition`, `'branch` (with `then`/`else`), `'clause` (with the
  pattern), `'scrutinee`, `'pattern`, `'recursive`, `'signature`,
  `'equivalent`, `'boolean-op`. This is what lets a sentence say *which*
  branch of the match, not just that a match was involved.
- **the context.** The enclosing form, so a sentence can place its subject:
  "`d` is the 1st argument of `(assoc d k t)`".
- **the path.** `constrain-with` pushes a segment as it decomposes —
  `domain`, `result`, `(field k)`, `(item 2)`, `(case Cons)` — so a bound
  discovered three levels inside a type can name what it is. Where the path
  can be walked back through the *syntax* it names the sub-expression
  directly (`(item 1)` of `(tuple 0 "b" …)` is `0`); where it cannot — the
  `result` of a call has no sub-expression — it is described instead ("the
  result of `(pick #t)`"). Without it, every such bound would be
  misreported as belonging to the whole expression.

The depth at which a failure is raised is recorded too: a mismatch found
while decomposing is blamed on the enclosing form rather than on whatever
sub-expression happened to trigger the decomposition, which is why the
header says `(+ 1 #t)` while the sentence under it says `#t`.

The prose tree is the **same derivation, transcribed**. Both renderers walk
the steps recorded by `constrain`; the only difference is what a node
becomes. Where `tree` prints `'m <: int`, `prose` prints "`x` is the 1st
argument of `(/ x 2)`, so it must be int" — the same constraint, worded
from the rule that placed it. Where `tree` prints `/ is (-> int (-> int
int))`, prose prints the same thing as a sentence. One step, one line, in
both. The counts match: 19 sentences to 19 nodes below the root for the
points program.

The two collapsed lines are the root — the constraint that failed — split
across the sides it relates. The root's two causes divide between them:
the cause whose `sub` is the failing `sub` explains the value, the cause
whose `sup` is the failing `sup` explains the demand.

Because the derivation follows causes rather than the shape of types, it
crosses instantiation. For

```scheme
(define (pick b) (if b 1 "two"))
(+ 1 (pick #t))
```

the story reaches `"two"` inside `pick`, which the old reachability walk
could not do: the bound that carries it lives on a freshened copy of
`pick`'s result variable, and only the causal edge connects them.

Chains cross function boundaries where the provenance survives:

```
▾ The value `(twice bump "go")` was expected to have type int
  · the 1st argument of `bump` must be int
▾ but it has type string
  · the result of `string-of-int` has type string
```

They stop at generalization, which is the honest place to stop:
instantiating a polymorphic binding copies its bounds with fresh origins,
so the story ends at the call, not inside a function that was already
checked and sealed.

The panel's style picker chooses between them — `terse`, `minimal`,
`flow`, `tree` and `prose`, in that order. Two more renderer stacks stay
registered without a button: `linked` and `detailed`, the latter being
the structured view model described above. `prose` is what the demo
opens with.

Richer renderings are the point: a renderer can diff two type nodes
structurally, draw the bound graph of a variable, or list what was in
scope. Nothing in the language or the transport has to change to add them.

### Explainers: analysis in Guile, painting in JS

A snapshot cannot answer counterfactuals. The JSON above is a photograph
of the constraint graph; the questions worth asking — *which* constraint
added this bound, from which expression, and what happens if it is
dropped — need the solver, and the solver is in Scheme. So the split is:
Guile writes explainers that consume a live diagnostic and emit a small
view model; React interprets that view model. Adding an explanation is a
Scheme change, not a JS change.

`constrain` records provenance as it solves. Every bound on a type
variable is a `<bound>` carrying the type and an `<origin>`: the
expression being checked when the bound was added, and the root
constraint that was being solved. Diagnostics carry that root constraint
too, so an explainer can say what question the checker was asking when it
gave up.

An explainer is a function from the diagnostic to a view model, and
authoring one is meant to be a dozen readable lines:

```scheme
(define-explainer unbound-value (diagnostic)
  (let* ((name (field diagnostic "name"))
         (suggestions (near-misses name (binding-names diagnostic))))
    (section "Unbound value"
             (paragraph (text "There is no value named ")
                        (code name)
                        (text " in scope here."))
             (and (pair? suggestions)
                  (paragraph (text "Did you mean ")
                             (code (car suggestions))
                             (text "?")))
             (in-scope-section diagnostic))))
```

`define-explainer` registers it against a tag; `explain` dispatches, and
falls back to a generic explanation for any tag without one. `#f`
children are dropped, so a conditional section is just `(and test ...)`.

The vocabulary is deliberately small: `text`, `code`, `type-node`,
`brief-node`, `variable-node`, `expression-node`, `value-node`,
`paragraph`, `section`, `bullets`, `steps`, `table`, `note`. Helpers for
authors: `field` (by schema name), `scope-of`, `bindings-of`,
`binding-names`, `near-misses`, and `provenance`, which walks the failing
constraint, collects every variable in it and tabulates where each bound
came from.

That last one is the point of the whole exercise. For

```scheme
(define (twice f x) (f (f x)))
(twice string-of-int 3)
```

the explainer produces:

```
## Type mismatch
  This expression has type string but the code around it needs int.
  In (twice string-of-int 3).
  The checker was asking whether 'a can be used as (-> (-> int string) 'f).
  ## Where these constraints came from
    | variable | direction    | bound            | from                   | while checking          |
    | 'a       | accepts      | (-> 'b (-> 'd 'e)) | -                    | ...                     |
    | 'a       | must satisfy | (-> (-> int string) 'f) | (twice string-of-int 3) | 'a flows into ... |
    | 'b       | must satisfy | (-> 'c 'e)       | (f (f x))              | 'g flows into (-> 'h 'j) |
    | 'b       | must satisfy | (-> 'd 'c)       | (f x)                  | 'g flows into (-> 'i 'h) |
    | 'c       | accepts      | string           | (twice string-of-int 3) | ...                    |
    | 'c       | must satisfy | int              | (twice string-of-int 3) | ...                    |
```

The last two rows are the contradiction, and rows three and four point at
the sub-expressions *inside the definition of `twice`* that created the
demand. No snapshot can produce that.

Types in these tables are rendered *briefly* — variables as their names,
constructors one level deep — by a third printer that shares the payload's
variable naming, because the coalesced form inlines every bound and is
unreadable in a cell.

On the JS side `web/src/view.jsx` is one component per node kind, and the
default renderer is simply the one that paints `diagnostic.explanation`:

```js
export const explanationRenderer = {
  id: "explanation",
  match: (diagnostic) => Boolean(diagnostic.explanation),
  render: (diagnostic) => <View node={diagnostic.explanation} />,
};

export const defaultRenderers = [explanationRenderer, minimalRenderer];
```

A custom JS renderer still wins by sitting earlier in the list, and the
raw `fields`, `constraint` and `graph` stay in the payload as the escape
hatch for analysis that wants to run client-side. Two contracts to keep
in sync — `diagnostic-schema` for what a tag carries and the view
vocabulary for what a renderer can paint — is the cost of the split.

### What each tag carries

Field names per tag, with the kind of each value. `type`, `scope` and
`value` fields carry an `id` into `graph`; `name`, `count` and `text`
fields carry a plain `value`; `form` fields carry a parsed `form`.

| tag | fields |
| --- | --- |
| `type-mismatch` | `actual` type, `expected` type |
| `not-a-function`, `not-a-record`, `not-a-variant` | `actual` type |
| `missing-field` | `actual` type, `field` name |
| `unhandled-case` | `expected` type, `case` name |
| `case-arity` | `case` name, `used` count, `declared` count |
| `unbound-value` | `name` name, `scope` scope |
| `unbound-module-value` | `module` name, `name` name |
| `unbound-type`, `unbound-module`, `unbound-module-type`, `undefined-during-definition`, `missing-signature-type`, `missing-signature-value`, `malformed-type-name` | `name` name |
| `signature-value-mismatch` | `name` name, `actual` type, `expected` type |
| `functor-has-no-types`, `functor-has-no-values`, `functor-as-argument`, `not-a-functor` | `module` name |
| `value-not-a-record`, `value-not-a-function`, `no-matching-clause` | `value` value |
| `value-missing-field` | `value` value, `field` name |
| `unorderable` | `left` value, `right` value |
| `division-by-zero` | `operator` text |
| `unreadable-source` | `detail` text |
| `inconsistent-case-arity` | `case` name |
| `malformed-*` (28 tags) | `form` form |
| `uncomparable-function`, `malformed-if`, `malformed-field-access`, `internal` | none |

`diagnostic-schema` in `diagnostics.scm` is the source of that table, and
`data` always carries the raise arguments positionally, so a tag with no
schema still hands over everything it was given.

## Layout

| Path                       | What it is                                    |
| -------------------------- | --------------------------------------------- |
| `src/lang/types.scm`       | the type graph, `constrain`, extrusion, levels |
| `src/lang/print.scm`       | coalescing, simplification, rendering          |
| `src/lang/env.scm`         | scopes, module types, signatures, type syntax  |
| `src/lang/infer.scm`       | the typing rules, structs, sealing, functors   |
| `src/lang/eval.scm`        | values and the interpreter                     |
| `src/lang/world.scm`       | the state of the world, and how failures raise |
| `src/lang/diagnostics.scm` | every message the user can be shown            |
| `src/lang/reader.scm`      | the reader: forms, and a span for every datum  |
| `web/render/`              | the renderers: one Guile module each           |
| `src/lang/program.scm`     | top level: read forms, produce the transcript  |
| `src/main.scm`             | the Hoot entry point: the host protocol        |
| `src/lang/explain.scm`     | explainers: live diagnostic to view model      |
| `src/lang/serialize.scm`   | the diagnostic as an interned object graph     |
| `web/src/language.js`      | loads the module, throws `DiagnosticError`     |
| `web/src/view.jsx`         | one component per view-model node kind         |
| `web/src/panel.jsx`        | the diagnostics box: list or tabs               |
| `web/src/diagnostics.jsx`  | the renderer registry and the default renderer |
| `web/src/flow.jsx`         | the linked flow: hover a site, light the code  |
| `web/src/tree.jsx`         | the constraint tree, one open-path set         |
| `web/src/prose.jsx`        | the two-line story and the sentences under it  |
| `web/src/links.jsx`        | the bits a renderer needs to link to the code  |
| `web/src/highlight.js`     | the highlight bus the editor listens to        |
| `web/src/editor.jsx`       | the CodeMirror component                       |
| `web/src/main.jsx`         | the React app and its boundary                 |
| `tests/`                   | the Scheme, WebAssembly and renderer suites    |
| `dev/server.py`            | static server that sends `application/wasm`    |

Everything under `src/lang/` is portable R7RS: `#:pure` modules importing
only `(scheme base)`, `(scheme cxr)`, `(scheme char)` and
`(scheme write)`. That is what lets the same files run under native
Guile and under Hoot.

There are no comments in this repository by request; the code is meant to
read on its own, and everything that would have been a comment is here.

## How the build works

Hoot is a set of Guile modules plus a `compile-wasm` script for `guild`,
so the subcommand only exists once `GUILE_LOAD_PATH` points at them. The
whole build is one command, which `flake.nix` runs for you:

```sh
guild compile-wasm -L src -L web -o build/lang.wasm --bundle=build src/main.scm
```

`--bundle` copies Hoot's runtime (`reflect.js`, `reflect.wasm`,
`wtf8.wasm`) next to the output. `reflect.js` is a classic script that
defines the `Scheme` global, and the React bundle calls
`Scheme.load_main("lang.wasm", {reflect_wasm_dir: ".", user_imports})`.

The page itself is built by a second derivation: `buildNpmPackage` takes
`package-lock.json` and the pinned `npmDepsHash`, then esbuild bundles
`web/src/mount.jsx` into `app.js` plus the chunks it splits out. React,
CodeMirror and the language module all ship as static files; the page
fetches nothing at run time.

### The outputs

```sh
nix build              # the demo page — a static directory, the default
nix build .#bundle     # the embeddable component and its wasm runtime
nix develop            # guile, hoot, node and esbuild on PATH
nix run                # watch and serve the demo on :8080
```

Both builds run the Guile suite and the WebAssembly end-to-end tests in
their `checkPhase`, so a failing test produces no output at all.

The demo is the whole thing at once: an editor, the transcript, and every
diagnostic renderer — terse, minimal, flow, graph and tree — stacked for
one program. `.github/workflows/pages.yml` builds it on every push and
publishes `main` to GitHub Pages. Everything is referenced relatively, so
it works from a project subpath without configuration.

### Embedding it in another page

`nix build .#bundle` gives two directories:

- `component/` — `index.js`, its chunks, `index.css`, and `index.d.ts`.
  React is left external and resolves to the host's copy.
- `assets/` — `lang.wasm` and Hoot's runtime. These must be served
  verbatim at a stable URL, because `reflect.js` derives sibling paths
  like `<dir>/reflect.wasm` at run time; a hashing asset pipeline breaks
  it. Pass that URL prefix as the component's `assetBase`.

`dev/vendor.mjs --components <dir> --assets <dir>` does the same copy
into a checkout you are working on, and produces byte-identical files.

## Crossing the boundary

`src/main.scm` declares its host functions with `define-foreign`, and
`web/app.js` supplies them under the matching module names:

```scheme
(define-foreign set-text!
  "element" "setText" (ref extern) (ref string) -> none)
```

```js
element: { setText: (el, text) => { el.textContent = text; } }
```

At load time the module registers its entry point with
`procedure->external`, and the host calls it with the editor's text.
Results travel back as `(ref string)` *arguments* to `app.transcript` or
`app.diagnostic`, never as return values — a string built at run time is
a mutable Scheme string, and the reflection layer hands those to
JavaScript as opaque heap objects rather than JS strings. Scheme touches
no DOM at all, which is what lets React own the page.

## What is not here

No co-occurrence simplification, so a few printed types keep more
variables than the shortest equivalent form. No inhabitation check: a
function whose parameter is demanded to be two incompatible things at
once — `(∩ int float)`, `(∩ (tuple …) (-> …))` — is well typed and
uncallable, which is inherent to MLsub and is what makes the demo's
errors land at the call rather than the definition. Types print in the
syntax you write them in, but the round trip is not closed yet: `∪`, `∩`,
`any`, `nothing`, `rec` and `'a` are printed and not yet accepted back by
the type parser, so a printed type that mentions one of them cannot be
pasted into a signature. No `with type` constraints, no separate
compilation — a program is a
single file — and no mutation or I/O beyond what the page does with the
result.
