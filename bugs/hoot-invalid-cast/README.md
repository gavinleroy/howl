# Hoot miscompiles a mutually recursive walker: `invalid cast` at function entry

A function that walks a list and, in one branch, calls a helper that calls
back into it is compiled to WebAssembly that traps on **entry** — before it
examines a single list element, and even when the branch that closes the
recursion never runs. Native Guile executes the same code correctly.

- guile-hoot 0.9.0 (nixpkgs), Guile 3.0.11, aarch64-darwin
- Found while elaborating ML module signatures that can describe
  sub-modules; reduced from `src/lang/infer.scm` in this repository

## Running it

```sh
bugs/hoot-invalid-cast/run.sh
```

Expected output:

```
repro: native   ok
repro: wasm     Backtrace:
works: native   ok
works: wasm     0
```

`repro.scm` and `works.scm` differ in one expression (see below). Both run
identically under native Guile; only `repro.scm` fails under Hoot.

## The failure

```
ERROR: In procedure execute*:
  1. &wasm-runtime-error:
      instruction: (ref.cast #<<ref-type> nullable?: #f heap-type: 37>)
      position: (func 5441 0 61)
      stack: #<<wasm-stack> items: (1)>
  2. &message: "WASM runtime error: invalid cast"
```

The backtrace printer then dies in `bytevector-u8-ref`, so no Scheme frame
is reported. Instrumenting the original code with `display` showed the trap
happening between entering `view` and reading its first item, on the second
call — the one that passes `#f` where the first passed a record.

## What is needed to trigger it

Both of these, together:

1. **A recursion cycle** — `view` calls `abstract`, which calls `view`. Replace
   the recursive call with a constant and the wasm runs.
2. **An `assq` on a record field inside the recursive branch** —
   `(cdr (assq name (result-modules table)))`. `works.scm` replaces exactly
   that expression with `'known` and the wasm runs. Everything else is
   identical.

Neither alone is enough: a bare mutually recursive walker over records
compiles and runs fine.

Things that turned out **not** to matter, each tested separately: the
number of named-let bindings; whether the walker returns a list or a
record; the function's arity; whether the recursive branch uses `when`;
whether the cycle is direct or passes through a wrapper function.

## Workaround

Route one edge of the cycle through a cell that is filled after both
functions are defined, so the call is not statically part of the cycle:

```scheme
(define %entry (vector #f))

(define (call-entry item scope table prefix)
  ((vector-ref %entry 0) item scope table prefix))

(vector-set! %entry 0 resolve-entry)
```

That is what `signature-module-entry` in `src/lang/infer.scm` does, and the
indirection is load-bearing: inline the call and the WebAssembly build
starts trapping again while every native test still passes.
