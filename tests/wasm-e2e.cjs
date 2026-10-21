"use strict";

const path = require("path");

const buildDir = path.resolve(process.argv[2] || path.join(__dirname, "..", "build"));
const { Scheme } = require(path.join(buildDir, "reflect.js"));

const SOURCE = `(type shape (enum (Circle float) (Rect float float)))

(define (area s)
  (match s
    ((Circle r) (*. 3.14159 (*. r r)))
    ((Rect w h) (*. w h))))

(define (describe s)
  (^ "area = " (string-of-float (area s))))

(describe (Rect 3.0 4.0))`;

const handlers = {};
let pending = null;

const user_imports = {
  app: {
    register: (name, fn) => { handlers[name] = fn; },
    transcript: (text) => { pending = { transcript: text, diagnostics: [] }; },
    diagnostic: (json) => {
      if (pending === null) pending = { transcript: "", diagnostics: [] };
      pending.diagnostics.push(JSON.parse(json));
    },
  },
};

let failures = 0;

function check(name, actual, expected) {
  if (actual === expected) {
    console.log(`ok   ${name}`);
  } else {
    failures++;
    console.log(`FAIL ${name}`);
    console.log(`  expected: ${JSON.stringify(expected)}`);
    console.log(`  actual:   ${JSON.stringify(actual)}`);
  }
}

function run(source) {
  pending = null;
  handlers.run(source);
  if (pending === null) throw new Error("no result was delivered");
  return pending;
}

async function main() {
  await Scheme.load_main(path.join(buildDir, "lang.wasm"), {
    reflect_wasm_dir: buildDir,
    user_imports,
  });

  check("the module registers its entry point", typeof handlers.run, "function");

  const startup = run(SOURCE);
  check("a clean program reports no diagnostics", startup.diagnostics.length, 0);
  check("transcript contents", startup.transcript,
        "type shape = (enum (Circle float) (Rect float float))\n" +
        "val area : (-> ((enum (Circle float) (Rect float float))) float) = <fun>\n" +
        "val describe : (-> ((enum (Circle float) (Rect float float))) string) = <fun>\n" +
        '- : string = "area = 12.0"');

  check("inference", run("(fun (f x) (f (f x)))").transcript,
        "- : (-> ((-> ((∪ 'a 'b)) (∩ 'c 'a)) 'b) 'c) = <fun>");
  check("structural records", run("(fun (p) p.x)").transcript,
        "- : (-> ((record (x 'a))) 'a) = <fun>");
  check("recursive types",
        run("(define (total t) (match t (Leaf 0) ((Node l v r) (+ (total l) (+ v (total r))))))").transcript,
        "val total : (-> ((rec 'a (enum Leaf (Node 'a int 'a)))) int) = <fun>");
  check("functors",
        run("(module-type ORD (sig (type t int) (val compare (-> (t t) int))))\n" +
            "(module IntOrd : ORD (struct (type t int) (define (compare a b) (- a b))))\n" +
            "(functor MakeBounds (M : ORD) (struct (define (largest a b) (if (< (M.compare a b) 0) b a))))\n" +
            "(module IntBounds (MakeBounds IntOrd))\n" +
            "(IntBounds.largest 3 7)").transcript
          .split("\n")
          .pop(),
        "- : int = 7");

  check("a failing program produces a diagnostic",
        run('(+ 1 "a")').diagnostics.length > 0, true);

  check("the module recovers", run("(+ 1 2)").transcript, "- : int = 3");

  check("the standard library is available as Std",
        run("(Std.List.fold + 0 (Std.List.map (fun (n) (* n 2)) (Std.List.range 1 4)))")
          .transcript,
        "- : int = 12");
  check("the standard library is not opened",
        run("(list-map (fun (x) x) Nil)").diagnostics.length > 0, true);
  check("std maps work through the wasm boundary",
        run("(Std.Option.value \"\" (Std.Map.find 2 " +
            "(Std.Map.insert 2 \"two\" (Std.Map.insert 1 \"one\" Std.Map.empty))))")
          .transcript,
        '- : string = "two"');
  check("std strings work through the wasm boundary",
        run("(Std.Str.join \"-\" (Std.Str.split \",\" \"a,b,c\"))").transcript,
        '- : string = "a-b-c"');
  check("std arrays work through the wasm boundary",
        run("(Std.Option.value 0 (Std.Array.get 1 (Std.Array.set 1 99 (Std.Array.repeat 3 0))))")
          .transcript,
        "- : int = 99");

  check("a sealed abstract type cannot be taken apart",
        run("(Std.Map.insert 1 2 MapEmpty)").diagnostics.length > 0, true);
  check("its signature type is what users see",
        run("(Std.Map.insert 1 \"a\" Std.Map.empty)").transcript,
        "- : (Std.Map.t int string) = MapNode MapEmpty 1 \"a\" MapEmpty");

  const several = run("(define x (+ 1 2))\n(+ x 1)\n(area \"circle\")\n(aera 1)");
  check("checking stops at the first failure", several.diagnostics.length, 1);
  check("and the transcript keeps what ran before it", several.transcript,
        "val x : int = 3\n- : int = 4");

  if (failures > 0) {
    console.log(`\n${failures} test(s) failed`);
    process.exit(1);
  }
  console.log("\nall wasm tests passed");
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
