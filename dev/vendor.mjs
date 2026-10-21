#!/usr/bin/env node
// Copy the playground into a site.
//
//   nix develop --command node dev/vendor.mjs --components <dir> --assets <dir>
//
// --components  where the JS/CSS/d.ts go (imported by your component code)
// --assets      where the wasm runtime goes (must be served verbatim at a
//               stable URL; reflect.js derives sibling paths like
//               `<dir>/reflect.wasm` at runtime, so hashed asset pipelines
//               such as Vite's `?url` will not work here)
//
// Re-run to update. Chunk names are content hashed, so the component
// directory is cleared first.

import { spawnSync } from "node:child_process";
import fs from "node:fs";
import path from "node:path";
import process from "node:process";

const RUNTIME = ["lang.wasm", "reflect.js", "reflect.wasm", "wtf8.wasm"];


const root = path.resolve(import.meta.dirname, "..");

function die(message, hint) {
  console.error(`vendor: ${message}`);
  if (hint) console.error(`        ${hint}`);
  process.exit(1);
}

function flag(name) {
  const at = process.argv.indexOf(`--${name}`);
  return at === -1 ? null : process.argv[at + 1];
}

const components = flag("components");
const assets = flag("assets");
if (!components || !assets) {
  die("usage: node dev/vendor.mjs --components <dir> --assets <dir>");
}
const componentDir = path.resolve(components);
const assetDir = path.resolve(assets);

// The wasm runtime comes from guild, which needs the nix environment; this
// script does not try to reproduce that.
const missing = RUNTIME.filter(
  (name) => !fs.existsSync(path.join(root, "build", name)),
);
if (missing.length) {
  die(
    `build/ is missing ${missing.join(", ")}`,
    "run: guild compile-wasm -L src -L web -o build/lang.wasm --bundle=build src/main.scm",
  );
}

const dist = path.join(root, "dist");
fs.rmSync(dist, { recursive: true, force: true });

const built = spawnSync(
  "esbuild",
  [
    "web/src/index.jsx",
    "--bundle",
    "--splitting",
    "--format=esm",
    "--jsx=automatic",
    "--outdir=dist",
    "--external:react",
    "--external:react-dom",
    "--minify",
  ],
  { cwd: root, stdio: "inherit" },
);
if (built.error || built.status !== 0) {
  die("the library build failed", "is esbuild on PATH? try: nix develop");
}

fs.rmSync(componentDir, { recursive: true, force: true });
fs.mkdirSync(componentDir, { recursive: true });
fs.mkdirSync(assetDir, { recursive: true });

for (const name of fs.readdirSync(dist)) {
  fs.copyFileSync(path.join(dist, name), path.join(componentDir, name));
}
fs.copyFileSync(path.join(root, "web", "src", "index.d.ts"),
                path.join(componentDir, "index.d.ts"));
for (const name of RUNTIME) {
  fs.copyFileSync(path.join(root, "build", name), path.join(assetDir, name));
}

const kb = (file) => (fs.statSync(file).size / 1024).toFixed(0);
const mb = (file) => (fs.statSync(file).size / 1024 / 1024).toFixed(1);

console.log(`
vendored

  ${componentDir}
      index.js   ${kb(path.join(componentDir, "index.js"))} KB loaded up front
      index.css  scoped under .howl-root
      index.d.ts types for tsc
      + ${fs.readdirSync(dist).length - 2} chunks, incl. the graph renderer (loaded on demand)

  ${assetDir}
      ${RUNTIME.join(", ")}   (lang.wasm is ${mb(path.join(assetDir, "lang.wasm"))} MB)

Serve the asset directory verbatim and pass its URL prefix as assetBase.
`);
