#!/bin/sh
set -eu
here=$(dirname "$0")
hoot=$(nix build --no-link --print-out-paths nixpkgs#guile-hoot)
export GUILE_LOAD_PATH="$hoot/share/guile/site/3.0"
export GUILE_LOAD_COMPILED_PATH="$hoot/lib/guile/3.0/site-ccache"
export GUILE_AUTO_COMPILE=0
for file in repro works; do
  printf '%s: native   ' "$file"
  nix shell nixpkgs#guile_3_0 --command guile --no-auto-compile "$here/$file.scm" \
    && printf 'ok\n'
  printf '%s: wasm     ' "$file"
  nix shell nixpkgs#guile_3_0 --command guild compile-wasm --run "$here/$file.scm" \
    2>&1 | head -1
done
