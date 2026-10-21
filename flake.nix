{
  description = "A Hindley-Milner language with records, enums and ML modules, written in Guile Scheme and compiled to WebAssembly with Hoot";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { self, nixpkgs, flake-utils }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      overlay = final: prev: {
        # mit-scheme is declared but never used, and it's Linux-only
        guile-irregex = prev.guile-irregex.overrideAttrs (old: {
          nativeBuildInputs =
            builtins.filter (d: (d.pname or "") != "mit-scheme") old.nativeBuildInputs;
        });
        # Guile 3.0.11's warnings-analysis pass segfaults on aarch64-darwin; -W0 skips it
        guile-srfi-180 = prev.guile-srfi-180.overrideAttrs (old: {
          buildPhase =
            builtins.replaceStrings [ "guild compile" ] [ "guild compile -W0" ] old.buildPhase;
        });
        guile-lsp-server = prev.guile-lsp-server.overrideAttrs (old: {
          makeFlags = (old.makeFlags or [ ]) ++ [ "GUILE_WARNINGS=-W0" ];
        });
      };
    in
    flake-utils.lib.eachSystem systems (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ overlay ];
        };

        sources = nixpkgs.lib.cleanSourceWith {
          src = ./.;
          filter =
            path: type:
            !(builtins.elem (baseNameOf path) [
              "node_modules"
              "build"
              "dist"
              "result"
              ".git"
            ]);
        };

        hootEnv = ''
          export GUILE_LOAD_PATH="${pkgs.guile-hoot}/share/guile/site/3.0"
          export GUILE_LOAD_COMPILED_PATH="${pkgs.guile-hoot}/lib/guile/3.0/site-ccache"
          export GUILE_AUTO_COMPILE=0
        '';

        ui = pkgs.buildNpmPackage {
          pname = "little-ml-ui";
          version = "0.1.0";

          src = sources;

          npmDepsHash = "sha256-aLyC57VXFTco2IR2UF6I6krQqVpIsZ0cI3e2Vz+uBEU=";

          nativeBuildInputs = [ pkgs.esbuild ];

          buildPhase = ''
            runHook preBuild

            esbuild web/src/mount.jsx --bundle --splitting --format=esm \
              --jsx=automatic --entry-names=app --outdir=dist --minify

            runHook postBuild
          '';

          installPhase = ''
            runHook preInstall

            mkdir -p $out
            cp dist/*.js $out/

            runHook postInstall
          '';
        };

        # The embeddable build: the playground as an ES module with React
        # left external, for a host page that already has its own. This is
        # what gets vendored into a blog post.
        component = pkgs.buildNpmPackage {
          pname = "little-ml-component";
          version = "0.1.0";

          src = sources;

          npmDepsHash = "sha256-aLyC57VXFTco2IR2UF6I6krQqVpIsZ0cI3e2Vz+uBEU=";

          nativeBuildInputs = [ pkgs.esbuild ];

          buildPhase = ''
            runHook preBuild

            esbuild web/src/index.jsx --bundle --splitting --format=esm \
              --jsx=automatic --outdir=dist --minify \
              --external:react --external:react-dom

            runHook postBuild
          '';

          installPhase = ''
            runHook preInstall

            mkdir -p $out
            cp dist/* $out/
            cp web/src/index.d.ts $out/

            runHook postInstall
          '';

          meta = {
            description = "The playground as an embeddable ES module";
            platforms = systems;
          };
        };

        site = pkgs.stdenv.mkDerivation {
          pname = "little-ml";
          version = "0.1.0";

          src = sources;

          nativeBuildInputs = [
            pkgs.guile_3_0
            pkgs.guile-hoot
            pkgs.nodejs
          ];

          configurePhase = ''
            runHook preConfigure

            ${hootEnv}

            runHook postConfigure
          '';

          buildPhase = ''
            runHook preBuild

            mkdir -p build
            guild compile-wasm -L src -L web -o build/lang.wasm --bundle=build src/main.scm
            cp web/index.html web/style.css web/page.css build/
            cp ${ui}/*.js build/
            # GitHub Pages otherwise runs the output through Jekyll.
            touch build/.nojekyll

            runHook postBuild
          '';

          doCheck = true;

          checkPhase = ''
            runHook preCheck

            guile -L src -L web tests/run-tests.scm
            node tests/wasm-e2e.cjs build

            runHook postCheck
          '';

          installPhase = ''
            runHook preInstall

            mkdir -p $out
            cp build/* $out/
            cp build/.nojekyll $out/

            runHook postInstall
          '';

          meta = {
            description = "Static site: type inference, records, enums and functors running in WebAssembly";
            platforms = systems;
          };
        };
      in
      {
        # The demo page is the default: a program, its transcript, and
        # every diagnostic renderer at once.
        packages.default = site;
        packages.demo = site;

        # The embeddable bundle, split the way a host page consumes it:
        # `component/` is imported, `assets/` is served verbatim and named
        # by the component's assetBase.
        packages.bundle = pkgs.runCommand "little-ml-bundle" { } ''
          mkdir -p $out/component $out/assets
          cp ${component}/* $out/component/
          for asset in lang.wasm reflect.js reflect.wasm wtf8.wasm; do
            cp ${site}/$asset $out/assets/
          done
        '';

        devShells.default = pkgs.mkShell {
          packages = [
            pkgs.guile_3_0
            pkgs.guile-hoot
            pkgs.guile-lsp-server
            pkgs.nodejs
            pkgs.esbuild
          ];
          shellHook = ''
            export GUILE_LOAD_PATH="${pkgs.guile-hoot}/share/guile/site/3.0''${GUILE_LOAD_PATH:+:$GUILE_LOAD_PATH}"
            export GUILE_LOAD_COMPILED_PATH="${pkgs.guile-hoot}/lib/guile/3.0/site-ccache''${GUILE_LOAD_COMPILED_PATH:+:$GUILE_LOAD_COMPILED_PATH}"
            export GUILE_AUTO_COMPILE=0
          '';
        };

        apps.default = {
          type = "app";
          program = "${pkgs.writeShellScript "watch-little-ml" ''
            ${hootEnv}
            export WATCH_GUILD="${pkgs.guile_3_0}/bin/guild"
            export WATCH_ESBUILD="${pkgs.esbuild}/bin/esbuild"

            exec ${pkgs.guile_3_0}/bin/guile -s ${./dev/watch.scm} "''${1:-8080}"
          ''}";
          meta.description = "Serve ./build on http://localhost:8080, rebuilding on Guile/React/HTML/CSS changes";
        };
      }
    );
}
