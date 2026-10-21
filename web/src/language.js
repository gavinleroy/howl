export class DiagnosticError extends Error {
  constructor(diagnostic) {
    super(`${diagnostic.phase} error: ${diagnostic.tag}`);
    this.name = "DiagnosticError";
    this.diagnostic = diagnostic;
  }
}

// Hoot's reflect.js is a classic script that defines a global `Scheme`; it
// has no module form, so it is injected rather than imported. One load is
// shared by every playground on the page.
let reflecting = null;

// `class Scheme {}` at the top level of a classic script binds in the
// global *lexical* scope, which a bare identifier reaches but which is not
// a property of globalThis — so `globalThis.Scheme` stays undefined no
// matter how successfully reflect.js loaded.
function schemeRuntime() {
  return typeof Scheme === "undefined" ? null : Scheme;
}

function resolveBase(assetBase) {
  const base = assetBase || "./";
  const slashed = base.endsWith("/") ? base : `${base}/`;
  return new URL(slashed, document.baseURI).href;
}

function loadReflect(base) {
  if (schemeRuntime()) return Promise.resolve();
  if (reflecting) return reflecting;
  reflecting = new Promise((resolve, reject) => {
    const script = document.createElement("script");
    script.src = `${base}reflect.js`;
    script.addEventListener("load", () => resolve());
    script.addEventListener("error", () =>
      reject(new Error(`could not load ${script.src}`)),
    );
    document.head.appendChild(script);
  });
  return reflecting;
}

export async function loadLanguage({ assetBase = "./" } = {}) {
  const base = resolveBase(assetBase);
  await loadReflect(base);

  const handlers = {};
  let pending = null;

  const user_imports = {
    app: {
      register: (name, fn) => { handlers[name] = fn; },
      transcript: (text) => {
        pending = { transcript: text, diagnostics: [] };
      },
      diagnostic: (json) => {
        if (pending === null) pending = { transcript: "", diagnostics: [] };
        pending.diagnostics.push(JSON.parse(json));
      },
    },
  };

  const runtime = schemeRuntime();
  if (!runtime) {
    throw new Error(
      `${base}reflect.js loaded but did not define Scheme`,
    );
  }

  // reflect.js builds its own asset paths as `dir + '/reflect.wasm'`, so
  // the directory it is given must not end in a slash.
  await runtime.load_main(`${base}lang.wasm`, {
    reflect_wasm_dir: base.replace(/\/$/, ""),
    user_imports,
  });

  return {
    run(source) {
      pending = null;
      handlers.run(source);
      if (pending === null) {
        throw new Error("the language module produced no result");
      }
      return pending;
    },
  };
}
