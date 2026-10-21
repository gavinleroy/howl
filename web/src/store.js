import { useCallback, useSyncExternalStore } from "react";
import { loadLanguage } from "./language.js";

// Playground state lives at module scope, keyed by id, so that separate
// React roots can share it. Astro gives every `client:*` component its own
// root, so an editor island and a diagnostic island cannot see each other
// through context — but they do import this module from the same chunk,
// which makes it the one thing they have in common. Same shape as the
// highlight bus in ./highlight.js.

const EMPTY = Object.freeze({
  seeded: false,
  source: "",
  result: null,
  ready: false,
  assetBase: "./",
  stdoutOpen: false,
  variant: null,
});

const states = new Map();
const listeners = new Map();

function listenersFor(id) {
  let found = listeners.get(id);
  if (!found) {
    found = new Set();
    listeners.set(id, found);
  }
  return found;
}

export function snapshot(id) {
  return states.get(id) || EMPTY;
}

function publish(id, next) {
  states.set(id, next);
  listenersFor(id).forEach((listener) => listener());
}

function update(id, patch) {
  publish(id, { ...snapshot(id), ...patch });
}

export function subscribe(id, listener) {
  const found = listenersFor(id);
  found.add(listener);
  return () => found.delete(listener);
}

// The server snapshot is always EMPTY: nothing is seeded until the editor
// mounts on the client, so prerendered markup and first client render
// agree.
export function useHowl(id) {
  const listen = useCallback((listener) => subscribe(id, listener), [id]);
  return useSyncExternalStore(
    listen,
    () => snapshot(id),
    () => EMPTY,
  );
}

// One runtime per asset base, shared by every playground on the page: the
// wasm module is several megabytes and the Scheme instance is already
// reused across runs.
const pending = new Map();
const runtimes = new Map();

function runtimeFor(assetBase) {
  let found = pending.get(assetBase);
  if (!found) {
    found = loadLanguage({ assetBase }).then((language) => {
      runtimes.set(assetBase, language);
      return language;
    });
    pending.set(assetBase, found);
  }
  return found;
}

export function setSource(id, source) {
  update(id, { source, seeded: true });
}

export function toggleStdout(id) {
  update(id, { stdoutOpen: !snapshot(id).stdoutOpen });
}

// Switching tabs swaps the program and runs it, so every diagnostic view
// sharing this id keeps describing what is actually on screen.
export function selectVariant(id, label, source) {
  update(id, { variant: label, source, seeded: true });
  run(id);
}

export function run(id) {
  const state = snapshot(id);
  const language = runtimes.get(state.assetBase);
  if (!language) return;
  try {
    update(id, { result: language.run(state.source) });
  } catch (failure) {
    update(id, {
      result: {
        transcript: "",
        diagnostics: [],
        host: String(failure.message || failure),
      },
    });
  }
}

// Seed the source and load the runtime. Called from the editor's mount
// effect, so it never runs during render or on the server.
export function start(id, { source, assetBase, variant = null }) {
  const state = snapshot(id);
  update(id, {
    assetBase,
    source: state.seeded ? state.source : source,
    variant: state.seeded ? state.variant : variant,
    seeded: true,
  });
  return runtimeFor(assetBase).then(
    () => {
      update(id, { ready: true });
      run(id);
    },
    (failure) => {
      update(id, {
        ready: false,
        result: {
          transcript: "",
          diagnostics: [],
          host: `could not load lang.wasm: ${failure}. The page needs a server that sends application/wasm and a browser with WebAssembly GC.`,
        },
      });
    },
  );
}
