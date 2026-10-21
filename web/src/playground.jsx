import React, { useCallback, useEffect } from "react";
import { Editor } from "./editor.jsx";
import { DiagnosticsPanel } from "./panel.jsx";
import { DiagnosticBoundary, RENDERERS } from "./diagnostics.jsx";
import { EXAMPLES, INITIAL_SOURCE } from "./examples.js";
import {
  run,
  selectVariant,
  setSource,
  start,
  toggleStdout,
  useHowl,
} from "./store.js";

// The playground as separable pieces. Each one is its own island when used
// from Astro — they find each other through the keyed store in ./store.js,
// not through React context, which cannot cross roots.

const DEFAULT_ID = "default";

function Transcript({ text }) {
  if (!text) {
    return <p className="placeholder">this program printed nothing</p>;
  }
  return <pre className="transcript">{text}</pre>;
}

export function HowlEditor({
  id = DEFAULT_ID,
  assetBase = "./",
  initialSource = INITIAL_SOURCE,
  examples = EXAMPLES,
  showStdout = true,
  readOnly = false,
  tabs = null,
}) {
  const state = useHowl(id);

  // `tabs` is label -> program. Object key order is insertion order for
  // string keys, so the tabs appear as written.
  const labels = tabs ? Object.keys(tabs) : [];
  const opening = labels.length ? tabs[labels[0]] : initialSource;
  const active = state.variant ?? labels[0] ?? null;

  useEffect(() => {
    start(id, { source: opening, assetBase, variant: labels[0] ?? null });
  }, [id, assetBase, opening]);

  const edit = useCallback((text) => setSource(id, text), [id]);
  const trigger = useCallback(() => run(id), [id]);

  const choose = useCallback(
    (example) => {
      setSource(id, example.source);
      run(id);
    },
    [id],
  );

  const pick = useCallback(
    (label) => selectVariant(id, label, tabs[label]),
    [id, tabs],
  );

  // Until the mount effect seeds the store, show the source the caller
  // asked for: the server renders this too, so the markup matches.
  const source = state.seeded ? state.source : opening;

  // A read-only editor is a second view of a program someone else owns:
  // its state is shared by id, so its own Run and stdout would act on the
  // same program the editable one already drives. It keeps the tabs,
  // which choose what is being read, and drops the rest.
  const controls = !readOnly;

  return (
    <div className="howl-root">
      <section className="panel">
        {controls || labels.length > 1 ? (
        <div className="toolbar">
          {controls ? (
          <button type="button" onClick={trigger} disabled={!state.ready}>
            Run
          </button>
          ) : null}
          {controls && examples && examples.length ? (
            <span className="examples">
              {examples.map((example) => (
                <button
                  key={example.name}
                  type="button"
                  className="example"
                  onClick={() => choose(example)}
                >
                  {example.name}
                </button>
              ))}
            </span>
          ) : null}
          {controls && showStdout ? (
            <button
              type="button"
              className={state.stdoutOpen ? "active" : ""}
              aria-pressed={state.stdoutOpen}
              onClick={() => toggleStdout(id)}
            >
              stdout
            </button>
          ) : null}
          {labels.length > 1 ? (
            <span className="tab-strip variants" role="tablist">
              {labels.map((label) => (
                <button
                  key={label}
                  type="button"
                  role="tab"
                  aria-selected={label === active}
                  className={label === active ? "tab active" : "tab"}
                  onClick={() => pick(label)}
                >
                  {label}
                </button>
              ))}
            </span>
          ) : null}
        </div>
        ) : null}

        <Editor
          id={id}
          value={source}
          onChange={edit}
          onRun={trigger}
          readOnly={readOnly}
        />

        {showStdout && state.stdoutOpen ? (
          <Transcript text={state.result ? state.result.transcript : ""} />
        ) : null}

        {state.result && state.result.host ? (
          <p className="diagnostic-text">{state.result.host}</p>
        ) : null}
      </section>
    </div>
  );
}

// One diagnostic, in one form. `index` picks which diagnostic (default the
// first); pass "all" for every one. Renders nothing when the program has no
// diagnostics, so prose around it closes up.
export function HowlDiagnostic({
  id = DEFAULT_ID,
  form = "minimal",
  index = 0,
}) {
  const { result } = useHowl(id);
  const renderer = RENDERERS[form];
  if (!renderer) return null;

  const all = result ? result.diagnostics : [];
  const matching = all.filter((diagnostic) => renderer.match(diagnostic));
  const chosen =
    index === "all" ? matching : matching.slice(index, index + 1);
  if (chosen.length === 0) return null;

  return (
    <div className="howl-root">
      {chosen.map((diagnostic, position) => (
        <DiagnosticBoundary
          key={position}
          resetKey={position}
          renderers={[renderer]}
        >
          <div className="diagnostic" data-tag={diagnostic.tag}>
            {renderer.render(diagnostic, id)}
          </div>
        </DiagnosticBoundary>
      ))}
    </div>
  );
}

export function HowlStdout({ id = DEFAULT_ID }) {
  const { result } = useHowl(id);
  return (
    <div className="howl-root">
      <Transcript text={result ? result.transcript : ""} />
    </div>
  );
}

// Everything at once, the shape the standalone page has always had.
export function Playground({
  id = DEFAULT_ID,
  assetBase = "./",
  initialSource = INITIAL_SOURCE,
  examples = EXAMPLES,
  renderers = null,
  initialLayout = "all",
}) {
  const { result } = useHowl(id);
  const diagnostics = result ? result.diagnostics : [];

  return (
    <div className="howl-root app">
      <main>
        <HowlEditor
          id={id}
          assetBase={assetBase}
          initialSource={initialSource}
          examples={examples}
        />
        <DiagnosticsPanel
          diagnostics={diagnostics}
          renderers={renderers}
          initialLayout={initialLayout}
          id={id}
        />
      </main>
    </div>
  );
}
