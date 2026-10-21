import React, { useState } from "react";
import {
  DiagnosticBoundary,
  DiagnosticView,
  defaultRenderers,
} from "./diagnostics.jsx";

export function DiagnosticsPanel({
  diagnostics,
  renderers,
  initialLayout = "all",
  id,
}) {
  const [layout, setLayout] = useState(initialLayout);
  const chosen = renderers || defaultRenderers;

  if (diagnostics.length === 0) return null;

  return (
    <section className="panel diagnostics-panel">
      <div className="panel-head">
        <div className="layout-toggle" role="group" aria-label="Renderer layout">
          <button
            type="button"
            className={layout === "all" ? "active" : ""}
            aria-pressed={layout === "all"}
            onClick={() => setLayout("all")}
          >
            All
          </button>
          <button
            type="button"
            className={layout === "tabs" ? "active" : ""}
            aria-pressed={layout === "tabs"}
            onClick={() => setLayout("tabs")}
          >
            Tabs
          </button>
        </div>
      </div>

      <div className="diagnostic-list">
        {diagnostics.map((diagnostic, index) => (
          <DiagnosticBoundary key={index} resetKey={index} renderers={chosen}>
            <DiagnosticView
              diagnostic={diagnostic}
              renderers={chosen}
              layout={layout}
              id={id}
            />
          </DiagnosticBoundary>
        ))}
      </div>
    </section>
  );
}
