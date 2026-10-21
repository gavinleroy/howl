import React, { Suspense } from "react";
import { DiagnosticError } from "./language.js";
// import { View } from "./view.jsx";
import { FlowView } from "./flow.jsx";
import { FlowAside } from "./flow2.jsx";
import { ConstraintTree } from "./tree.jsx";
// import { Prose } from "./prose.jsx";

// The graph renderer pulls in spytial-core and its dependencies — roughly
// 2.4 MB of the bundle, against a tree renderer that costs nothing beyond
// React. Loading it on demand keeps that weight off any page that never
// opens a graph view.
const ConstraintGraph = React.lazy(() =>
  import("./graph.jsx").then((module) => ({ default: module.ConstraintGraph })),
);

// export function field(diagnostic, key) {
//   return diagnostic.fields ? diagnostic.fields[key] : undefined;
// }
//
// export function fieldText(diagnostic, key) {
//   const found = field(diagnostic, key);
//   if (!found) return undefined;
//   if (found.kind === "name" || found.kind === "count" || found.kind === "text") {
//     return String(found.value);
//   }
//   return found.text;
// }
//
// export function lookupType(diagnostic, id) {
//   return diagnostic.graph && id ? diagnostic.graph.types[id] : undefined;
// }
//
// export function lookupScope(diagnostic, id) {
//   return diagnostic.graph && id ? diagnostic.graph.scopes[id] : undefined;
// }
//
// export function lookupValue(diagnostic, id) {
//   return diagnostic.graph && id ? diagnostic.graph.values[id] : undefined;
// }
//
// export function fieldType(diagnostic, key) {
//   const found = field(diagnostic, key);
//   return found && found.kind === "type" ? lookupType(diagnostic, found.id) : undefined;
// }
//
// export function bindingsInScope(diagnostic, key = "scope") {
//   const found = field(diagnostic, key);
//   const scope = found
//     ? lookupScope(diagnostic, found.id)
//     : lookupScope(diagnostic, diagnostic.world && diagnostic.world.scope);
//   return scope ? scope.values : [];
// }
//
// export function constraintsOn(diagnostic, id) {
//   const node = lookupType(diagnostic, id);
//   if (!node || node.kind !== "var") return { lower: [], upper: [] };
//   const resolve = (bound) => ({
//     type: lookupType(diagnostic, bound.type),
//     origin: bound.origin,
//   });
//   return {
//     lower: node.lower.map(resolve),
//     upper: node.upper.map(resolve),
//   };
// }

// export const explanationRenderer = {
//   id: "explanation",
//   match: (diagnostic) => Boolean(diagnostic.explanation),
//   render: (diagnostic) => <View node={diagnostic.explanation} />,
// };

function textRenderer(id, style) {
  return {
    id,
    match: (diagnostic) => Boolean(diagnostic.renderings),
    render: (diagnostic) => (
      <pre className="diagnostic-text">{diagnostic.renderings[style]}</pre>
    ),
  };
}

export const terseRenderer = textRenderer("terse", "terse");
export const minimalRenderer = textRenderer("minimal", "minimal");

export const flowRenderer = {
  id: "flow",
  match: (diagnostic) => Boolean(diagnostic.flow),
  render: (diagnostic, id) => <FlowView flow={diagnostic.flow} id={id} />,
};

// The same flow, read beside the code rather than drawn over it. Both
// renderers live off one payload, so the design can be compared without
// touching the compiler.
export const flowAsideRenderer = {
  id: "flow2",
  match: (diagnostic) => Boolean(diagnostic.flow),
  render: (diagnostic, id) => <FlowAside flow={diagnostic.flow} id={id} />,
};

// export const linkedRenderer = {
//   id: "linked",
//   match: (diagnostic) => Boolean(diagnostic.flow),
//   render: (diagnostic) => <FlowLinks flow={diagnostic.flow} />,
// };

export const treeRenderer = {
  id: "tree",
  match: (diagnostic) => Boolean(diagnostic.tree),
  render: (diagnostic, id) => <ConstraintTree tree={diagnostic.tree} id={id} />,
};

export const graphRenderer = {
  id: "graph",
  match: (diagnostic) => Boolean(diagnostic.constraints),
  render: (diagnostic) => (
    <Suspense
      fallback={<p className="placeholder">loading the graph renderer…</p>}
    >
      <ConstraintGraph
        constraints={diagnostic.constraints}
        graph={diagnostic.graph}
      />
    </Suspense>
  ),
};

// export const proseRenderer = {
//   id: "prose",
//   match: (diagnostic) => Boolean(diagnostic.prose),
//   render: (diagnostic) => <Prose prose={diagnostic.prose} />,
// };

export const defaultRenderers = [
  terseRenderer,
  minimalRenderer,
  flowRenderer,
  graphRenderer,
  treeRenderer,
  // proseRenderer,
  // explanationRenderer,
];

// Named forms, so an embed can ask for one by string: <HowlDiagnostic
// form="tree" />.
export const RENDERERS = {
  terse: terseRenderer,
  minimal: minimalRenderer,
  flow: flowRenderer,
  flow2: flowAsideRenderer,
  graph: graphRenderer,
  tree: treeRenderer,
};

export function where(diagnostic) {
  if (diagnostic.expression) return diagnostic.expression.text;
  if (diagnostic.world && diagnostic.world.form) return diagnostic.world.form.text;
  return undefined;
}

export function DiagnosticView({ diagnostic, renderers, layout = "all", id }) {
  const matching = renderers.filter((r) => r.match(diagnostic));
  const [active, setActive] = React.useState(0);
  const current = Math.min(active, matching.length - 1);
  const shown = layout === "tabs" ? matching.slice(current, current + 1) : matching;
  return (
    <div className="diagnostic" data-tag={diagnostic.tag}>
      {layout === "tabs" ? (
        <div className="tab-strip" role="tablist">
          {matching.map((r, index) => (
            <button
              key={r.id}
              type="button"
              role="tab"
              aria-selected={index === current}
              className={index === current ? "tab active" : "tab"}
              onClick={() => setActive(index)}
            >
              <span className="tab-label">{r.id}</span>
            </button>
          ))}
        </div>
      ) : null}
      {shown.map((r) => (
        <div key={r.id} className="renderer-entry">
          {layout === "all" ? <h3 className="renderer-name">{r.id}</h3> : null}
          {r.render(diagnostic, id)}
        </div>
      ))}
    </div>
  );
}

export class DiagnosticBoundary extends React.Component {
  constructor(props) {
    super(props);
    this.state = { failure: null };
  }

  static getDerivedStateFromError(failure) {
    return { failure };
  }

  componentDidUpdate(previous) {
    if (previous.resetKey !== this.props.resetKey && this.state.failure) {
      this.setState({ failure: null });
    }
  }

  render() {
    const { failure } = this.state;
    if (!failure) return this.props.children;
    if (failure instanceof DiagnosticError) {
      return (
        <DiagnosticView
          diagnostic={failure.diagnostic}
          renderers={defaultRenderers}
        />
      );
    }
    return (
      <div className="diagnostic" data-tag="renderer-failure">
        <div className="diagnostic-head">
          <span className="diagnostic-phase">renderer error</span>
        </div>
        <p className="diagnostic-text">{String(failure.message || failure)}</p>
      </div>
    );
  }
}
