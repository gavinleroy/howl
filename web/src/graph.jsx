import React, { useEffect, useRef } from "react";
import {
  JSONDataInstance,
  LayoutInstance,
  SGraphQueryEvaluator,
  parseLayoutSpec,
} from "spytial-core";

// Types are nodes; a directed edge a -> b is the claim `a <: b` some
// derivation step asserted. Flow runs top-down, so concrete types sit at
// the rim: sources are the values that flowed in, sinks the demands. The
// failing claim is drawn as its own red `conflict` relation. The full
// derivation (steps + causes) rides along in the payload for anything
// that wants explicit provenance.
const SPEC = `
constraints:
  - orientation: { selector: subtype, directions: [below] }
  - orientation: { selector: conflict, directions: [below] }
directives:
  - atomStyle: { selector: univ, borderStyle: { color: "#8a8a86" } }
  - edgeStyle: { field: conflict, lineStyle: { color: "#cc2222", weight: 3 } }
  - flag: hideDisconnectedBuiltIns
`;

// One atom per type id (variables and concrete types drawn as distinct
// atom types, labelled briefly — the bound-expanded text stays available
// in the shared type table), one tuple per subtyping edge.
function dataOf(constraints, graph) {
  const types = (graph && graph.types) || {};
  const label = (id) =>
    (types[id] && (types[id].brief || types[id].text)) || id;
  const kind = (id) =>
    types[id] && types[id].kind === "var" ? "Var" : "Type";

  const tuple = (edge) => ({
    atoms: [edge.from, edge.to],
    types: [kind(edge.from), kind(edge.to)],
  });

  return {
    atoms: constraints.nodes.map((id) => ({
      id,
      type: kind(id),
      label: label(id),
    })),
    relations: [
      {
        id: "subtype",
        name: "subtype",
        types: ["univ", "univ"],
        tuples: constraints.edges.filter((edge) => !edge.failing).map(tuple),
      },
      {
        id: "conflict",
        name: "conflict",
        types: ["univ", "univ"],
        tuples: constraints.edges.filter((edge) => edge.failing).map(tuple),
      },
    ],
  };
}

export function ConstraintGraph({ constraints, graph }) {
  const ref = useRef(null);

  useEffect(() => {
    ref.current.setViewOptions({ toolbar: "none" });
    const instance = new JSONDataInstance(dataOf(constraints, graph));
    const evaluator = new SGraphQueryEvaluator();
    evaluator.initialize({ sourceData: instance });
    const generated = new LayoutInstance(
      parseLayoutSpec(SPEC),
      evaluator,
    ).generateLayout(instance);
    ref.current.renderLayout(generated.layout);
  }, [constraints, graph]);

  return <webcola-cnd-graph ref={ref} className="constraint-spytial" />;
}
