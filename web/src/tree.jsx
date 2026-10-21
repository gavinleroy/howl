import React, { useState } from "react";
import { hoverSpans } from "./highlight.js";

// Directory-tree view of the provenance analysis: the Scheme side sends
// fully-phrased lines ({id, text, ref, site, marks, children}); this
// component only lays them out as an expandable tree. Roots start open.
// Hovering a line highlights its source marks in the editor.
//
// Keys carry the line's text, not just its position: diagnostics are
// keyed by index upstream, so a positional key would hand one program's
// expansion state to the next program's tree.
function keyOf(node, index) {
  return `${index}-${node.text}`;
}

function Line({ node, depth, initiallyOpen, id }) {
  const [open, setOpen] = useState(initiallyOpen);
  const kids = node.children || [];
  const marks = node.marks || [];

  const enter = () => {
    if (marks.length) hoverSpans(id, marks.flatMap((mark) => mark.spans));
  };
  const leave = () => {
    if (marks.length) hoverSpans(id, []);
  };

  return (
    <div className="tree-node">
      <div
        className="tree-line"
        style={{ paddingLeft: `${depth * 1.25}rem` }}
        onMouseEnter={enter}
        onMouseLeave={leave}
      >
        {kids.length ? (
          <button
            type="button"
            className="tree-toggle"
            aria-expanded={open}
            onClick={() => setOpen(!open)}
          >
            {open ? "▾" : "▸"}
          </button>
        ) : (
          <span className="tree-toggle tree-leaf" />
        )}
        <code className="tree-label">{node.text}</code>
      </div>
      {open &&
        kids.map((kid, index) => (
          <Line
            key={keyOf(kid, index)}
            node={kid}
            depth={depth + 1}
            initiallyOpen={false}
            id={id}
          />
        ))}
    </div>
  );
}

export function ConstraintTree({ tree, id }) {
  return (
    <div className="constraint-tree">
      {tree.map((node, index) => (
        <Line
          key={keyOf(node, index)}
          node={node}
          depth={0}
          initiallyOpen={true}
          id={id}
        />
      ))}
    </div>
  );
}
