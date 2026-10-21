import React, { useCallback, useEffect, useRef, useState } from "react";
import { Editor } from "./editor.jsx";
import { hoverSpans } from "./highlight.js";

// The same flow payload as flow.jsx, read beside the code instead of
// drawn over it. One flow at a time; each of its steps is a card in the
// rail, sitting at the height of the line it happens on. Alignment does
// what the arrows did — a card and its code are the same distance down
// the page — so the editor is left to be read as code.
//
// Everything here is iteration surface. The contract with the compiler is
// only the shape rendered by web/render/flow.scm: flows -> hops, each hop
// carrying sub/sup, a shape, and the spans behind each side.

const SLOTS = 3;

function slotOf(type, order) {
  const at = order.indexOf(type);
  return at === -1 || at >= SLOTS ? "x" : String(at + 1);
}

function lineOf(source, span) {
  if (!span || typeof span.from !== "number") return null;
  return source.slice(0, span.from).split("\n").length;
}

// The middle of the line a card is about, in the track's own coordinates
// — the track is what cards are positioned inside, so measuring against
// anything else puts every card off by the distance between the two.
function anchorOf(view, track, spans) {
  const span = spans && spans[0];
  if (!view || !span) return null;
  const at = view.coordsAtPos(span.from);
  if (!at) return null;
  return (at.top + at.bottom) / 2 - track.top;
}

const GAP = 6;

// Cards keep their order and never overlap: each is centred on its line,
// then pushed down past whatever already sits there. A leader line spans
// whatever gap that opens, so a displaced card still points at its line.
function settle(centres, heights) {
  const placed = [];
  let floor = 0;
  centres.forEach((centre, at) => {
    const height = heights[at] || 0;
    const wanted = centre === null ? floor : centre - height / 2;
    const top = Math.max(wanted, floor);
    placed.push(top);
    floor = top + height + GAP;
  });
  return placed;
}

export function FlowAside({ flow, id = "default" }) {
  const host = useRef(null);
  const view = useRef(null);
  const track = useRef(null);
  const cards = useRef([]);
  const [tops, setTops] = useState([]);
  const [leaders, setLeaders] = useState([]);
  const [chosen, setChosen] = useState(0);
  const [active, setActive] = useState(null);

  const flows = Array.isArray(flow && flow.flows) ? flow.flows : null;
  const signatures = Array.isArray(flow && flow.signatures)
    ? flow.signatures
    : [];

  const order = [];
  for (const path of flows || []) {
    if (!order.includes(path.type)) order.push(path.type);
  }

  const path = flows && flows[Math.min(chosen, flows.length - 1)];
  const hops = (path && path.hops) || [];

  const measure = useCallback(() => {
    const box = track.current;
    if (!box || !view.current) return;
    const rect = box.getBoundingClientRect();
    const wanted = hops.map((hop) => anchorOf(view.current, rect, hop.spans));
    const heights = cards.current.map((el) => (el ? el.offsetHeight : 0));
    const next = settle(wanted, heights);
    setTops((previous) =>
      previous.length === next.length && previous.every((v, i) => v === next[i])
        ? previous
        : next,
    );
    // Only cards that had to move carry a leader; the rest sit on their
    // own line and need nothing drawn.
    const drawn = wanted
      .map((want, at) => ({
        at,
        from: want,
        to: next[at] + (heights[at] || 0) / 2,
      }))
      .filter(
        (leader) =>
          leader.from !== null && Math.abs(leader.from - leader.to) > 2,
      );
    setLeaders((previous) =>
      previous.length === drawn.length &&
      previous.every(
        (leader, i) =>
          leader.from === drawn[i].from && leader.to === drawn[i].to,
      )
        ? previous
        : drawn,
    );
  }, [hops]);

  useEffect(() => {
    measure();
    const box = host.current;
    const scroller = box && box.querySelector(".cm-scroller");
    const again = () => measure();
    window.addEventListener("resize", again);
    if (scroller) scroller.addEventListener("scroll", again);
    // The editor lays out after its first paint; one late pass catches it.
    const settleIn = setTimeout(again, 60);
    return () => {
      window.removeEventListener("resize", again);
      if (scroller) scroller.removeEventListener("scroll", again);
      clearTimeout(settleIn);
    };
  }, [measure]);

  const enter = (at, hop) => {
    setActive(at);
    hoverSpans(id, [
      ...(hop.subSpans || []).map((span) => ({ ...span, role: "sub" })),
      ...(hop.supSpans || []).map((span) => ({ ...span, role: "sup" })),
    ]);
  };
  const leave = () => {
    setActive(null);
    hoverSpans(id, []);
  };

  if (!flows) {
    return (
      <div className="howl-root">
        <p className="placeholder">
          this diagnostic came from an older build of the compiler than the
          flow view; rebuild lang.wasm
        </p>
      </div>
    );
  }

  const hue = path ? slotOf(path.type, order) : "x";
  const named = (which) =>
    (signatures.find((row) => row.id === which) || {}).name || "?";

  return (
    <div className="howl-root flow2" ref={host}>
      {/* Headings sit in their own row above both columns, so the track
          starts level with the editor's first line and a card can be put
          exactly on its line rather than pushed down past a legend. */}
      <div className="flow2-legend">
        <h4 className="flow2-head">Concrete Types</h4>
        {/* Which flow is being read. One per concrete type that reaches
            the conflict, named by where it starts. */}
        <div className="flow2-picker">
          {flows.map((one, at) => (
            <button
              type="button"
              key={`${one.type}-${one.signature}-${one.slot}-${at}`}
              className={`flow2-chip flow-hue-${slotOf(one.type, order)}${
                at === Math.min(chosen, flows.length - 1)
                  ? " flow2-chip-on"
                  : ""
              }`}
              onClick={() => setChosen(at)}
            >
              <code>{one.type}</code>
              <span className="flow2-chip-from">{named(one.signature)}</span>
            </button>
          ))}
        </div>
        {path ? (
          <h4 className="flow2-head flow2-head-flows">
            Flows from <code>{path.type}</code>
          </h4>
        ) : null}
      </div>

      <div className="flow2-code">
        <Editor
          id={id}
          value={flow.source}
          onChange={() => {}}
          onRun={() => {}}
          readOnly={true}
          viewRef={view}
        />
      </div>

      <div className="flow2-rail">
        <div className="flow2-track" ref={track}>
          {/* Leaders live inside the rail, never over the code. */}
          <svg className="flow2-leaders" aria-hidden="true">
            {leaders.map((leader) => (
              <polyline
                key={leader.at}
                className={`flow2-leader flow-hue-${hue}`}
                points={`0,${leader.from} 8,${leader.from} 14,${leader.to} 22,${leader.to}`}
              />
            ))}
          </svg>

          {hops.map((hop, at) => {
            const line = lineOf(flow.source, (hop.spans || [])[0]);
            const top = tops[at];
            return (
              <div
                key={`${hop.sub}-${hop.sup}-${at}`}
                ref={(el) => {
                  cards.current[at] = el;
                }}
                className={`flow2-card flow-hue-${hue}${
                  active === at ? " flow2-card-on" : ""
                }`}
                style={top === undefined ? undefined : { top: `${top}px` }}
                onMouseEnter={() => enter(at, hop)}
                onMouseLeave={leave}
              >
                <div className="flow2-card-head">
                  <span className="flow2-line">{line ? `line ${line}` : ""}</span>
                  <code className="flow2-relation">
                    <span className={markClass(hop, "sub")}>{hop.sub}</span>
                    <span className="flow2-plain"> &lt;: </span>
                    <span className={markClass(hop, "sup")}>{hop.sup}</span>
                  </code>
                </div>
                {hop.shape && hop.shape.length ? (
                  isNote(hop.shape) ? (
                    <span className="flow2-note">{hop.shape[0].text}</span>
                  ) : (
                    <code className="flow2-shape">
                      {hop.shape.map((part, index) => (
                        <span
                          key={index}
                          className={
                            part.role === "plain"
                              ? "flow2-plain"
                              : markClass(hop, part.role)
                          }
                        >
                          {part.text}
                        </span>
                      ))}
                    </code>
                  )
                ) : null}
              </div>
            );
          })}
        </div>
      </div>
    </div>
  );
}

const isNote = (shape) => shape.length === 1 && shape[0].role === "note";

// Tinted only when the editor has a token lit to match, same rule the
// overlay uses: a tint promises a link, so it is only worn when there is
// one.
function markClass(hop, role) {
  const spans = role === "sub" ? hop.subSpans : hop.supSpans;
  return spans && spans.length ? `flow-mark-${role}` : "flow-mark-bare";
}

export default FlowAside;
