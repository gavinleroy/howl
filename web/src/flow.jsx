import React, { useCallback, useEffect, useRef, useState } from "react";
import { Editor } from "./editor.jsx";
import { hoverSpans } from "./highlight.js";

// The error as information flow: each concrete type starts in a signature
// on the right and travels through the code on the left until two of them
// arrive at the same place and disagree. Colour is keyed to the
// originating type — every float-rooted arrow one hue, every int-rooted
// arrow another — so the collision reads as two colours meeting.

// Three slots, validated against both surfaces for all-pairs separation.
// Beyond three, origins share the neutral slot rather than inventing hues.
const SLOTS = 3;

function slotOf(type, order) {
  const at = order.indexOf(type);
  return at === -1 || at >= SLOTS ? "x" : String(at + 1);
}

// Anchor on a code token: under the middle of its first span. Sitting on
// the line's bottom edge rather than its middle keeps the dot and the
// arrows off the glyphs they are about.
const DROP = 1;

function codeAnchor(view, host, spans) {
  const span = spans && spans[0];
  if (!view || !span) return null;
  const from = view.coordsAtPos(span.from);
  const to = view.coordsAtPos(span.to);
  if (!from) return null;
  const right = to ? to.right : from.right;
  return {
    x: (from.left + right) / 2 - host.left,
    y: from.bottom - host.top + DROP,
  };
}

// Each hop is its own straight arrow. Both ends are pulled back from the
// anchor so the head stops beside the token rather than covering it.
const PAD = 7;

function segment(a, b) {
  const dx = b.x - a.x;
  const dy = b.y - a.y;
  const length = Math.hypot(dx, dy);
  if (length <= PAD * 2) return null;
  const ux = dx / length;
  const uy = dy / length;
  return {
    x1: a.x + ux * PAD,
    y1: a.y + uy * PAD,
    x2: b.x - ux * PAD,
    y2: b.y - uy * PAD,
  };
}

// Some hops have no structure to deconstruct and carry a bare word for
// why they exist instead. That is prose, not type, so it sets as prose.
const isNote = (shape) => shape.length === 1 && shape[0].role === "note";

// A side is tinted only when it has a token lit to match. A pattern's
// subject is a binder with nowhere to point, so it stays plain rather
// than promising a link the editor cannot show.
function markClass(dot, role) {
  const spans = role === "sub" ? dot.subSpans : dot.supSpans;
  return spans && spans.length ? `flow-mark-${role}` : "flow-mark-bare";
}

export function FlowView({ flow, id = "default" }) {
  const host = useRef(null);
  const view = useRef(null);
  const slots = useRef(new Map());
  const [lines, setLines] = useState([]);
  const [dots, setDots] = useState([]);
  const [size, setSize] = useState({ width: 0, height: 0 });
  const [active, setActive] = useState(null);

  // A payload from a compiler build older than this renderer has a
  // different shape; say so rather than throwing inside the boundary.
  const flows = Array.isArray(flow && flow.flows) ? flow.flows : null;
  const signatures = Array.isArray(flow && flow.signatures)
    ? flow.signatures
    : [];

  // Colour is per originating type, in first-seen order.
  const order = [];
  for (const path of flows || []) {
    if (!order.includes(path.type)) order.push(path.type);
  }

  const measure = useCallback(() => {
    const box = host.current;
    if (!box || !view.current) return;
    const rect = box.getBoundingClientRect();
    setSize({ width: rect.width, height: rect.height });

    const drawnLines = [];
    const drawnDots = [];

    (flows || []).forEach((path, index) => {
      const hue = slotOf(path.type, order);
      const points = [];

      // The first point is the slot in the signature panel the concrete
      // type enters from; the rest are code tokens.
      const start = slots.current.get(`${path.signature}:${path.slot}`);
      if (start) {
        const r = start.getBoundingClientRect();
        points.push({
          x: r.left - rect.left,
          y: (r.top + r.bottom) / 2 - rect.top,
          label: path.origin,
          detail: null,
        });
      }
      path.hops.forEach((hop, at) => {
        const point = codeAnchor(view.current, rect, hop.spans);
        if (point) {
          points.push({
            ...point,
            label: hop.relation,
            sub: hop.sub,
            sup: hop.sup,
            // The token this dot sits on, and the token behind each side
            // of the constraint. Hovering lights both sides in their own
            // tint, and the tip tints the same two types to match, so
            // either bare variable can be read back to the code it names.
            spans: hop.spans || [],
            subSpans: hop.subSpans || [],
            supSpans: hop.supSpans || [],
            // Empty when the constraint line already is the structure.
            shape:
              Array.isArray(hop.shape) && hop.shape.length > 0
                ? hop.shape
                : null,
          });
        }
        void at;
      });

      points.forEach((point, at) => {
        drawnDots.push({
          ...point,
          hue,
          flow: index,
          at,
          key: `${index}-${at}`,
          path,
        });
      });
      for (let at = 1; at < points.length; at++) {
        const line = segment(points[at - 1], points[at]);
        if (line) {
          // `at` is the point this arrow ends on, so it touches dots
          // `at - 1` and `at`.
          drawnLines.push({
            ...line,
            hue,
            flow: index,
            at,
            key: `${index}-${at}`,
          });
        }
      }
    });

    setLines(drawnLines);
    setDots(drawnDots);
  }, [flow]);

  useEffect(() => {
    measure();
    const box = host.current;
    const scroller = box && box.querySelector(".cm-scroller");
    const again = () => measure();
    window.addEventListener("resize", again);
    if (scroller) scroller.addEventListener("scroll", again);
    // Geometry settles a tick after mount, once the editor has laid out.
    const settle = setTimeout(again, 0);
    return () => {
      window.removeEventListener("resize", again);
      if (scroller) scroller.removeEventListener("scroll", again);
      clearTimeout(settle);
    };
  }, [measure]);

  // A dot lights the two tokens its constraint relates, each in its own
  // tint, rather than the whole path: a path's worth of lit tokens ties a
  // type in the tip to nothing. The dot at the signature slot has no
  // constraint of its own, so it falls back to the path it starts.
  const enter = (dot) => {
    setActive(dot);
    const sides = [
      ...(dot.subSpans || []).map((span) => ({ ...span, role: "sub" })),
      ...(dot.supSpans || []).map((span) => ({ ...span, role: "sup" })),
    ];
    hoverSpans(
      id,
      sides.length ? sides : dot.path.hops.flatMap((hop) => hop.spans || []),
    );
  };
  const leave = () => {
    setActive(null);
    hoverSpans(id, []);
  };

  // Hovering reads in two weights: what the hovered dot is attached to
  // stays full, everything else recedes equally. Nothing is hidden — a
  // dimmed dot is still there to be hovered.
  const lineWeight = (line) => {
    if (!active) return "";
    const touching =
      line.flow === active.flow &&
      (line.at === active.at || line.at === active.at + 1);
    return touching ? "" : " flow-faded";
  };

  const dotWeight = (dot) => {
    if (!active) return "";
    return dot.flow === active.flow && dot.at === active.at
      ? ""
      : " flow-faded";
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

  return (
    <div className="howl-root flow-view" ref={host}>
      <div className="flow-code">
        <Editor
          id={id}
          value={flow.source}
          onChange={() => {}}
          onRun={() => {}}
          readOnly={true}
          viewRef={view}
        />
      </div>

      <div className="flow-panel">
        {signatures.map((row) => (
          <div className="flow-sig" key={row.id}>
            <code className="flow-sig-name">{row.name}</code>
            <code className="flow-sig-type">
              {"(-> ("}
              {row.slots
                .filter((slot) => slot.slot !== "result")
                .map((slot, at) => (
                  <React.Fragment key={slot.slot}>
                    {at > 0 ? " " : ""}
                    <span
                      className="flow-slot"
                      ref={(el) => {
                        if (el) slots.current.set(`${row.id}:${slot.slot}`, el);
                      }}
                    >
                      {slot.text}
                    </span>
                  </React.Fragment>
                ))}
              {") "}
              <span
                className="flow-slot"
                ref={(el) => {
                  if (el) slots.current.set(`${row.id}:result`, el);
                }}
              >
                {(row.slots.find((slot) => slot.slot === "result") || {}).text}
              </span>
              {")"}
            </code>
          </div>
        ))}
      </div>

      <svg className="flow-arrows" width={size.width} height={size.height}>
        <defs>
          {/* In user-space units, so the head keeps this size whatever
              the stroke does. Blunter than it is long, which is what
              makes it read as a head rather than as more line. */}
          <marker
            id="howl-flow-head"
            markerWidth="12"
            markerHeight="10"
            refX="12"
            refY="5"
            orient="auto"
            markerUnits="userSpaceOnUse"
          >
            <polygon points="0 0, 12 5, 0 10" fill="context-stroke" />
          </marker>
        </defs>

        {lines.map((line) => (
          <line
            key={line.key}
            x1={line.x1}
            y1={line.y1}
            x2={line.x2}
            y2={line.y2}
            className={`flow-arrow flow-hue-${line.hue}${lineWeight(line)}`}
            markerEnd="url(#howl-flow-head)"
          />
        ))}

        {dots.map((dot) => (
          <circle
            key={dot.key}
            cx={dot.x}
            cy={dot.y}
            r="4"
            className={`flow-dot flow-hue-${dot.hue}${dotWeight(dot)}`}
            onMouseEnter={() => enter(dot)}
            onMouseLeave={leave}
          />
        ))}
      </svg>

      {active ? (
        <div
          className={`flow-tip flow-hue-${active.hue}`}
          style={{ left: `${active.x}px`, top: `${active.y + 10}px` }}
          role="tooltip"
        >
          <code className="flow-tip-relation">
            {active.sub ? (
              <>
                <span className={markClass(active, "sub")}>{active.sub}</span>
                <span className="flow-tip-plain"> &lt;: </span>
                <span className={markClass(active, "sup")}>{active.sup}</span>
              </>
            ) : (
              active.label
            )}
          </code>
          {active.shape && isNote(active.shape) ? (
            <span className="flow-tip-note">{active.shape[0].text}</span>
          ) : null}
          {active.shape && !isNote(active.shape) ? (
            <code className="flow-tip-shape">
              {active.shape.map((part, at) => (
                <span
                  key={at}
                  className={
                    part.role === "plain"
                      ? "flow-tip-plain"
                      : markClass(active, part.role)
                  }
                >
                  {part.text}
                </span>
              ))}
            </code>
          ) : null}
        </div>
      ) : null}
    </div>
  );
}
