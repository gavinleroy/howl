// Editor highlight bus, keyed by playground id: a page can hold several
// independent editors, and character offsets from one must not light up
// another.

const listeners = new Map();
const states = new Map();

const EMPTY = { hover: [], pinned: [] };

function listenersFor(id) {
  let found = listeners.get(id);
  if (!found) {
    found = new Set();
    listeners.set(id, found);
  }
  return found;
}

function same(left, right) {
  if (left.length !== right.length) return false;
  return left.every(
    (span, index) =>
      span.from === right[index].from && span.to === right[index].to,
  );
}

export function highlightState(id) {
  return states.get(id) || EMPTY;
}

function publish(id, next) {
  states.set(id, next);
  listenersFor(id).forEach((listener) => listener(next));
}

export function hoverSpans(id, spans) {
  publish(id, { ...highlightState(id), hover: spans || [] });
}

export function pinSpans(id, spans) {
  const state = highlightState(id);
  const wanted = spans || [];
  publish(id, {
    ...state,
    pinned: same(state.pinned, wanted) ? [] : wanted,
  });
}

export function clearHighlights(id) {
  publish(id, { hover: [], pinned: [] });
}

export function onHighlight(id, listener) {
  const found = listenersFor(id);
  found.add(listener);
  listener(highlightState(id));
  return () => found.delete(listener);
}

// A hovered span may name which side of a constraint it belongs to, so
// the two can be lit apart. Spans without a role light the one way they
// always have.
export function decorations({ hover, pinned }) {
  return [
    ...pinned.map((span) => ({ ...span, kind: "pinned" })),
    ...hover.map((span) => ({
      ...span,
      kind: span.role === "sup" ? "hover-sup" : "hover",
    })),
  ];
}
