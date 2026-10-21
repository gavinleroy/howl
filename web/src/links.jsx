import React from "react";
import { hoverSpans, pinSpans } from "./highlight.js";

export function linkProps(spans) {
  if (!spans || spans.length === 0) return {};
  return {
    tabIndex: 0,
    onMouseEnter: () => hoverSpans(spans),
    onMouseLeave: () => hoverSpans([]),
    onFocus: () => hoverSpans(spans),
    onBlur: () => hoverSpans([]),
    onClick: () => pinSpans(spans),
    onKeyDown: (event) => {
      if (event.key === "Enter" || event.key === " ") {
        event.preventDefault();
        pinSpans(spans);
      }
    },
  };
}

export function Link({ text, spans, className }) {
  const linked = spans && spans.length > 0;
  return (
    <span
      className={[className, linked ? "flow-link" : null].filter(Boolean).join(" ")}
      title={linked ? "highlight this in the editor" : undefined}
      {...linkProps(spans)}
    >
      {text}
    </span>
  );
}

export function Type({ text }) {
  return <span className="flow-type">{text}</span>;
}
