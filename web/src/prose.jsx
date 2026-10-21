import React, { useEffect, useMemo, useState } from "react";
import { clearHighlights } from "./highlight.js";
import { Link, Type, linkProps } from "./links.jsx";

function paths(sentences, prefix, found) {
  sentences.forEach((sentence, index) => {
    const path = `${prefix}.${index}`;
    if (sentence.children.length > 0) found.push(path);
    paths(sentence.children, path, found);
  });
  return found;
}

function everyPath(prose) {
  const found = [];
  ["expected", "actual"].forEach((side) => {
    if (prose[side].sentences.length > 0) found.push(side);
    paths(prose[side].sentences, side, found);
  });
  return found;
}

function Parts({ parts }) {
  return (
    <>
      {parts.map((part, index) => {
        if (part.kind === "text") return <span key={index}>{part.value}</span>;
        if (part.kind === "code") {
          return (
            <Link
              key={index}
              className="prose-code"
              text={part.value}
              spans={part.spans}
            />
          );
        }
        return <Type key={index} text={part.value} />;
      })}
    </>
  );
}

function Toggle({ open, expandable, onClick }) {
  if (!expandable) return <span className="prose-toggle prose-leaf">{"·"}</span>;
  return (
    <button
      type="button"
      className="prose-toggle"
      aria-expanded={open}
      aria-label={open ? "collapse" : "expand"}
      onClick={onClick}
    >
      {open ? "▾" : "▸"}
    </button>
  );
}

function Where({ site }) {
  const span = site && site.spans && site.spans[0];
  if (!span) return null;
  return (
    <span className="prose-where" title={`line ${span.line}, column ${span.column}`}>
      {span.line}:{span.column}
    </span>
  );
}

function Sentence({ sentence, path, open, toggle }) {
  const expandable = sentence.children.length > 0;
  const expanded = open.has(path);
  return (
    <li className="prose-sentence">
      <div className="prose-line" {...linkProps(sentence.site && sentence.site.spans)}>
        <Toggle open={expanded} expandable={expandable} onClick={() => toggle(path)} />
        <span className="prose-words">
          <Parts parts={sentence.parts} />
        </span>
        <Where site={sentence.site} />
        {expandable && !expanded ? (
          <span className="prose-count">{sentence.children.length}</span>
        ) : null}
      </div>
      {expandable && expanded ? (
        <ul className="prose-children">
          {sentence.children.map((child, index) => (
            <Sentence
              key={index}
              sentence={child}
              path={`${path}.${index}`}
              open={open}
              toggle={toggle}
            />
          ))}
        </ul>
      ) : null}
    </li>
  );
}

function Passage({ passage, side, open, toggle }) {
  const expandable = passage.sentences.length > 0;
  const expanded = open.has(side);
  return (
    <li className={`prose-passage prose-${side}`}>
      <div className="prose-line prose-headline">
        <Toggle open={expanded} expandable={expandable} onClick={() => toggle(side)} />
        <span className="prose-words">
          <Parts parts={passage.line} />
        </span>
        {expandable && !expanded ? (
          <span className="prose-count">{passage.sentences.length}</span>
        ) : null}
      </div>
      {expandable && expanded ? (
        <ul className="prose-children">
          {passage.sentences.map((sentence, index) => (
            <Sentence
              key={index}
              sentence={sentence}
              path={`${side}.${index}`}
              open={open}
              toggle={toggle}
            />
          ))}
        </ul>
      ) : null}
    </li>
  );
}

export function Prose({ prose, initialOpen = [] }) {
  const all = useMemo(() => everyPath(prose), [prose]);
  const [open, setOpen] = useState(() => new Set(initialOpen));

  useEffect(() => clearHighlights, []);

  const toggle = (path) =>
    setOpen((current) => {
      const next = new Set(current);
      if (next.has(path)) next.delete(path);
      else next.add(path);
      return next;
    });

  return (
    <div className="prose-story">
      <div className="prose-controls">
        <button type="button" onClick={() => setOpen(new Set(all))}>
          Expand all
        </button>
        <button type="button" onClick={() => setOpen(new Set())}>
          Collapse all
        </button>
      </div>
      <ul className="prose-root">
        <Passage passage={prose.expected} side="expected" open={open} toggle={toggle} />
        <Passage passage={prose.actual} side="actual" open={open} toggle={toggle} />
      </ul>
    </div>
  );
}
