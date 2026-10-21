import React, { useEffect, useRef } from "react";
import { EditorState, StateEffect, StateField } from "@codemirror/state";
import {
  Decoration,
  EditorView,
  drawSelection,
  highlightActiveLine,
  keymap,
} from "@codemirror/view";
import { decorations, onHighlight } from "./highlight.js";
import { defaultKeymap, history, historyKeymap } from "@codemirror/commands";
import {
  HighlightStyle,
  StreamLanguage,
  defaultHighlightStyle,
  syntaxHighlighting,
} from "@codemirror/language";
import { tags } from "@lezer/highlight";
import { scheme } from "@codemirror/legacy-modes/mode/scheme";

// The scheme mode tags almost everything "builtin" or "variable", and the
// default highlight style colors neither — so retag with this language's
// actual special forms, and read capitalized names as constructors.
const KEYWORDS = new Set([
  "define", "fun", "let", "match", "if", "and", "or", "dot",
  "tuple", "record", "enum", "type", "module", "sig", "functor",
]);

const howl = {
  ...scheme,
  name: "howl",
  token(stream, state) {
    const style = scheme.token(stream, state);
    if (style === "builtin" || style === "variable" || style === "keyword") {
      const word = stream.current();
      if (KEYWORDS.has(word)) return "keyword";
      if (/^[A-Z]/.test(word)) return "type";
      return "variable";
    }
    return style;
  },
};

// Emit the class names a static code block gets rather than colours of our
// own, so the editor is painted by whatever syntax theme the host page
// already loads. These are the classes starry-night assigns to Scheme:
// `pl-k` keywords, `pl-c1` constants and numbers, `pl-s` string bodies with
// `pl-pds` for their quotes, `pl-c` comments. Plain variables get nothing,
// as they do in a static block.
const howlHighlight = HighlightStyle.define([
  { tag: tags.keyword, class: "pl-k" },
  { tag: [tags.typeName, tags.namespace, tags.atom, tags.number], class: "pl-c1" },
  { tag: tags.string, class: "pl-s" },
  { tag: tags.comment, class: "pl-c" },
]);

export function languageExtension() {
  return StreamLanguage.define(howl);
}

export const showHighlights = StateEffect.define();

const marks = {
  hover: Decoration.mark({ class: "cm-linked-hover" }),
  "hover-sup": Decoration.mark({ class: "cm-linked-demand" }),
  pinned: Decoration.mark({ class: "cm-linked-pinned" }),
};

export function highlightRanges(spans, length) {
  return spans
    .filter((span) => span.from < span.to && span.to <= length && marks[span.kind])
    .sort((left, right) => left.from - right.from || left.to - right.to)
    .map((span) => marks[span.kind].range(span.from, span.to));
}

const highlightField = StateField.define({
  create: () => Decoration.none,
  update(value, transaction) {
    let next = value.map(transaction.changes);
    for (const effect of transaction.effects) {
      if (effect.is(showHighlights)) {
        next = Decoration.set(
          highlightRanges(effect.value, transaction.state.doc.length),
          true,
        );
      }
    }
    return next;
  },
  provide: (field) => EditorView.decorations.from(field),
});

export function Editor({
  id,
  value,
  onChange,
  onRun,
  readOnly = false,
  viewRef = null,
}) {
  const host = useRef(null);
  const view = useRef(null);
  const callbacks = useRef({ onChange, onRun });
  callbacks.current = { onChange, onRun };

  useEffect(() => {
    const state = EditorState.create({
      doc: value,
      extensions: [
        highlightField,
        ...(readOnly
          ? [EditorState.readOnly.of(true), EditorView.editable.of(false)]
          : []),
        history(),
        drawSelection(),
        highlightActiveLine(),
        syntaxHighlighting(howlHighlight),
        syntaxHighlighting(defaultHighlightStyle, { fallback: true }),
        languageExtension(),
        keymap.of([
          {
            key: "Mod-Enter",
            preventDefault: true,
            run: () => {
              callbacks.current.onRun();
              return true;
            },
          },
          ...defaultKeymap,
          ...historyKeymap,
        ]),
        EditorView.updateListener.of((update) => {
          if (update.docChanged) {
            callbacks.current.onChange(update.state.doc.toString());
          }
        }),
        // Sized by its content, in the host's code font: an embedded
        // program should read like the code blocks around it.
        EditorView.theme({
          ".cm-scroller": {
            fontFamily: "var(--howl-mono)",
            fontSize: "var(--howl-code-size)",
            lineHeight: "1.5",
          },
          ".cm-content": { padding: "0" },
          "&.cm-focused": { outline: "none" },
        }),
      ],
    });

    view.current = new EditorView({ state, parent: host.current });
    if (viewRef) viewRef.current = view.current;
    return () => {
      view.current.destroy();
      view.current = null;
      if (viewRef) viewRef.current = null;
    };
  }, []);

  useEffect(
    () =>
      onHighlight(id, (state) => {
        const editor = view.current;
        if (!editor) return;
        editor.dispatch({ effects: [showHighlights.of(decorations(state))] });
      }),
    [id],
  );

  useEffect(() => {
    const editor = view.current;
    if (!editor) return;
    const current = editor.state.doc.toString();
    if (current !== value) {
      editor.dispatch({
        changes: { from: 0, to: current.length, insert: value },
      });
    }
  }, [value]);

  return <div className="editor" ref={host} />;
}
