// Hand-written: the bundle is plain JS, this keeps `tsc --noEmit` happy.
import type * as React from "react";

export interface HowlEditorProps {
  /** Ties this editor to the diagnostic views that share the id. */
  id?: string;
  /** URL prefix the wasm runtime is served from, e.g. "/howl/". */
  assetBase?: string;
  initialSource?: string;
  /** Example buttons; pass null to hide them. */
  examples?: { name: string; source: string }[] | null;
  showStdout?: boolean;
  readOnly?: boolean;
  /** label -> program; shown as tabs, in key order. */
  tabs?: Record<string, string> | null;
}

export type HowlForm = "terse" | "minimal" | "flow" | "graph" | "tree";

export interface HowlDiagnosticProps {
  id?: string;
  form?: HowlForm;
  /** Which diagnostic: an index, or "all". */
  index?: number | "all";
}

export declare const HowlEditor: React.FC<HowlEditorProps>;
export declare const HowlDiagnostic: React.FC<HowlDiagnosticProps>;
export declare const HowlStdout: React.FC<{ id?: string }>;
export declare const Playground: React.FC<
  HowlEditorProps & { initialLayout?: "all" | "tabs" }
>;

export declare const EXAMPLES: { name: string; source: string }[];
export declare const INITIAL_SOURCE: string;

export declare function setSource(id: string, source: string): void;
export declare function run(id: string): void;
export declare function snapshot(id: string): unknown;
export declare function subscribe(id: string, listener: () => void): () => void;
