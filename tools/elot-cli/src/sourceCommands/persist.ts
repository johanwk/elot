// src/sourceCommands/persist.ts
//
// Step 2.3.7b: pure helpers for persisting the current effective
// `elot.activeLabelSources` value into a specific configuration
// scope (Workspace or User), mirroring the Elisp
// `elot-label-active-sources-persist` convenience.
//
// No vscode import; tsx-testable.  The vscode wrapper in
// persistCommands.ts reads the effective value via
// `cfg.get(...)` (which already resolves precedence) and calls
// `cfg.update(key, value, target)` with the result of
// `buildPersistPlan`.

import {
  normalizeActiveSources,
  type NormalizedActiveSource,
} from "../activeSources.js";

export type PersistScope = "workspace" | "user";

export interface PersistPlan {
  scope: PersistScope;
  /** Canonicalised value to write (always the {source,dataSource} shape). */
  value: Array<{ source: string; dataSource: string }>;
  /** Number of entries that survived normalisation. */
  count: number;
}

/**
 * Build a persist plan from the current effective setting value and
 * a target scope.  Strings are expanded to {source, dataSource:""}
 * so settings.json is unambiguous after persistence.
 */
export function buildPersistPlan(
  effective: unknown,
  scope: PersistScope,
): PersistPlan {
  const normalized: NormalizedActiveSource[] =
    normalizeActiveSources(effective);
  const value = normalized.map((s) => ({
    source: s.source,
    dataSource: s.dataSource ?? "",
  }));
  return { scope, value, count: value.length };
}

/**
 * Friendly summary used in the toast confirmation.  Stable wording
 * keeps tests robust.
 */
export function describePersistResult(plan: PersistPlan): string {
  const where = plan.scope === "workspace" ? "workspace" : "user";
  if (plan.count === 0) {
    return `ELOT: cleared active label sources at ${where} scope.`;
  }
  if (plan.count === 1) {
    return `ELOT: persisted 1 active label source to ${where} settings.`;
  }
  return `ELOT: persisted ${plan.count} active label sources to ${where} settings.`;
}
