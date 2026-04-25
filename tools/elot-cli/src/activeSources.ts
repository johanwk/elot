// src/activeSources.ts
//
// Step 2.3.1: settings-layer helpers for the VS Code DB bridge.
// Pure normalisers (no vscode import) so they're testable under tsx.
//
// `elot.activeLabelSources` shape:
//   - array of strings ("source-name", dataSource defaults to "")
//   - array of objects { source: string, dataSource?: string }
// `elot.preferredLanguages` shape:
//   - array of strings (BCP-47 tags or "" for the untagged sentinel)
//
// Both settings are user-editable via VS Code's settings UI; the
// normalisers tolerate missing / malformed input and silently drop
// invalid entries.

import { effectiveLanguagePrefs, LangPref } from "./db/langPicker.js";

export interface NormalizedActiveSource {
  source: string;
  /** Empty string is the canonical "no data_source" sentinel. */
  dataSource: string;
}

/**
 * Normalise raw `elot.activeLabelSources` into a list of
 * `{source, dataSource}` records.  Drops entries that are not
 * strings or objects with a non-empty `source` field.
 */
export function normalizeActiveSources(raw: unknown): NormalizedActiveSource[] {
  if (!Array.isArray(raw)) return [];
  const out: NormalizedActiveSource[] = [];
  for (const item of raw) {
    if (typeof item === "string") {
      if (item.length > 0) out.push({ source: item, dataSource: "" });
      continue;
    }
    if (item && typeof item === "object") {
      const o = item as { source?: unknown; dataSource?: unknown };
      const source = typeof o.source === "string" ? o.source : null;
      if (!source || source.length === 0) continue;
      const ds = typeof o.dataSource === "string" ? o.dataSource : "";
      out.push({ source, dataSource: ds });
    }
  }
  return out;
}

/**
 * Normalise raw `elot.preferredLanguages` into a list of language
 * tags.  Empty strings (the untagged sentinel) survive normalisation
 * but only when the underlying picker accepts them via
 * `effectiveLanguagePrefs`.
 */
export function normalizePreferredLanguages(raw: unknown): string[] {
  if (!Array.isArray(raw)) return [];
  return raw.filter(
    (x): x is string => typeof x === "string" && x.length > 0,
  );
}

/**
 * Resolve the effective language preference list applied by the
 * picker: user prefs first, then the default `[UNTAGGED, "en"]`
 * fallback when prefs are empty.  Thin wrapper over
 * `effectiveLanguagePrefs` for symmetry with `getActiveSources`.
 */
export function getEffectiveLanguagePrefs(raw: unknown): LangPref[] {
  const tags = normalizePreferredLanguages(raw);
  return effectiveLanguagePrefs(tags as LangPref[]);
}
