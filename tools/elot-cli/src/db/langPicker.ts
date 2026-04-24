// src/db/langPicker.ts
//
// Pure language-picker helpers, ported from elot-db.el.
//
// Ports of the Elisp functions:
//   elot-db--effective-language-prefs  -> effectiveLanguagePrefs
//   elot-db--select-by-language        -> selectByLanguage
//   elot-db--pick-value-by-lang        -> pickValueByLang
//   elot-db--looks-like-uri-p          -> looksLikeUriP
//   elot-db--looks-like-curie-p        -> looksLikeCurieP
//
// Behaviour must match Elisp byte-for-byte so the existing ERT
// test vectors can be reused.  See ELOT-DB-PLAN.org Step 2.2.1 and
// the Elisp source in elot-package/elot-db.el for the contract.

/** Sentinel that matches rows whose language tag is missing/empty. */
export const UNTAGGED = ":untagged" as const;
export type Untagged = typeof UNTAGGED;

/** A language preference: either a BCP-47 tag, or the UNTAGGED sentinel. */
export type LangPref = string | Untagged;

/** A language-tagged row as produced by the DB read path. */
export interface LangRow {
  value: string;
  /** Empty string or null means "no language tag". */
  lang: string | null;
}

/**
 * Return the effective preference list.
 *
 * When PREFS is null/empty -> returns [UNTAGGED, "en"] (built-in policy).
 * When PREFS already contains UNTAGGED -> returned unchanged.
 * Otherwise UNTAGGED is appended at the tail, matching elot-db.el.
 */
export function effectiveLanguagePrefs(
  prefs?: readonly LangPref[] | null,
): LangPref[] {
  if (!prefs || prefs.length === 0) return [UNTAGGED, "en"];
  if (prefs.includes(UNTAGGED)) return [...prefs];
  return [...prefs, UNTAGGED];
}

/**
 * Pick the best row from ROWS according to language preferences.
 *
 * Policy (mirrors elot-db--select-by-language):
 *  1. Walk effective prefs in order; first match wins.
 *     UNTAGGED matches rows whose lang is null/empty.
 *     A string preference matches case-insensitively.
 *  2. Otherwise, alphabetical fallback (code-unit) over tagged rows;
 *     if no tagged rows, over the full list.
 *  3. Single-element ROWS returns that element directly.
 */
export function selectByLanguage(
  rows: readonly LangRow[],
  prefs?: readonly LangPref[] | null,
): LangRow | null {
  if (!rows || rows.length === 0) return null;
  if (rows.length === 1) return rows[0];
  const effective = effectiveLanguagePrefs(prefs);
  const norm = (l: string | null): string | null =>
    l === null || l === "" ? null : l.toLowerCase();

  for (const pref of effective) {
    for (const row of rows) {
      const rl = norm(row.lang);
      if (pref === UNTAGGED && rl === null) return row;
      if (
        typeof pref === "string" &&
        pref !== UNTAGGED &&
        rl !== null &&
        rl === pref.toLowerCase()
      ) {
        return row;
      }
    }
  }

  // Alphabetical fallback over tagged rows only (untagged was reachable
  // exclusively via UNTAGGED, handled above).
  const tagged = rows.filter((r) => r.lang !== null && r.lang !== "");
  const pool = tagged.length > 0 ? tagged : [...rows];
  // Stable sort by lang, treating null/"" as "".
  const sorted = [...pool].sort((a, b) => {
    const la = a.lang ?? "";
    const lb = b.lang ?? "";
    return la < lb ? -1 : la > lb ? 1 : 0;
  });
  return sorted[0] ?? null;
}

/** Return the VALUE string of the winning row, or null when ROWS is empty. */
export function pickValueByLang(
  rows: readonly LangRow[],
  prefs?: readonly LangPref[] | null,
): string | null {
  const r = selectByLanguage(rows, prefs);
  return r ? r.value : null;
}

/** True when S contains "://" (looks like a full URI/IRI). */
export function looksLikeUriP(s: unknown): s is string {
  return typeof s === "string" && s.includes("://");
}

/** True when S contains ":" but no "://" (CURIE-like). */
export function looksLikeCurieP(s: unknown): s is string {
  return typeof s === "string" && s.includes(":") && !s.includes("://");
}
