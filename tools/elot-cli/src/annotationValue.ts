// src/annotationValue.ts
//
// Port of `elot-annotation-string-or-uri` and `elot-unprefix-uri`
// from elot-tangle.el
//
// Classifies an annotation value string and formats it for OMN output.
// The result always starts with whitespace (1-2 spaces) for alignment.

import type { PrefixEntry } from "./types.js";

/**
 * Remove common leading whitespace from continuation lines in a string.
 *
 * Port of `elot--strip-continuation-indent` from elot-tangle.el.
 *
 * Finds the minimum number of leading spaces on lines after the first,
 * then removes exactly that many spaces from the beginning of each
 * continuation line.  The first line is left unchanged.
 * Only strips from lines that actually begin with at least that many spaces.
 */
export function stripContinuationIndent(str: string): string {
  if (!str.includes("\n")) return str;

  const lines = str.split("\n");
  const firstLine = lines[0];
  const restLines = lines.slice(1);

  // Find the minimum indent among continuation lines that have leading spaces
  let minIndent = Infinity;
  for (const line of restLines) {
    const m = line.match(/^( +)/);
    if (m) {
      minIndent = Math.min(minIndent, m[1].length);
    }
  }
  // If no continuation line had leading spaces, or min is 0, return unchanged
  if (!isFinite(minIndent) || minIndent === 0) return str;

  const prefix = " ".repeat(minIndent);
  const trimmed = restLines.map((line) =>
    line.startsWith(prefix) ? line.slice(minIndent) : line
  );
  return [firstLine, ...trimmed].join("\n");
}

/**
 * The CURIE regex from elot-puri-re.
 * Matches "prefix:localname" where prefix can be empty (default prefix).
 * Anchored to ^...$ to match the whole string.
 */
const CURIE_RE =
  /^([a-zA-Z][-a-zA-Z0-9_.]*|):([-\w_./]*)$/;

/**
 * Expand a CURIE to a full URI using the given prefix map.
 *
 * Port of `elot-unprefix-uri`.
 *
 * @param puri    - The CURIE string, e.g. "obo:BFO_0000001"
 * @param prefixes - Map from prefix name (without trailing colon) to namespace URI
 * @param noerror  - If true, return null on unknown prefix instead of passing through
 * @returns The expanded URI in angle brackets, or the original string, or null
 */
export function unprefixUri(
  puri: string,
  prefixes: Map<string, string> | null,
  noerror = false
): string | null {
  // If no prefix map, return unchanged (matches Elisp: (if (eq abbrev-alist nil) puri ...))
  if (!prefixes) return puri;

  const m = puri.match(CURIE_RE);
  if (!m) return puri;

  const thisPrefix = m[1];   // e.g. "obo" or "" for default prefix
  const thisLocalname = m[2]; // e.g. "BFO_0000001"

  const thisNs = prefixes.get(thisPrefix);
  if (thisNs) {
    return `<${thisNs}${thisLocalname}>`;
  }

  // Prefix not found
  if (noerror) return null;
  // Tentatively let the raw value through (matches Elisp comment)
  return puri;
}

/**
 * Format an annotation value for Manchester Syntax output.
 *
 * Port of `elot-annotation-string-or-uri`.
 *
 * The return value is prefixed with whitespace (1-2 spaces) matching
 * the Elisp convention for alignment within annotation blocks.
 *
 * @param str      - The raw value string from a description list body
 * @param prefixes - Prefix map for CURIE expansion
 * @returns Formatted string with leading whitespace
 */
export function annotationStringOrUri(
  str: string,
  prefixes: Map<string, string> | null
): string {
  // Strip common continuation-line indent first (port of Elisp behaviour).
  str = stripContinuationIndent(str);

  // Skip Org macro expansion ({{{...}}}) — not applicable in CLI context.
  // The CLI reads the raw Org text; macros are an Emacs-only feature.

  // 1. A number — return the string
  if (/^\d+\.?\d*$/.test(str)) {
    return `  ${str}`;
  }

  // 2. A bare URI wrapped in Org double-bracket links: [[http://...]]
  {
    const m = str.match(/^\[\[(http[^ ]*)\]\]$/);
    if (m) return `  <${m[1]}>`;
  }

  // 3. A bare URI, no brackets — wrap in angles
  {
    const m = str.match(/^(http[^ ]*)$/);
    if (m) return `  <${m[1]}>`;
  }

  // 4. A bare URI already in angles — passthrough
  {
    const m = str.match(/^(<http[^ ]*>)$/);
    if (m) return `  ${m[1]}`;
  }

  // 5. A bare URN in angles — passthrough
  {
    const m = str.match(/^(<urn:[^>]+>)$/);
    if (m) return `  ${m[1]}`;
  }

  // 6. A URN without angles (specifically urn:uuid) — wrap as xsd:string
  {
    const m = str.match(/^(urn:uuid[^ ]+)$/);
    if (m) return `  "${m[1]}"^^xsd:string`;
  }

  // 7. Boolean true
  if (/^true$/.test(str)) {
    return ` "true"^^xsd:boolean`;
  }

  // 8. Boolean false
  if (/^false$/.test(str)) {
    return ` "false"^^xsd:boolean`;
  }

  // 9. String with explicit datatype — return unchanged
  //    Matches: "value"^^prefix:Type
  if (/^".*"\^\^[-_a-zA-Z0-9]*:[-_a-zA-Z0-9]+$/.test(str)) {
    return `  ${str}`;
  }

  // 10. Check if it's NOT a CURIE (unprefix returns the same string)
  //     This means it's a plain string — wrap in quotes.
  const expanded = unprefixUri(str, prefixes);
  if (expanded === str) {
    // Not a CURIE (or prefix not found) — it's a plain string.
    // Check if it's already a language-tagged string: "..."@en
    if (/^"(?:.*\n)*.*"@[a-z]+/.test(str)) {
      return ` ${str}`;
    }
    // Otherwise, wrap in quotes (escaping internal quotes)
    const escaped = str.replace(/"/g, '\\"');
    return `  "${escaped}"`;
  }

  // 11. It IS a CURIE that expanded to a URI — return the expanded form
  return `  ${expanded}`;
}
