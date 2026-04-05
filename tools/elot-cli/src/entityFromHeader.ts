// src/entityFromHeader.ts
//
// Port of `elot-entity-from-header` from elot-tangle.el
//
// Given a heading text, extract the identifier it declares.
// Returns either:
//   - a CURIE (e.g. "ex:Apple")
//   - a full URI wrapped in <> (e.g. "<http://example.org/Apple>")
//   - a composite string like "ex:Ont <http://…/0.9>" for ontology/version pairs
//   - null if no recognisable identifier is found

export interface EntityInfo {
  /** The OWL identifier: CURIE, <URI>, or "CURIE <versionURI>" */
  id: string;
  /** The human-readable label (text before the parenthesised identifier), or null */
  label: string | null;
}

// A CURIE like "prefix:LocalName" or ":LocalName"
// Prefix part: optional, letters then alphanums/hyphens/dots/underscores
// Local part: alphanums, hyphens, underscores, dots, slashes
const CURIE_PATTERN = String.raw`(?:[a-zA-Z][-a-zA-Z0-9_.]*|):(?:[-\w_./]*)`;

// A full HTTP(S) URI
const FULL_URI_PATTERN = String.raw`https?://[-\w._~:/?#\[\]@!$&'()*+,;=%]*`;

// A URN like <urn:isbn:0943396611>
const URN_PATTERN = String.raw`<urn:[^>]+>`;

/**
 * Extract an OWL entity identifier from an Org heading title string.
 *
 * Mirrors the Elisp `elot-entity-from-header` exactly,
 * matching the same priority order of patterns.
 */
export function entityFromHeader(str: string): EntityInfo | null {
  let m: RegExpMatchArray | null;

  // 1. Single full URI at beginning of line (with or without angle brackets)
  m = str.match(new RegExp(String.raw`^<?(${ FULL_URI_PATTERN })>?`));
  if (m) {
    return { id: `<${m[1]}>`, label: extractLabel(str, m[0]) };
  }

  // 2. Single full URI in parentheses
  m = str.match(new RegExp(String.raw`\(<?(${ FULL_URI_PATTERN })>?\)`));
  if (m) {
    return { id: `<${m[1]}>`, label: extractLabel(str, m[0]) };
  }

  // 3. CURIE at beginning of line
  m = str.match(new RegExp(String.raw`^(${ CURIE_PATTERN })`));
  if (m) {
    return { id: m[1], label: extractLabel(str, m[0]) };
  }

  // 4. CURIE in parentheses
  m = str.match(new RegExp(String.raw`\((${ CURIE_PATTERN })\)`));
  if (m) {
    return { id: m[1], label: extractLabel(str, m[0]) };
  }

  // 5. Two full URIs in parentheses (ontology + version)
  m = str.match(
    new RegExp(
      String.raw`\(<?(${ FULL_URI_PATTERN })>?\s+<?(${ FULL_URI_PATTERN })>?\)`
    )
  );
  if (m) {
    return { id: `<${m[1]}> <${m[2]}>`, label: extractLabel(str, m[0]) };
  }

  // 6. CURIE then full URI in parentheses (ontology + version)
  m = str.match(
    new RegExp(
      String.raw`\((${ CURIE_PATTERN })\s+<?(${ FULL_URI_PATTERN })>?\)`
    )
  );
  if (m) {
    return { id: `${m[1]} <${m[2]}>`, label: extractLabel(str, m[0]) };
  }

  // 7. Two CURIEs in parentheses (ontology + version)
  m = str.match(
    new RegExp(String.raw`\((${ CURIE_PATTERN })\s+(${ CURIE_PATTERN })\)`)
  );
  if (m) {
    return { id: `${m[1]} ${m[2]}`, label: extractLabel(str, m[0]) };
  }

  // 8. URN at start of string
  m = str.match(new RegExp(String.raw`^(${ URN_PATTERN })$`));
  if (m) {
    return { id: m[1], label: null };
  }

  // 9. URN in parentheses
  m = str.match(new RegExp(String.raw`\((${ URN_PATTERN })\)`));
  if (m) {
    return { id: m[1], label: extractLabel(str, m[0]) };
  }

  // No match
  return null;
}

/**
 * Extract the label portion: text before the matched identifier region.
 * E.g. for '"entity"@en (obo:BFO_0000001)' with match '(obo:BFO_0000001)',
 * the label is '"entity"@en'.
 *
 * Returns null if the label would be empty or identical to the identifier.
 */
function extractLabel(str: string, matchedPortion: string): string | null {
  const idx = str.indexOf(matchedPortion);
  if (idx <= 0) return null;
  const label = str.substring(0, idx).trim();
  return label.length > 0 ? label : null;
}
