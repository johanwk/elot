// src/dbHover/token.ts
//
// Step 2.3.2: pure token-at-offset detector for the DB hover
// provider.  Recognises three shapes used to refer to ontology
// entities in non-Org code (TypeScript, Python, comments, JSON, ...):
//
//   1. CURIE                    e.g.  obo:BFO_0000001  rdfs:label  :localName
//   2. Angle-bracketed full IRI e.g.  <http://example.org/Widget>
//   3. Bare http(s) URI         e.g.  http://example.org/Widget
//
// Pure: operates on a string (single line of text) and a 0-based
// character offset; returns the matched token in the form the DB
// stores it (URIs without enclosing brackets) plus the matched
// span.  No vscode import -- testable under tsx.

export interface DetectedToken {
  /** Token in DB-canonical form: CURIE as-is, IRIs without <...>. */
  token: string;
  /** Inclusive start column. */
  start: number;
  /** Exclusive end column. */
  end: number;
  /** Which shape matched -- useful for diagnostics. */
  kind: "curie" | "angle-iri" | "bare-iri";
}

// CURIE: `prefix:localName` where prefix is empty, or a NCName-ish
// identifier; localName allows the usual OBO/RDF id characters.  We
// deliberately tolerate dots in the prefix (e.g. `dc11:`) and the
// empty prefix (`:Widget`) which Org/Turtle both permit.
const CURIE_RE = /(?:[A-Za-z][-A-Za-z0-9_.]*|):[A-Za-z_][-\w_./%:]*/g;

// Angle-bracketed IRI.  `<...>` with no whitespace inside.
const ANGLE_IRI_RE = /<https?:\/\/[^\s<>"']+>/g;

// Bare http(s) IRI.  Stop at common trailing punctuation/markup.
const BARE_IRI_RE = /https?:\/\/[^\s<>"'`,)\]}]+/g;

interface RawHit {
  text: string;
  start: number;
  end: number;
}

function findAt(line: string, offset: number, re: RegExp): RawHit | null {
  re.lastIndex = 0;
  let m: RegExpExecArray | null;
  while ((m = re.exec(line)) !== null) {
    const s = m.index;
    const e = s + m[0].length;
    if (offset >= s && offset <= e) {
      return { text: m[0], start: s, end: e };
    }
    if (s > offset) break;
  }
  return null;
}

/**
 * Find the entity-reference token at OFFSET on LINE, if any.
 *
 * Resolution order: angle-bracketed IRI, then bare IRI, then CURIE.
 * (Angle-bracketed wins so the `<` and `>` characters resolve to
 * the full IRI rather than to the bare-URI fallback.)  Returns null
 * if no shape matches the cursor position.
 */
export function detectTokenAtOffset(
  line: string,
  offset: number,
): DetectedToken | null {
  const angle = findAt(line, offset, ANGLE_IRI_RE);
  if (angle) {
    // Strip < and >.
    const inner = angle.text.slice(1, -1);
    return {
      token: inner,
      start: angle.start,
      end: angle.end,
      kind: "angle-iri",
    };
  }
  const bare = findAt(line, offset, BARE_IRI_RE);
  if (bare) {
    return {
      token: bare.text,
      start: bare.start,
      end: bare.end,
      kind: "bare-iri",
    };
  }
  const curie = findAt(line, offset, CURIE_RE);
  if (curie) {
    return {
      token: curie.text,
      start: curie.start,
      end: curie.end,
      kind: "curie",
    };
  }
  return null;
}
