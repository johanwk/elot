// src/dbDecorations/scan.ts
//
// Step 2.3.3: pure document-text scanner for the DB-backed
// decoration provider.
//
// Walks a text once and emits non-overlapping `ScanHit` records for
// every CURIE / angle-bracketed IRI / bare http(s) IRI it finds.
// Resolution priority is angle-IRI > bare-IRI > CURIE so that the
// `http://...` inside `<http://...>` is reported once (as the angle
// hit) rather than twice.
//
// Pure: no vscode import.  Used by `dbDecorations.ts` to drive the
// per-editor decoration set, and tested in isolation under tsx.

export interface ScanHit {
  /** Canonical token (angle-IRI without enclosing <>, others as-is). */
  token: string;
  /** Inclusive 0-based start offset in the source text. */
  start: number;
  /** Exclusive end offset. */
  end: number;
  kind: "curie" | "angle-iri" | "bare-iri";
}

// Same lexical shapes as src/dbHover/token.ts -- keep these in sync.
const ANGLE_IRI_RE = /<(https?:\/\/[^\s<>"']+)>/g;
const BARE_IRI_RE = /https?:\/\/[^\s<>"'`,)\]}]+/g;
const CURIE_RE = /(?:[A-Za-z][-A-Za-z0-9_.]*|):[A-Za-z_][-\w_./%:]*/g;

/**
 * Scan TEXT for entity-reference tokens.  Returns hits sorted by
 * start offset, with overlapping shorter matches suppressed.
 *
 * MAX-HITS caps the number of returned tokens; callers should report
 * a "too many" status when the cap is reached.  Pass Infinity to
 * disable.  The scanner short-circuits as soon as the cap is hit, so
 * this is also a defence against pathological inputs.
 */
export function scanTokens(text: string, maxHits = Infinity): ScanHit[] {
  const hits: ScanHit[] = [];
  // Sorted by start; binary-search overlap check would be possible
  // but linear is fine up to maxHits in the low thousands.
  const covered: Array<[number, number]> = [];

  function overlaps(s: number, e: number): boolean {
    for (const [cs, ce] of covered) {
      if (s < ce && e > cs) return true;
    }
    return false;
  }

  function push(hit: ScanHit): boolean {
    if (overlaps(hit.start, hit.end)) return true;
    covered.push([hit.start, hit.end]);
    hits.push(hit);
    return hits.length < maxHits;
  }

  let m: RegExpExecArray | null;

  ANGLE_IRI_RE.lastIndex = 0;
  while ((m = ANGLE_IRI_RE.exec(text)) !== null) {
    const ok = push({
      token: m[1],
      start: m.index,
      end: m.index + m[0].length,
      kind: "angle-iri",
    });
    if (!ok) return finish(hits);
  }

  BARE_IRI_RE.lastIndex = 0;
  while ((m = BARE_IRI_RE.exec(text)) !== null) {
    const ok = push({
      token: m[0],
      start: m.index,
      end: m.index + m[0].length,
      kind: "bare-iri",
    });
    if (!ok) return finish(hits);
  }

  CURIE_RE.lastIndex = 0;
  while ((m = CURIE_RE.exec(text)) !== null) {
    const ok = push({
      token: m[0],
      start: m.index,
      end: m.index + m[0].length,
      kind: "curie",
    });
    if (!ok) return finish(hits);
  }

  return finish(hits);
}

function finish(hits: ScanHit[]): ScanHit[] {
  hits.sort((a, b) => a.start - b.start);
  return hits;
}
