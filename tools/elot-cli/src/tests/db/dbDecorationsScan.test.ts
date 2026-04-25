// src/tests/db/dbDecorationsScan.test.ts
//
// Tests for the pure document-text scanner used by dbDecorations.
//
// Verifies:
//   - CURIE / angle-IRI / bare-IRI recognition
//   - non-overlap (angle-IRI suppresses the inner bare-IRI)
//   - sorted-by-start output
//   - maxHits cap

import { scanTokens } from "../../dbDecorations/scan.js";

let pass = 0;
let fail = 0;

function eq<T>(a: T, e: T, msg: string): void {
  const aj = JSON.stringify(a);
  const ej = JSON.stringify(e);
  if (aj === ej) {
    pass++;
  } else {
    fail++;
    console.error(`FAIL ${msg}\n  got:      ${aj}\n  expected: ${ej}`);
  }
}

function ok(cond: unknown, msg: string): void {
  if (cond) pass++;
  else {
    fail++;
    console.error(`FAIL ${msg}`);
  }
}

// 1. CURIE only.
{
  const hits = scanTokens("see obo:BFO_0000001 here");
  eq(hits.length, 1, "1 curie hit count");
  eq(hits[0].kind, "curie", "1 curie kind");
  eq(hits[0].token, "obo:BFO_0000001", "1 curie token");
  eq(hits[0].start, 4, "1 curie start");
}

// 2. Angle IRI suppresses the inner bare IRI (no double-decoration).
{
  const hits = scanTokens("ref <http://example.org/Widget> end");
  eq(hits.length, 1, "2 angle suppress count");
  eq(hits[0].kind, "angle-iri", "2 angle kind");
  eq(hits[0].token, "http://example.org/Widget", "2 angle token unbracketed");
  eq(hits[0].start, 4, "2 angle start (includes <)");
  eq(hits[0].end, 31, "2 angle end (after >)");
}

// 3. Bare IRI when no angle brackets.
{
  const hits = scanTokens("see http://ex.org/Widget today");
  eq(hits.length, 1, "3 bare count");
  eq(hits[0].kind, "bare-iri", "3 bare kind");
  eq(hits[0].token, "http://ex.org/Widget", "3 bare token");
}

// 4. Bare IRI trailing punctuation cutoffs.
{
  const hits = scanTokens("(see http://ex.org/x), or http://ex.org/y.");
  eq(hits.length, 2, "4 cutoff count");
  eq(hits[0].token, "http://ex.org/x", "4 first stops at )");
  // bare-IRI regex doesn't strip trailing dot; ensure no comma though
  ok(!hits[1].token.includes(","), "4 second has no trailing comma");
}

// 5. Mixed: CURIE + angle-IRI together, sorted output.
{
  const hits = scanTokens(
    "x obo:BFO_0000001 y <http://ex.org/W> z rdfs:label",
  );
  eq(hits.length, 3, "5 mixed count");
  // sorted by start
  ok(
    hits[0].start < hits[1].start && hits[1].start < hits[2].start,
    "5 sorted",
  );
  eq(hits[0].kind, "curie", "5 first kind");
  eq(hits[1].kind, "angle-iri", "5 second kind");
  eq(hits[2].kind, "curie", "5 third kind");
}

// 6. Empty-prefix CURIE :Widget recognised.
{
  const hits = scanTokens("see :Widget here");
  eq(hits.length, 1, "6 empty-prefix count");
  eq(hits[0].token, ":Widget", "6 empty-prefix token");
}

// 7. Multiple CURIEs on one line.
{
  const hits = scanTokens("a:Foo b:Bar c:Baz");
  eq(hits.length, 3, "7 multi curie count");
  eq(hits.map((h) => h.token).join("|"), "a:Foo|b:Bar|c:Baz", "7 tokens");
}

// 8. maxHits cap.
{
  const text = Array.from({ length: 20 }, (_, i) => `p:Q${i}`).join(" ");
  const hits = scanTokens(text, 5);
  eq(hits.length, 5, "8 cap count");
}

// 9. No false-positives on plain text.
{
  const hits = scanTokens("just some prose, nothing to see here");
  eq(hits.length, 0, "9 plain text");
}

// 10. URL with port and path matched as bare-IRI.
{
  const hits = scanTokens("see http://localhost:8080/api/v1 yes");
  eq(hits.length, 1, "10 port count");
  eq(hits[0].token, "http://localhost:8080/api/v1", "10 port token");
}

// 11. Dotted-prefix CURIE `dc11:identifier`.
{
  const hits = scanTokens("attr dc11:identifier value");
  eq(hits.length, 1, "11 dotted count");
  eq(hits[0].token, "dc11:identifier", "11 dotted token");
}

// 12. Multi-line text: offsets are absolute over the whole text.
{
  const text = "first obo:Foo\nsecond obo:Bar";
  const hits = scanTokens(text);
  eq(hits.length, 2, "12 multiline count");
  eq(hits[0].token, "obo:Foo", "12 first token");
  eq(hits[1].token, "obo:Bar", "12 second token");
  ok(hits[1].start > text.indexOf("\n"), "12 second start past newline");
}

console.log(
  `dbDecorationsScan tests: ${pass} passed, ${fail} failed`,
);
if (fail > 0) process.exit(1);
