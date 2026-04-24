// src/parsers/org.ts
//
// Org parser adaptor.  Ports elot-source-parse-org from
// elot-package/elot-sources.el (which is itself a thin wrapper around
// `elot-build-slurp' running inside a temp buffer in org-mode).
//
// The heavy lifting is already done by the existing pipeline:
//
//   parseOrg(text)     -- src/parseOrgWasm.ts  (orgize WASM + ELOT
//                          enrichment: entityFromHeader, rdf:type
//                          inference from :ID: suffix, auto
//                          rdf:type / rdfs:label insertion)
//   buildSlurp(root)   -- src/buildSlurp.ts    (flatten to
//                          Map<uri, SlurpEntry>)
//   getPrefixMap(root) -- src/parseOrgWasm.ts  (locate the
//                          :prefixdefs: block)
//
// This adaptor just shapes the SlurpEntry output into the
// EntityTriple form consumed by ElotDb.updateSource(), preserving
// the Elisp attribute order:
//
//   ("rdfs:label" LABEL) ("rdf:type" TYPE?) ...other-properties
//
// See elot-tangle.el, `elot-build-slurp' (line 842+) for the
// canonical Elisp shape.
//
// Note: `kind' is not populated by the Org path (the Elisp source
// parser also leaves :kind off); ElotDb.updateSource() will default
// it to 'unknown'.  Rich kind inference from rdfType (owl:Class ->
// 'class', etc.) is deferred: Elisp doesn't do it here either, and
// label display only reads `entities.label'.

import { readFileSync } from "fs";
import { parseOrg, getPrefixMap } from "../parseOrgWasm.js";
import { buildSlurp } from "../buildSlurp.js";
import { EntityTriple, AttrValue } from "../db/sqljs.js";
import { ParsedSource } from "./csvTsv.js";

/**
 * Strip an `"..."@tag' wrapper, returning `{value, lang}'.
 * Matches the Elisp behaviour at ingest: a literal of the form
 * "foo"@en becomes value="foo" lang="en"; anything else passes
 * through untagged.
 */
function splitLangTag(s: string): { value: string; lang: string } {
  const m = /^"(.*)"@([a-zA-Z-]+)$/.exec(s);
  if (m) return { value: m[1], lang: m[2] };
  return { value: s, lang: "" };
}

/**
 * Parse an ELOT Org FILE into the ParsedSource shape consumed by
 * dbCli register / refresh.
 */
export function parseOrgSource(file: string): ParsedSource {
  const text = readFileSync(file, "utf-8");
  const root = parseOrg(text);
  const slurp = buildSlurp(root);

  const entries: EntityTriple[] = [];
  for (const [rawUri, entry] of slurp) {
    // buildSlurp keys are bracketed (`<http://...>`) when the heading
    // used a full URI rather than a CURIE.  Strip the brackets ONLY
    // for simple single-URI ids so the stored id is canonical
    // (matches TTL/RQ ingest, and lets the two-pass resolver in
    // getLabelAny / getAttrAny round-trip CURIE <-> URI correctly).
    //
    // Composite ids -- e.g. an OWL ontology heading carrying both
    // an ontology IRI and a versionIRI, encoded by entityFromHeader
    // as "<URI1> <URI2>" or "CURIE <URI>" -- must be left alone:
    // a naive outer-bracket strip would mangle them into
    // "URI1> <URI2".  Detect the simple case by requiring no
    // internal whitespace or `>'.
    const isSimpleBracketedUri =
      rawUri.length > 1 &&
      rawUri.startsWith("<") &&
      rawUri.endsWith(">") &&
      !/[\s>]/.test(rawUri.slice(1, -1));
    const uri = isSimpleBracketedUri ? rawUri.slice(1, -1) : rawUri;
    const attrs: Array<[string, AttrValue]> = [];

    // 1. rdfs:label first (Elisp parity; always emitted so the
    //    picker can see a language-tagged variant when the raw
    //    heading carried one).
    const labelSplit = splitLangTag(entry.label);
    if (labelSplit.lang) {
      attrs.push([
        "rdfs:label",
        { value: labelSplit.value, lang: labelSplit.lang },
      ]);
    } else {
      // Untagged literal; emit as bare string so it writes lang="".
      attrs.push(["rdfs:label", labelSplit.value]);
    }

    // 2. rdf:type second (only when buildSlurp inferred one).
    if (entry.rdfType) {
      attrs.push(["rdf:type", entry.rdfType]);
    }

    // 3. Arbitrary properties in document order.  Keep tagged
    //    literals tagged so the lang picker sees them downstream
    //    (e.g. multi-lingual rdfs:comment values).
    for (const p of entry.properties ?? []) {
      const ls = splitLangTag(p.value);
      if (ls.lang) {
        attrs.push([p.tag, { value: ls.value, lang: ls.lang }]);
      } else {
        attrs.push([p.tag, ls.value]);
      }
    }

    entries.push({
      id: uri,
      // Denormalised entities.label gets the picker-friendly plain
      // form (no enclosing quotes / lang tag); the tagged copy
      // lives in the rdfs:label attribute row above.
      label: labelSplit.value,
      attrs,
    });
  }

  // Harvest prefix declarations from the :prefixdefs: block, if any.
  const prefixMap = getPrefixMap(root);
  const prefixes: Array<[string, string]> = [];
  if (prefixMap) {
    for (const [prefix, iri] of prefixMap) {
      prefixes.push([prefix, iri]);
    }
  }

  return prefixes.length > 0 ? { entries, prefixes } : { entries };
}
