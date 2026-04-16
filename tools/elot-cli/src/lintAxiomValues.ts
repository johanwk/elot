// src/lintAxiomValues.ts
//
// Stage 6: Axiom value CURIE checker (checker #7) and OMN keyword
// appropriateness checker (checker #8).
// Pure TypeScript — no VS Code imports.

import type { ElotNode } from "./types.js";
import type { LintDiagnostic } from "./elotLintHelpers.js";
import { walkResourceNodes, getSectionSuffix } from "./elotLintHelpers.js";
import { isOmnKeyword, isMiscKeyword } from "./omnKeywords.js";
import { buildSlurp } from "./buildSlurp.js";
import type { SlurpEntry } from "./buildSlurp.js";

// ─── Constants ──────────────────────────────────────────────────

/**
 * Built-in OWL/XSD/RDF resources that are always considered known.
 * Ported from `elot-owl-builtin-resources` in elot-tangle.el.
 */
export const BUILTIN_RESOURCES = new Set([
  "owl:Thing",
  "owl:Nothing",
  "owl:rational",
  "owl:real",
  "xsd:string",
  "xsd:boolean",
  "xsd:decimal",
  "xsd:integer",
  "xsd:float",
  "xsd:double",
  "xsd:dateTime",
  "xsd:dateTimeStamp",
  "xsd:time",
  "xsd:date",
  "xsd:gYear",
  "xsd:gMonth",
  "xsd:gDay",
  "xsd:gYearMonth",
  "xsd:gMonthDay",
  "xsd:hexBinary",
  "xsd:base64Binary",
  "xsd:anyURI",
  "xsd:normalizedString",
  "xsd:token",
  "xsd:language",
  "xsd:Name",
  "xsd:NCName",
  "xsd:NMTOKEN",
  "xsd:nonNegativeInteger",
  "xsd:nonPositiveInteger",
  "xsd:positiveInteger",
  "xsd:negativeInteger",
  "xsd:long",
  "xsd:unsignedLong",
  "xsd:int",
  "xsd:unsignedInt",
  "xsd:short",
  "xsd:unsignedShort",
  "xsd:byte",
  "xsd:unsignedByte",
  "rdf:PlainLiteral",
  "rdf:XMLLiteral",
  // OWL 2 §3.2 constraining facets
  "xsd:minInclusive",
  "xsd:maxInclusive",
  "xsd:minExclusive",
  "xsd:maxExclusive",
  "xsd:minLength",
  "xsd:maxLength",
  "xsd:length",
  "xsd:pattern",
  "rdf:langRange",
]);

/**
 * Regex matching CURIE-shaped tokens in axiom values.
 * Same pattern used in lintDescriptionCuries.ts.
 */
const CURIE_REGEX = /^[-_./:\w]*:[-_./:\w]*$/;

/**
 * Map from section ID suffix to allowed OMN frame keywords.
 * Ported from `elot-omn-keywords-by-section` in elot-lint.el.
 */
export const KEYWORDS_BY_SECTION: Record<string, string[]> = {
  "-datatypes": ["EquivalentTo"],
  "-class-hierarchy": [
    "SubClassOf",
    "EquivalentTo",
    "DisjointWith",
    "DisjointUnionOf",
    "HasKey",
  ],
  "-object-property-hierarchy": [
    "SubPropertyOf",
    "EquivalentTo",
    "DisjointWith",
    "Domain",
    "Range",
    "Characteristics",
    "InverseOf",
    "SubPropertyChain",
  ],
  "-data-property-hierarchy": [
    "SubPropertyOf",
    "EquivalentTo",
    "DisjointWith",
    "Domain",
    "Range",
    "Characteristics",
  ],
  "-annotation-property-hierarchy": ["SubPropertyOf", "Domain", "Range"],
  "-individuals": ["Types", "Facts", "SameAs", "DifferentFrom"],
};

/**
 * Human-readable names for section suffixes, for diagnostic messages.
 */
const SECTION_NAMES: Record<string, string> = {
  "-datatypes": "Datatypes",
  "-class-hierarchy": "Classes",
  "-object-property-hierarchy": "Object properties",
  "-data-property-hierarchy": "Data properties",
  "-annotation-property-hierarchy": "Annotation properties",
  "-individuals": "Individuals",
};

// ─── Helpers ────────────────────────────────────────────────────

/**
 * Check whether parentheses in a string are balanced.
 * Mirrors `elot-string-balanced-parentheses-p` from elot-lint.el.
 */
export function isParenthesesBalanced(str: string): boolean {
  let count = 0;
  for (let i = 0; i < str.length; i++) {
    const ch = str[i];
    if (ch === "(") {
      count++;
    } else if (ch === ")") {
      count--;
      if (count < 0) return false;
    }
  }
  return count === 0;
}

/**
 * Extract CURIE-shaped tokens from an axiom value string.
 * Splits on whitespace and commas, filters to CURIE pattern,
 * excludes http:// and https:// URLs.
 */
export function extractCurieTokens(value: string): string[] {
  const tokens = value.split(/[\s,]+/).filter((t) => t.length > 0);
  return tokens.filter(
    (t) => CURIE_REGEX.test(t) && !t.startsWith("http://") && !t.startsWith("https://"),
  );
}

// ─── Checker #7 ─────────────────────────────────────────────────

/**
 * Check that CURIEs in axiom values are declared and not annotation
 * properties used in non-annotation sections. Also checks balanced
 * parentheses.
 *
 * Mirrors `elot-check-axiom-value-curies` from elot-lint.el.
 *
 * @param root - The parsed ElotNode root (level 0)
 * @param slurpMap - Optional pre-built slurp map; if omitted, built from root
 * @returns Array of lint diagnostics
 */
export function checkAxiomValueCuries(
  root: ElotNode,
  slurpMap?: Map<string, SlurpEntry>,
): LintDiagnostic[] {
  const diagnostics: LintDiagnostic[] = [];
  const slurp = slurpMap ?? buildSlurp(root);

  walkResourceNodes(root, (node, ancestors) => {
    const descriptions = node.descriptions ?? [];

    for (const desc of descriptions) {
      const tag = desc.tag;

      // Only check descriptions whose tag is an OMN keyword
      if (!isOmnKeyword(tag)) continue;

      const value = desc.value;

      // Determine if we're in an annotation-property section
      const suffix = getSectionSuffix(node, ancestors);
      const inAnnotationSection =
        suffix === "-annotation-property-hierarchy";

      // Extract CURIE tokens from the value
      const curies = extractCurieTokens(value);

      for (const curie of curies) {
        // Skip built-in resources
        if (BUILTIN_RESOURCES.has(curie)) continue;

        // Look up in slurp map
        const entry = slurp.get(curie);

        if (entry) {
          // Known CURIE — warn if it's an annotation property in a
          // non-annotation section
          if (
            !inAnnotationSection &&
            entry.rdfType === "owl:AnnotationProperty"
          ) {
            diagnostics.push({
              node,
              message: `Annotation property used in axiom: ${curie}`,
              severity: "warning",
            });
          }
        } else {
          // Unknown CURIE
          diagnostics.push({
            node,
            message: `Unknown CURIE in axiom: ${curie}`,
            severity: "warning",
          });
        }
      }

      // Check balanced parentheses
      if (!isParenthesesBalanced(value)) {
        diagnostics.push({
          node,
          message: "Unbalanced parentheses in axiom value",
          severity: "warning",
        });
      }
    }
  });

  return diagnostics;
}

// ─── Checker #8 ─────────────────────────────────────────────────

/**
 * Check that OMN keywords are appropriate for their section type.
 *
 * Mirrors `elot-check-omn-keyword-appropriateness` from elot-lint.el.
 *
 * For each description list item whose tag is an OMN property keyword
 * (not misc), verify the keyword is valid for the enclosing section
 * according to the OWL 2 Manchester Syntax spec.
 *
 * @param root - The parsed ElotNode root (level 0)
 * @returns Array of lint diagnostics
 */
export function checkOmnKeywordAppropriateness(
  root: ElotNode,
): LintDiagnostic[] {
  const diagnostics: LintDiagnostic[] = [];

  walkResourceNodes(root, (node, ancestors) => {
    const descriptions = node.descriptions ?? [];

    for (const desc of descriptions) {
      const tag = desc.tag;

      // Only check OMN keywords
      if (!isOmnKeyword(tag)) continue;

      // Skip misc keywords — they're valid anywhere
      if (isMiscKeyword(tag)) continue;

      // Determine section suffix from ancestors
      const suffix = getSectionSuffix(node, ancestors);
      if (!suffix) continue; // Unknown section — can't validate

      // Look up allowed keywords for this section
      const allowed = KEYWORDS_BY_SECTION[suffix];
      if (!allowed) continue; // Section has no keyword restrictions (e.g. -ontology-declaration)

      // Check if the keyword is in the allowed list
      if (!allowed.includes(tag)) {
        const sectionName = SECTION_NAMES[suffix] ?? suffix;
        diagnostics.push({
          node,
          message: `"${tag}" is not valid in ${sectionName} section (allowed: ${allowed.join(", ")})`,
          severity: "error",
        });
      }
    }
  });

  return diagnostics;
}
