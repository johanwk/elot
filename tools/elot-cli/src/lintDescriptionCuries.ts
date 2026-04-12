// src/lintDescriptionCuries.ts
//
// Stage 5: Description list CURIE checker (checker #6).
// Validates that CURIE-shaped description list tags are declared
// annotation properties. Pure TypeScript — no VS Code imports.

import type { ElotNode } from "./types.js";
import type { LintDiagnostic } from "./elotLintHelpers.js";
import { walkResourceNodes } from "./elotLintHelpers.js";
import { isOmnKeyword } from "./omnKeywords.js";
import { buildSlurp } from "./buildSlurp.js";
import type { SlurpEntry } from "./buildSlurp.js";

// ─── Constants ──────────────────────────────────────────────────

/**
 * Known annotation properties and structural predicates that are always
 * allowed in description lists without being declared in the ontology.
 *
 * Mirrors `elot-known-annotation-properties` from elot-lint.el, plus
 * `rdf:type` which is auto-injected by the parser (parseOrgWasm.ts
 * infers it from the governing section's :ID: suffix and inserts a
 * synthetic `rdf:type` description item).
 */
const KNOWN_ANNOTATION_PROPERTIES = new Set([
  "rdfs:label",
  "rdfs:comment",
  "rdfs:seeAlso",
  "rdfs:isDefinedBy",
  "rdf:type",
]);

/**
 * Regex matching CURIE-shaped strings: optional prefix, colon,
 * optional local name. Allows hyphens, underscores, dots, slashes,
 * and word characters on both sides.
 *
 * Mirrors the Elisp regex:
 *   "\\`[-_./[:alnum:]]*:[-_/.[:alnum:]]*\\'"
 */
const CURIE_REGEX = /^[-_./\w]*:[-_./\w]*$/;

// ─── Checker #6 ─────────────────────────────────────────────────

/**
 * Check that CURIE-shaped description list tags are declared annotation
 * properties.
 *
 * Mirrors `elot-check-description-list-curies` from elot-lint.el.
 *
 * For each node inside ELOT scope, iterates over `node.descriptions`.
 * For each description item whose `tag` matches the CURIE pattern:
 * - Skip if it's an OMN keyword (axiom, not annotation)
 * - Skip if it's a known annotation property (rdfs:label, etc.)
 * - Skip if the slurp map contains it with rdfType "owl:AnnotationProperty"
 * - Otherwise: WARNING
 *
 * @param root - The parsed ElotNode root (level 0)
 * @param slurpMap - Optional pre-built slurp map; if omitted, built from root
 * @returns Array of lint diagnostics
 */
export function checkDescriptionListCuries(
  root: ElotNode,
  slurpMap?: Map<string, SlurpEntry>,
): LintDiagnostic[] {
  const diagnostics: LintDiagnostic[] = [];
  const slurp = slurpMap ?? buildSlurp(root);

  walkResourceNodes(root, (node, _ancestors) => {
    const descriptions = node.descriptions ?? [];

    for (const desc of descriptions) {
      const tag = desc.tag;

      // Only check tags that look like CURIEs
      if (!CURIE_REGEX.test(tag)) continue;

      // Skip OMN keywords — they are axiom lines, not annotation properties
      if (isOmnKeyword(tag)) continue;

      // Skip known annotation properties (always allowed)
      if (KNOWN_ANNOTATION_PROPERTIES.has(tag)) continue;

      // Skip if declared as AnnotationProperty in the slurp map
      const entry = slurp.get(tag);
      if (entry && entry.rdfType === "owl:AnnotationProperty") continue;

      // Not found or not an annotation property → warn
      diagnostics.push({
        node,
        message: `Unknown or invalid annotation property: ${tag}`,
        severity: "warning",
      });
    }
  });

  return diagnostics;
}
