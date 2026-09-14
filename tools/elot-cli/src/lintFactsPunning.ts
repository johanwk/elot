// src/lintFactsPunning.ts
//
// Punning checker (checker #9).
// Port of `elot-check-facts-punning` from elot-lint.el (commit 5ec7000).
// Pure TypeScript — no VS Code imports.

import type { ElotNode } from "./types.js";
import type { LintDiagnostic } from "./elotLintHelpers.js";
import { walkResourceNodes } from "./elotLintHelpers.js";
import { buildSlurp } from "./buildSlurp.js";
import type { SlurpEntry } from "./buildSlurp.js";
import { extractCurieTokens } from "./lintAxiomValues.js";

/**
 * The description-list tags whose values name individuals.
 * Mirrors the `'("Facts" "SameAs" "DifferentFrom")' list in
 * `elot-check-facts-punning'.
 */
const INDIVIDUAL_ROW_KEYWORDS = new Set([
  "Facts",
  "SameAs",
  "DifferentFrom",
]);

/**
 * Check for punned class CURIEs that are not also declared as individuals.
 *
 * Manchester syntax requires an explicit declaration for punned entities:
 * if a CURIE declared as an `owl:Class` appears in a `Facts`, `SameAs` or
 * `DifferentFrom` row, it must also be declared as an `owl:NamedIndividual`
 * in the same file.
 *
 * A CURIE that is also declared as an object or data property is *not*
 * flagged in a `Facts` row, since it may legitimately occupy the property
 * position there.  `SameAs` and `DifferentFrom` take individuals only, so
 * the property exemption does not apply.
 *
 * Mirrors `elot-check-facts-punning` from elot-lint.el.
 *
 * @param root - The parsed ElotNode root (level 0)
 * @param slurpMap - Optional pre-built slurp map; if omitted, built from root
 * @returns Array of lint diagnostics (severity "error")
 */
export function checkFactsPunning(
  root: ElotNode,
  slurpMap?: Map<string, SlurpEntry>,
): LintDiagnostic[] {
  const diagnostics: LintDiagnostic[] = [];
  const slurp = slurpMap ?? buildSlurp(root);

  // Partition the signature by declared type, as the Elisp does with
  // three hash tables.
  const classes = new Set<string>();
  const individuals = new Set<string>();
  const properties = new Set<string>();
  for (const [curie, entry] of slurp) {
    switch (entry.rdfType) {
      case "owl:Class":
        classes.add(curie);
        break;
      case "owl:NamedIndividual":
        individuals.add(curie);
        break;
      case "owl:ObjectProperty":
      case "owl:DatatypeProperty":
        properties.add(curie);
        break;
      default:
        break;
    }
  }

  walkResourceNodes(root, (node) => {
    for (const desc of node.descriptions ?? []) {
      if (!INDIVIDUAL_ROW_KEYWORDS.has(desc.tag)) continue;

      for (const curie of extractCurieTokens(desc.value)) {
        if (!classes.has(curie)) continue;
        if (individuals.has(curie)) continue;
        // In a Facts row the token may be the property, not the object.
        if (desc.tag === "Facts" && properties.has(curie)) continue;

        diagnostics.push({
          node,
          message:
            `Class '${curie}' used as an individual must be explicitly ` +
            `declared as an Individual (punning).`,
          severity: "error",
        });
      }
    }
  });

  return diagnostics;
}
