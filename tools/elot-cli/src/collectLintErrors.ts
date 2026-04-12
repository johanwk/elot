// src/collectLintErrors.ts
//
// Stage 7: Aggregates all ELOT lint checkers into a single function.
// Pure TypeScript — no VS Code imports — so it's testable without the
// extension host.
//
// Each checker function takes an ElotNode tree and returns LintDiagnostic[].
// This module calls them all and merges the results.

import type { ElotNode } from "./types.js";
import type { LintDiagnostic } from "./elotLintHelpers.js";
import type { SlurpEntry } from "./buildSlurp.js";
import { buildSlurp } from "./buildSlurp.js";
import {
  checkOntologyPresence,
  checkOntologyHeader,
  checkPrefixTable,
} from "./lintStructural.js";
import { checkRequiredSections } from "./lintRequiredSections.js";
import { checkHeadingIdentity } from "./lintHeadingIdentity.js";
import { checkDescriptionListCuries } from "./lintDescriptionCuries.js";
import {
  checkAxiomValueCuries,
  checkOmnKeywordAppropriateness,
} from "./lintAxiomValues.js";

/**
 * Run all 8 ELOT lint checkers on the parsed ElotNode tree and
 * return the merged diagnostics.
 *
 * The slurp map is built once and shared between checkers that need it
 * (checkers #6 and #7).
 *
 * @param root - The parsed ElotNode root (level 0)
 * @returns All lint diagnostics from all checkers
 */
export function collectAllLintErrors(root: ElotNode): LintDiagnostic[] {
  // Build the slurp map once for all checkers that need it
  const slurpMap: Map<string, SlurpEntry> = buildSlurp(root);

  const diagnostics: LintDiagnostic[] = [
    // Structural checkers (#1–#3)
    ...checkOntologyPresence(root),
    ...checkOntologyHeader(root),
    ...checkPrefixTable(root),
    // Required sections (#4)
    ...checkRequiredSections(root),
    // Heading identity (#5)
    ...checkHeadingIdentity(root),
    // Description list CURIEs (#6)
    ...checkDescriptionListCuries(root, slurpMap),
    // Axiom value CURIEs (#7)
    ...checkAxiomValueCuries(root, slurpMap),
    // OMN keyword appropriateness (#8)
    ...checkOmnKeywordAppropriateness(root),
  ];

  return diagnostics;
}
