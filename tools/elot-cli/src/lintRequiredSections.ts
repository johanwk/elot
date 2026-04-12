// src/lintRequiredSections.ts
//
// Stage 3: Required sections checker (#4).
// Pure TypeScript — no VS Code imports.

import type { ElotNode } from "./types.js";
import type { LintDiagnostic } from "./elotLintHelpers.js";
import { findOntologyRoots } from "./elotLintHelpers.js";

// ─── Required section suffixes ───────────────────────────────────

/**
 * The 7 required section :ID: suffixes for any ELOT ontology.
 * Each ontology with localname L must have sections with IDs:
 *   L-ontology-declaration, L-datatypes, L-class-hierarchy, etc.
 */
const REQUIRED_SUFFIXES: string[] = [
  "-ontology-declaration",
  "-datatypes",
  "-class-hierarchy",
  "-object-property-hierarchy",
  "-data-property-hierarchy",
  "-annotation-property-hierarchy",
  "-individuals",
];

// ─── Helper: collect all :ID: → node pairs under a subtree ──────

interface HeadlineEntry {
  node: ElotNode;
  resourcedefs: boolean;
}

/**
 * Walk the entire subtree under `root` and collect a map from :ID: to
 * the node and its resourcedefs status.
 */
function collectHeadlineIds(root: ElotNode): Map<string, HeadlineEntry> {
  const map = new Map<string, HeadlineEntry>();
  const stack: ElotNode[] = [root];
  while (stack.length > 0) {
    const node = stack.pop()!;
    if (node.id) {
      map.set(node.id, {
        node,
        resourcedefs: node.resourcedefs === true,
      });
    }
    const children = node.children ?? [];
    for (let i = children.length - 1; i >= 0; i--) {
      stack.push(children[i]);
    }
  }
  return map;
}

// ─── Checker #4: Required sections ──────────────────────────────

/**
 * Check that all 7 required section IDs exist under each ontology
 * heading and have `:resourcedefs: yes`.
 *
 * For each ontology root with `elotContextLocalname` L:
 * - WARNING if `L + suffix` is missing entirely.
 * - WARNING if present but `resourcedefs` is not `true`.
 *
 * Returns WARNING diagnostics.
 */
export function checkRequiredSections(root: ElotNode): LintDiagnostic[] {
  const diagnostics: LintDiagnostic[] = [];
  const ontologyRoots = findOntologyRoots(root);

  for (const onto of ontologyRoots) {
    const localname = onto.elotContextLocalname;
    if (!localname) continue;

    // Collect all :ID: values in the subtree under this ontology heading
    const headlineIds = collectHeadlineIds(onto);

    for (const suffix of REQUIRED_SUFFIXES) {
      const requiredId = localname + suffix;
      const entry = headlineIds.get(requiredId);

      if (!entry) {
        diagnostics.push({
          node: onto,
          message: `Missing section with ID ${requiredId}`,
          severity: "warning",
        });
      } else if (!entry.resourcedefs) {
        diagnostics.push({
          node: entry.node,
          message: `Section ${requiredId} should have :resourcedefs: yes`,
          severity: "warning",
        });
      }
    }
  }

  return diagnostics;
}
