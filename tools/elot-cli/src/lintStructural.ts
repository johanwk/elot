// src/lintStructural.ts
//
// Stage 2: Structural lint checkers (ontology presence, header, prefix table).
// Pure TypeScript — no VS Code imports.

import type { ElotNode } from "./types.js";
import type { LintDiagnostic } from "./elotLintHelpers.js";
import { findOntologyRoots } from "./elotLintHelpers.js";
import { buildPrefixMap } from "./prefixes.js";

// ─── Local prefix map helper (avoids importing parseOrgWasm/WASM) ─

/**
 * Find the prefix map from the tree by locating the `prefixdefs` node.
 * Equivalent to `getPrefixMap` from `parseOrgWasm.ts` but without
 * the WASM import, so this module stays testable without the extension host.
 */
function getPrefixMapFromTree(root: ElotNode): Map<string, string> | null {
  const stack: ElotNode[] = [root];
  while (stack.length > 0) {
    const node = stack.pop()!;
    if (node.prefixdefs && (node.prefixes ?? []).length > 0) {
      return buildPrefixMap(node.prefixes!);
    }
    const children = node.children ?? [];
    for (let i = children.length - 1; i >= 0; i--) {
      stack.push(children[i]);
    }
  }
  return null;
}

// ─── Checker #1: Ontology presence ──────────────────────────────

/**
 * Ensure at least one top-level heading has `:ELOT-context-type: ontology`.
 *
 * Returns one ERROR diagnostic if no ontology heading is found.
 */
export function checkOntologyPresence(root: ElotNode): LintDiagnostic[] {
  const ontologyRoots = findOntologyRoots(root);
  if (ontologyRoots.length === 0) {
    return [
      {
        node: root,
        message:
          "No top-level heading with :ELOT-context-type: ontology",
        severity: "error",
      },
    ];
  }
  return [];
}

// ─── Checker #2: Ontology header ────────────────────────────────

/**
 * Validate the top-level ontology heading's properties:
 * - :ID: must be present and non-empty.
 * - :ELOT-context-localname: should match :ID:.
 * - :header-args:omn: must have a valid .omn tangle target.
 * - :ELOT-default-prefix: should be in the prefix table.
 *
 * Returns ERROR/WARNING diagnostics.
 */
export function checkOntologyHeader(root: ElotNode): LintDiagnostic[] {
  const diagnostics: LintDiagnostic[] = [];
  const ontologyRoots = findOntologyRoots(root);
  const prefixMap = getPrefixMapFromTree(root);

  for (const onto of ontologyRoots) {
    // Check :ID:
    if (!onto.id || onto.id.trim() === "") {
      diagnostics.push({
        node: onto,
        message: "Top-level heading missing :ID:",
        severity: "error",
      });
    }

    // Check :ELOT-context-localname: matches :ID:
    if (
      onto.id &&
      onto.elotContextLocalname &&
      onto.id !== onto.elotContextLocalname
    ) {
      diagnostics.push({
        node: onto,
        message: ":ELOT-context-localname: should match :ID:",
        severity: "warning",
      });
    }

    // Check :header-args:omn: → tangleTargetOmn
    if (!onto.tangleTargetOmn || !onto.tangleTargetOmn.endsWith(".omn")) {
      diagnostics.push({
        node: onto,
        message: ":tangle missing or invalid in :header-args:omn:",
        severity: "error",
      });
    }

    // TODO: Check :noweb yes in :header-args:omn: — the WASM parser doesn't
    // extract arbitrary header-args, so we skip this sub-check for now.

    // Check :ELOT-default-prefix: is in the prefix table
    if (onto.elotDefaultPrefix && prefixMap) {
      // Strip trailing colon for comparison — prefixMap keys have no colon
      const prefixKey = onto.elotDefaultPrefix.endsWith(":")
        ? onto.elotDefaultPrefix.slice(0, -1)
        : onto.elotDefaultPrefix;
      if (!prefixMap.has(prefixKey)) {
        diagnostics.push({
          node: onto,
          message:
            ":ELOT-default-prefix: is not defined in the prefix table",
          severity: "warning",
        });
      }
    }
  }

  return diagnostics;
}

// ─── Checker #3: Prefix table ───────────────────────────────────

/**
 * Find the descendant with `prefixdefs === true` under each ontology root.
 * Iterative search to avoid recursion limits.
 */
function findPrefixDefsNode(ontologyRoot: ElotNode): ElotNode | null {
  const stack: ElotNode[] = [ontologyRoot];
  while (stack.length > 0) {
    const node = stack.pop()!;
    if (node.prefixdefs) return node;
    const children = node.children ?? [];
    for (let i = children.length - 1; i >= 0; i--) {
      stack.push(children[i]);
    }
  }
  return null;
}

/**
 * Ensure each ontology heading has a non-empty prefix table.
 *
 * Returns ERROR diagnostics if:
 * - No descendant with `prefixdefs: true` exists.
 * - The prefixdefs node has no `prefixes` or `prefixes` is empty.
 */
export function checkPrefixTable(root: ElotNode): LintDiagnostic[] {
  const diagnostics: LintDiagnostic[] = [];
  const ontologyRoots = findOntologyRoots(root);

  for (const onto of ontologyRoots) {
    const prefixNode = findPrefixDefsNode(onto);
    if (!prefixNode) {
      diagnostics.push({
        node: onto,
        message: "Prefix table is missing under ontology heading",
        severity: "error",
      });
    } else if (!prefixNode.prefixes || prefixNode.prefixes.length === 0) {
      diagnostics.push({
        node: prefixNode,
        message: "Prefix table is empty",
        severity: "error",
      });
    }
  }

  return diagnostics;
}
