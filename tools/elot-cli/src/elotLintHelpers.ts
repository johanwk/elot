// src/elotLintHelpers.ts
//
// Shared helper functions for all ELOT lint checkers.
// Pure TypeScript — no VS Code imports — so it's testable without the extension host.

import type { ElotNode } from "./types.js";
import { ID_SUFFIX_TO_TYPE } from "./types.js";

// ─── LintDiagnostic interface ────────────────────────────────────

/**
 * A single lint diagnostic produced by a checker function.
 * This is a pure data object; mapping to VS Code Diagnostic happens
 * in diagnosticsProvider.ts (Stage 7).
 */
export interface LintDiagnostic {
  /** The ElotNode that triggered the diagnostic */
  node: ElotNode;
  /** Optional line number (1-based) for more precise location */
  line?: number;
  /** Human-readable message */
  message: string;
  /** Severity level */
  severity: "error" | "warning";
}

// ─── Scoping helpers ─────────────────────────────────────────────

/**
 * Is this node inside an ontology context?
 * Returns true if any ancestor has `elotContextType === "ontology"`.
 */
export function isInsideOntologyContext(
  _node: ElotNode,
  ancestors: ElotNode[],
): boolean {
  return ancestors.some((a) => a.elotContextType === "ontology");
}

/**
 * Is this node inside a resourcedefs section?
 * Returns true if the node itself or any ancestor has `resourcedefs === true`.
 */
export function isInsideResourcedefs(
  node: ElotNode,
  ancestors: ElotNode[],
): boolean {
  if (node.resourcedefs) return true;
  return ancestors.some((a) => a.resourcedefs === true);
}

/**
 * Combined guard: the node is both inside an ontology context and
 * inside a resourcedefs section.
 */
export function isInsideElotScope(
  node: ElotNode,
  ancestors: ElotNode[],
): boolean {
  return (
    isInsideOntologyContext(node, ancestors) &&
    isInsideResourcedefs(node, ancestors)
  );
}

// ─── Section suffix detection ────────────────────────────────────

/** All known :ID: suffixes (from ID_SUFFIX_TO_TYPE + ontology-declaration). */
const KNOWN_SUFFIXES: string[] = [
  ...Object.keys(ID_SUFFIX_TO_TYPE),
  "-ontology-declaration",
];

/**
 * Walk the node and its ancestors to find the nearest :ID: ending
 * in a well-known suffix. Returns the suffix string (e.g.
 * `"-class-hierarchy"`) or `null` if none found.
 *
 * Checks the node's own `id` first, then walks ancestors from
 * nearest to farthest.
 */
export function getSectionSuffix(
  node: ElotNode,
  ancestors: ElotNode[],
): string | null {
  // Check node itself first
  const nodeId = node.id;
  if (nodeId) {
    for (const suffix of KNOWN_SUFFIXES) {
      if (nodeId.endsWith(suffix)) return suffix;
    }
  }
  // Walk ancestors from nearest (end of array) to farthest (start)
  for (let i = ancestors.length - 1; i >= 0; i--) {
    const aid = ancestors[i].id;
    if (aid) {
      for (const suffix of KNOWN_SUFFIXES) {
        if (aid.endsWith(suffix)) return suffix;
      }
    }
  }
  return null;
}

// ─── Ontology root finder ────────────────────────────────────────

/**
 * Return all level-1 children of root that have
 * `elotContextType === "ontology"`.
 */
export function findOntologyRoots(root: ElotNode): ElotNode[] {
  return (root.children ?? []).filter(
    (child) => child.level === 1 && child.elotContextType === "ontology",
  );
}

// ─── :nodeclare: check ──────────────────────────────────────────

/**
 * Is this node (or any ancestor) tagged `:nodeclare:`?
 */
export function isNodeclare(
  node: ElotNode,
  ancestors: ElotNode[],
): boolean {
  if (node.tags?.includes("nodeclare")) return true;
  return ancestors.some((a) => a.tags?.includes("nodeclare"));
}

// ─── CURIE prefix validation ────────────────────────────────────

/**
 * Check whether a CURIE's prefix is present in the prefix map.
 *
 * @param curie - A CURIE like `"obo:BFO_0000001"`
 * @param prefixMap - Map from prefix (e.g. `"obo"`) to namespace URI
 * @returns `true` if the prefix part is found in the map
 */
export function isCurieKnown(
  curie: string,
  prefixMap: Map<string, string>,
): boolean {
  const colonIdx = curie.indexOf(":");
  if (colonIdx < 0) return false;
  const prefix = curie.substring(0, colonIdx);
  return prefixMap.has(prefix);
}

// ─── Tree walker ─────────────────────────────────────────────────

/**
 * Walk the ElotNode tree iteratively, calling `callback(node, ancestors)`
 * for every node that passes `isInsideElotScope`.
 *
 * The `ancestors` array is maintained as a stack during traversal and
 * represents the path from the root to the node's parent (does NOT
 * include the node itself).
 *
 * Traversal is in document order (depth-first, children left to right).
 */
export function walkResourceNodes(
  root: ElotNode,
  callback: (node: ElotNode, ancestors: ElotNode[]) => void,
): void {
  // Stack items: [node, ancestors]
  const stack: Array<[ElotNode, ElotNode[]]> = [[root, []]];

  while (stack.length > 0) {
    const [node, ancestors] = stack.pop()!;

    // Call the callback if this node is inside ELOT scope
    if (isInsideElotScope(node, ancestors)) {
      callback(node, ancestors);
    }

    // Push children in reverse order so leftmost child is popped first
    const children = node.children ?? [];
    const childAncestors = [...ancestors, node];
    for (let i = children.length - 1; i >= 0; i--) {
      stack.push([children[i], childAncestors]);
    }
  }
}
