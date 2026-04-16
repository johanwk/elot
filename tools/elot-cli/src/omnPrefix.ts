// src/omnPrefix.ts
//
// Port of `elot-omn-prefix-block` from elot-tangle.el
//
// Formats prefix definitions as OMN Prefix: lines.

import type { ElotNode, PrefixEntry } from "./types.js";

/**
 * Find the prefix-defining descendant node and format the OMN prefix block.
 *
 * Port of `elot-omn-prefix-block`.
 */
export function omnPrefixBlock(ontologyNode: ElotNode): string | null {
  const prefixNode = findPrefixNode(ontologyNode);
  if (!prefixNode) return null;

  const prefixes = prefixNode.prefixes ?? [];
  if (!prefixes || prefixes.length === 0) return null;

  const lines = prefixes
    .filter((entry) => {
      // Skip header row if present (case-insensitive)
      const p = entry.prefix.toLowerCase();
      const u = entry.uri.toLowerCase();
      return !(p === "prefix" && u === "uri") && !(p === "prefix:" && u === "uri");
    })
    .map((entry) => {
      const pfx = entry.prefix.endsWith(":") ? entry.prefix : entry.prefix + ":";
      return `Prefix: ${pfx.padEnd(5)} <${entry.uri}>`;
    });

  if (lines.length === 0) return null;

  return "## Prefixes\n" + lines.join("\n");
}

/**
 * Recursively find the node with prefixdefs=true.
 */
function findPrefixNode(node: ElotNode): ElotNode | null {
  if (node.prefixdefs) return node;
  for (const child of node.children ?? []) {
    const found = findPrefixNode(child);
    if (found) return found;
  }
  return null;
}
