// src/omnDeclarations.ts
//
// Port of `elot-omn-resource-declarations` from elot-tangle.el
//
// Recursively traverses the ElotNode tree and generates OMN frames.

import type { ElotNode } from "./types.js";
import { omnResourceFrame, omnMiscFrames } from "./omnFrame.js";

/**
 * Recursively generate OMN resource declarations from ElotNode children.
 *
 * Port of `elot-omn-resource-declarations`.
 *
 * @param nodes     - Array of ElotNode children to process
 * @param parentUri - The parent's URI (for implicit SubClassOf/SubPropertyOf)
 * @param prefixMap - Prefix map for CURIE expansion
 * @returns A single string with all frames separated by blank lines
 */
export function omnResourceDeclarations(
  rootNodes: ElotNode[],
  rootParentUri: string | null,
  prefixMap: Map<string, string> | null
): string {
  // Iterative DFS to avoid call-stack overflow on deeply nested ontology hierarchies.
  // Each stack entry carries the node to process and its effective parent URI.
  // We also need to collect frames in document order, so we use a two-pass approach:
  // push a sentinel after pushing children so we know when all children of a node
  // have been processed — but for flat frame collection a simple ordered stack suffices.

  type StackEntry = { node: ElotNode; parentUri: string | null };
  const stack: StackEntry[] = [];

  // Seed in reverse so first node is processed first
  for (let i = rootNodes.length - 1; i >= 0; i--) {
    stack.push({ node: rootNodes[i], parentUri: rootParentUri });
  }

  const frames: string[] = [];

  while (stack.length > 0) {
    const { node, parentUri } = stack.pop()!;
    const uri = node.uri;
    const children = node.children ?? [];
    const isNodeclare = (node.tags ?? []).includes("nodeclare");

    // 1. Try to generate a resource frame
    const resFrame = omnResourceFrame(node, parentUri, prefixMap);
    if (resFrame) {
      frames.push(resFrame);
    }

    // 2. Generate misc frames (e.g. DisjointClasses:)
    const miscFrames = omnMiscFrames(node, prefixMap);
    for (const m of miscFrames) {
      frames.push(m);
    }

    // 3. Push children onto the stack in reverse order so they process left-to-right
    if (children.length > 0) {
      // Determine effective parent URI for children:
      // - If this node has a URI and is not :nodeclare:, it becomes the parent
      // - :nodeclare: nodes are transparent — they pass the current parentUri through
      // - Wrapper nodes without URI (like "Classes", "Datatypes") pass parentUri through
      const effectiveParent =
        !isNodeclare && uri && typeof uri === "string" ? uri : parentUri;
      for (let i = children.length - 1; i >= 0; i--) {
        stack.push({ node: children[i], parentUri: effectiveParent });
      }
    }
  }

  return frames.join("\n\n");
}
