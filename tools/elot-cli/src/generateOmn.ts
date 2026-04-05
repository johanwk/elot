// src/generateOmn.ts
//
// Port of `elot-get-ontology-node-omn` from elot-tangle.el
//
// Top-level document assembly: combines prefix block + resource declarations,
// then appends any #+begin_src omn blocks found in the ElotNode tree.

import type { ElotNode } from "./types.js";
import { omnPrefixBlock } from "./omnPrefix.js";
import { omnResourceDeclarations } from "./omnDeclarations.js";
import { getPrefixMap, collectOmnSrcBlocks } from "./parseOrgWasm.js";

/**
 * Generate the complete OMN content for an ontology node.
 *
 * Port of `elot-get-ontology-node-omn`.
 *
 * @param node - An ontology-level ElotNode (L1 headline)
 * @param externalPrefixMap - Optional prefix map from root if not found in node
 * @param skipPrefixBlock - If true, don't generate the Prefix: block (e.g. if already added at root)
 * @returns Complete OMN string
 */
export function getOntologyNodeOmn(
  node: ElotNode,
  externalPrefixMap: Map<string, string> | null = null,
  skipPrefixBlock: boolean = false
): string {
  const localPrefixMap = getPrefixMap(node);
  const prefixMap = localPrefixMap || externalPrefixMap;

  const prefixBlock = skipPrefixBlock ? null : omnPrefixBlock(node);

  // The L1 ontology node itself doesn't produce a frame.
  // Its children (L2 headings) contain the ontology declaration,
  // resource sections, etc. We pass `null` as parentUri since
  // the L1 node has no OWL parent.
  const resources = omnResourceDeclarations(node.children ?? [], null, prefixMap);

  let result = "";
  if (prefixBlock) {
    result += prefixBlock;
    if (resources) {
      result += "\n\n";
    }
  }
  result += resources;

  return result;
}

/**
 * Generate the full OMN output for a parsed Org file, including any
 * omn source blocks appended at the end.
 *
 * @param root - The parsed ElotNode root (level 0)
 * @returns Complete OMN file content
 */
export function generateFullOmn(root: ElotNode): string {
  const parts: string[] = [];
  const prefixParts: string[] = [];

  // 1. Collect prefixes from root or sub-nodes
  const rootPrefixBlock = omnPrefixBlock(root);
  if (rootPrefixBlock) {
    prefixParts.push(rootPrefixBlock);
  }
  const rootPrefixMap = getPrefixMap(root);

  // 2. Process each ontology node
  for (const node of root.children ?? []) {
    const omn = getOntologyNodeOmn(node, rootPrefixMap, !!rootPrefixBlock);
    if (omn) {
      parts.push(omn);
    }
  }

  // 3. Append any omn source blocks from the ElotNode tree
  const srcBlocks = collectOmnSrcBlocks(root);
  for (const block of srcBlocks) {
    parts.push(block);
  }

  const allPrefixes = prefixParts.join("\n\n");
  const allResources = parts.join("\n\n");

  let finalResult = "";
  if (allPrefixes) finalResult += allPrefixes + "\n\n";
  finalResult += allResources;

  return finalResult.trimEnd() + "\n";
}
