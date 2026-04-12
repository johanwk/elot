// src/buildSlurp.ts
//
// Port of `elot-build-slurp` from elot-tangling.org.
//
// Walks an ElotNode tree and builds a map from CURIE/URI → label,
// plus optional extra description properties per entity.
// This is the foundation for label-display features in VS Code
// (hover, inlay hints, decoration toggling).

import type { ElotNode, DescriptionItem } from "./types.js";

/**
 * A single "slurp" entry: everything we know about one OWL entity
 * from the Org-mode source.
 */
export interface SlurpEntry {
  /** The OWL identifier: CURIE or <URI> */
  uri: string;
  /** The human-readable label (from heading title), or the URI itself */
  label: string;
  /** The rdf:type, e.g. "owl:Class" */
  rdfType?: string;
  /** Additional description properties (tag to value pairs), excluding rdf:type and rdfs:label */
  properties?: Array<{ tag: string; value: string }>;
}

/**
 * Build a CURIE→label map by walking the ElotNode tree.
 *
 * This is the TypeScript equivalent of `elot-build-slurp` from
 * elot-tangling.org. It iterates (non-recursively) over all nodes,
 * collecting URI, label, rdfType, and description properties.
 *
 * @param root - The parsed ElotNode root (level 0)
 * @returns Map from URI string to SlurpEntry
 */
export function buildSlurp(root: ElotNode): Map<string, SlurpEntry> {
  const result = new Map<string, SlurpEntry>();
  const stack: ElotNode[] = [root];

  while (stack.length > 0) {
    const node = stack.pop()!;

    if (node.uri) {
      const uri = node.uri;
      const label = node.label ?? uri;

      // Collect extra properties from descriptions
      const properties: Array<{ tag: string; value: string }> = [];
      let rdfType: string | undefined = node.rdfType ?? undefined;

      for (const desc of node.descriptions ?? []) {
        if (desc.tag === "rdf:type") {
          // Use explicit rdf:type if present
          if (!rdfType) rdfType = desc.value;
        } else if (desc.tag === "rdfs:label") {
          // Skip — we already have the label from the heading
        } else {
          properties.push({ tag: desc.tag, value: desc.value });
        }
      }

      const entry: SlurpEntry = { uri, label };
      if (rdfType) entry.rdfType = rdfType;
      if (properties.length > 0) entry.properties = properties;

      result.set(uri, entry);
    }

    // Push children in reverse order so we process in document order
    const children = node.children ?? [];
    for (let i = children.length - 1; i >= 0; i--) {
      stack.push(children[i]);
    }
  }

  return result;
}

/**
 * Build a simple CURIE→label map (convenience wrapper).
 *
 * This is the most common use case: you just want to look up
 * the human-readable label for a given CURIE.
 *
 * @param root - The parsed ElotNode root (level 0)
 * @returns Map from URI string to label string
 */
export function buildLabelMap(root: ElotNode): Map<string, string> {
  const slurp = buildSlurp(root);
  const labelMap = new Map<string, string>();
  for (const [uri, entry] of slurp) {
    labelMap.set(uri, entry.label);
  }
  return labelMap;
}
