// src/omnFrame.ts
//
// Port of `elot-omn-resource-frame`, `elot-omn-misc-frames`,
// `elot-omn-format-annotations`, and `elot-omn-format-restrictions`
// from elot-tangle.el
//
// Generates OMN frame strings for individual resources and standalone axioms.

import type { ElotNode, DescriptionItem } from "./types.js";
import { RDF_TYPE_TO_OMN_KEYWORD } from "./types.js";
import {
  isPropertyKeyword,
  isMiscKeyword,
  isOmnKeyword,
} from "./omnKeywords.js";
import { annotationStringOrUri } from "./annotationValue.js";

/**
 * Ensure a URI string is wrapped in angle brackets.
 * If it already has them, return as-is.  If it's a bare http(s) URI, wrap it.
 * For CURIEs or other values, return as-is (they don't need brackets).
 */
function ensureAngleBrackets(val: string): string {
  const trimmed = val.trim();
  // Already wrapped
  if (trimmed.startsWith("<") && trimmed.endsWith(">")) return trimmed;
  // Bare URI — wrap it
  if (/^https?:\/\/\S+$/.test(trimmed)) return `<${trimmed}>`;
  // Anything else (CURIE, complex expression) — pass through
  return trimmed;
}

/**
 * Indent all lines of a string (optionally skipping the first one).
 */
function indentLines(
  text: string,
  indentLevel: number,
  skipFirst: boolean
): string {
  const lines = text.split("\n");
  if (lines.length <= 1) return text;

  const ind = " ".repeat(indentLevel);
  return lines
    .map((line, i) => {
      if (i === 0 && skipFirst) return line;
      // For subsequent lines, we trim existing indentation and add our own.
      return ind + line.trimStart();
    })
    .join("\n");
}

/**
 * Format a list of annotations with the given indent level.
 *
 * Port of `elot-omn-format-annotations`.
 *
 * Each annotation is "key value" where value is formatted by
 * annotationStringOrUri. Meta-annotations are rendered recursively
 * with "Annotations:" prefix.
 */
export function formatAnnotations(
  items: DescriptionItem[],
  indentLevel: number,
  prefixMap: Map<string, string> | null
): string {
  const ind = " ".repeat(indentLevel);
  return items
    .map((item) => {
      const key = item.tag;
      const val = item.value;
      const meta = item.meta ?? [];

      let metaBlock = "";
      if (meta.length > 0) {
        metaBlock =
          ind +
          "Annotations: \n" +
          formatAnnotations(meta, indentLevel + 4, prefixMap) +
          "\n";
      }

      const formattedVal = annotationStringOrUri(val, prefixMap);
      // Only re-indent continuation lines for multi-line values that are
      // NOT quoted strings.  Newlines inside quoted strings (e.g.
      // rdfs:comment) are content, not formatting — re-indenting them
      // would inject whitespace into the string literal.
      const trimmedVal = formattedVal.trimStart();
      const isQuotedString = /^"/.test(trimmedVal);
      const subInd = indentLevel + key.length + 1; // +1 for the space after key
      const finalVal = isQuotedString
        ? trimmedVal
        : indentLines(trimmedVal, subInd, true);

      return metaBlock + ind + key + " " + finalVal;
    })
    .join(",\n");
}

/**
 * Format a list of restrictions (axiom lines) with the given indent level.
 *
 * Port of `elot-omn-format-restrictions`.
 *
 * Each restriction is "Keyword: value". If there are meta-annotations,
 * they're rendered as an "Annotations:" block before the value.
 */
export function formatRestrictions(
  items: DescriptionItem[],
  indentLevel: number,
  prefixMap: Map<string, string> | null
): string {
  const ind = " ".repeat(indentLevel);
  return items
    .map((item) => {
      const key = item.tag;
      const val = item.value;
      const meta = item.meta ?? [];

      // For OMN keywords, the value is passed through literally —
      // EXCEPT for Import, where we wrap bare URIs in <...> for user convenience.
      let formattedVal: string;
      if (key === "Import") {
        formattedVal = ensureAngleBrackets(val);
      } else if (isOmnKeyword(key)) {
        formattedVal = val;
      } else {
        formattedVal = annotationStringOrUri(val, prefixMap).trimStart();
      }

      // For OMN keywords the value already carries its original indentation
      // from the Org source (preserved by the parser), so pass it through
      // without re-indenting.  For non-OMN values, apply computed indent.
      const subInd = indentLevel + key.length + 2; // +2 for ": "
      const finalVal = isOmnKeyword(key)
        ? formattedVal
        : indentLines(formattedVal, subInd, true);

      if (meta.length > 0) {
        // Rule annotations are not supported in Manchester Syntax
        // (OWLAPI parser rejects them), so emit a warning comment
        // and drop the annotations.
        if (key === "Rule") {
          return (
            ind +
            key +
            ": # WARNING: Rule annotations are not supported in Manchester Syntax\n" +
            " ".repeat(indentLevel + 4) +
            finalVal
          );
        }
        const metaInd = " ".repeat(indentLevel + 4);
        return (
          ind +
          key +
          ": \n" +
          metaInd +
          "Annotations: \n" +
          formatAnnotations(meta, indentLevel + 8, prefixMap) +
          "\n" +
          metaInd +
          finalVal
        );
      }

      return ind + key + ": " + finalVal;
    })
    .join("\n");
}

/**
 * Generate a resource frame string for an ElotNode.
 *
 * Port of `elot-omn-resource-frame`.
 *
 * @param node       - The ElotNode to generate a frame for
 * @param parentUri  - The parent resource URI (for implicit SubClassOf/SubPropertyOf)
 * @param prefixMap  - Prefix map for CURIE expansion
 * @returns OMN frame string, or null if the node doesn't declare a resource
 */
export function omnResourceFrame(
  node: ElotNode,
  parentUri: string | null,
  prefixMap: Map<string, string> | null
): string | null {
  const uri = node.uri;
  const desc = node.descriptions ?? [];
  const tags = node.tags ?? [];

  // Skip if no URI or tagged :nodeclare:
  if (!uri || tags.includes("nodeclare")) return null;

  // Find the rdf:type
  const typeEntry = desc.find((d) => d.tag === "rdf:type");
  const rdfType = typeEntry?.value ?? null;

  // Partition descriptions
  const annotations: DescriptionItem[] = [];
  const restrictions: DescriptionItem[] = [];

  // Auto-add rdfs:label from the heading label if it differs from the URI
  // and isn't already present with the same value (there may be other
  // rdfs:label entries with different language tags — those are kept).
  const headingLabel = node.label;
  if (headingLabel && headingLabel !== uri) {
    const alreadyPresent = desc.some(
      (d) => d.tag === "rdfs:label" && d.value === headingLabel
    );
    if (!alreadyPresent) {
      annotations.push({ tag: "rdfs:label", value: headingLabel });
    }
  }

  for (const d of desc) {
    if (isPropertyKeyword(d.tag)) {
      restrictions.push(d);
    } else if (isMiscKeyword(d.tag)) {
      // Skip — handled by omnMiscFrames
    } else if (d.tag === "rdf:type") {
      // Skip — used for frame type
    } else {
      annotations.push(d);
    }
  }

  // Add inferred taxonomy from parent
  if (parentUri) {
    if (rdfType === "owl:Class") {
      const hasExplicit = restrictions.some(
        (r) => r.tag === "SubClassOf" && r.value === parentUri
      );
      if (!hasExplicit) {
        restrictions.push({ tag: "SubClassOf", value: parentUri });
      }
    } else if (rdfType && rdfType.includes("Property")) {
      const hasExplicit = restrictions.some(
        (r) => r.tag === "SubPropertyOf" && r.value === parentUri
      );
      if (!hasExplicit) {
        restrictions.push({ tag: "SubPropertyOf", value: parentUri });
      }
    }
  }

  // Determine OMN frame keyword
  const omnType =
    (rdfType && RDF_TYPE_TO_OMN_KEYWORD[rdfType]) ??
    (rdfType ? rdfType.replace(/^.*:/, "") : "Class");

  const frameParts: string[] = [];

  // Ontology frames have special syntax: the URI may contain both
  // ontology IRI and version IRI (e.g. "obo:bfo.owl <http://...>")
  frameParts.push(`${omnType}: ${uri}`);

  if (annotations.length > 0) {
    frameParts.push(
      "    Annotations: \n" +
        formatAnnotations(annotations, 8, prefixMap)
    );
  }

  if (restrictions.length > 0) {
    frameParts.push(formatRestrictions(restrictions, 4, prefixMap));
  }

  return frameParts.join("\n");
}

/**
 * Generate standalone misc-frame strings (e.g. DisjointClasses:) from a node.
 *
 * Port of `elot-omn-misc-frames`.
 */
export function omnMiscFrames(
  node: ElotNode,
  prefixMap: Map<string, string> | null
): string[] {
  const frames: string[] = [];
  for (const d of node.descriptions ?? []) {
    if (isMiscKeyword(d.tag)) {
      frames.push(formatRestrictions([d], 0, prefixMap));
    }
  }
  return frames;
}
