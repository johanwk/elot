// src/omnKeywords.ts
//
// Port of `elot-omn-property-keywords` and `elot-omn-misc-keywords`
// from elot-tangle.el
//
// These static keyword lists determine whether a description list tag
// is an OWL axiom keyword (restriction) or an annotation property.

/**
 * Property keywords: these appear as axiom lines within a resource frame.
 * E.g., `SubClassOf: obo:BFO_0000001`
 */
export const propertyKeywords: readonly string[] = [
  "EquivalentTo",
  "SubClassOf",
  "Characteristics",
  "DisjointWith",
  "DisjointUnionOf",
  "Domain",
  "Range",
  "InverseOf",
  "SubPropertyOf",
  "SubPropertyChain",
  "SameAs",
  "DifferentFrom",
  "Types",
  "Facts",
  "HasKey",
  "Import",
] as const;

/**
 * Misc keywords: these produce top-level standalone axioms (not inside a frame).
 * E.g., `DisjointClasses: obo:BFO_0000004, obo:BFO_0000020`
 */
export const miscKeywords: readonly string[] = [
  "DisjointClasses",
  "EquivalentClasses",
  "DisjointProperties",
  "EquivalentProperties",
  "SameIndividual",
  "DifferentIndividuals",
  "Rule",
] as const;

/**
 * Combined list of all OMN keywords (property + misc).
 */
export const allKeywords: readonly string[] = [
  ...propertyKeywords,
  ...miscKeywords,
] as const;

// Pre-built Sets for O(1) lookup

const propertyKeywordSet = new Set(propertyKeywords);
const miscKeywordSet = new Set(miscKeywords);
const allKeywordSet = new Set(allKeywords);

/**
 * Is `key` a property keyword (axiom within a resource frame)?
 */
export function isPropertyKeyword(key: string): boolean {
  return propertyKeywordSet.has(key);
}

/**
 * Is `key` a misc keyword (standalone top-level axiom)?
 */
export function isMiscKeyword(key: string): boolean {
  return miscKeywordSet.has(key);
}

/**
 * Is `key` any OMN keyword (property or misc)?
 */
export function isOmnKeyword(key: string): boolean {
  return allKeywordSet.has(key);
}
