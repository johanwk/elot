// src/types.ts
//
// Core interfaces for the elot-cli pipeline.
// These mirror the plist structures used in the Elisp `elot-parse-headline-hierarchy`.

/**
 * A single description list item: tag :: value, with optional meta-annotations.
 *
 * In the Elisp, descriptions are stored as lists:
 *   ("skos:definition" "some text" ("rdfs:comment" "meta1") ("rdfs:comment" "meta2"))
 *
 * Here, `tag` and `value` are the primary pair, and `meta` holds
 * nested annotation items (meta-annotations on the annotation).
 */
export interface DescriptionItem {
  /** The description list tag, e.g. "skos:definition", "SubClassOf", "rdf:type" */
  tag: string;
  /** The description list body text */
  value: string;
  /** Optional nested meta-annotations (annotations on this annotation) */
  meta?: DescriptionItem[];
}

/**
 * A prefix table entry: maps a prefix name to a URI namespace.
 *
 * In the Org source, this comes from a table under a heading with `:prefixdefs: yes`:
 *   | obo: | http://purl.obolibrary.org/obo/ |
 *
 * The prefix string may or may not end with `:` — normalise at output time.
 */
export interface PrefixEntry {
  /** Prefix name, e.g. "obo" or "obo:" */
  prefix: string;
  /** Full namespace URI, e.g. "http://purl.obolibrary.org/obo/" */
  uri: string;
}

/**
 * Result of parsing a heading title with `entityFromHeader`.
 */
export interface EntityInfo {
  /** The OWL identifier: CURIE, <URI>, or "CURIE <versionURI>" */
  id: string;
  /** The human-readable label (text before the parenthesised identifier), or null */
  label: string | null;
}

/**
 * A node in the parsed ELOT hierarchy.
 *
 * Mirrors the Elisp plist returned by `elot-parse-headline-hierarchy`.
 * Each node corresponds to one Org headline and carries all information
 * needed to generate its OMN frame.
 */
export interface ElotNode {
  /** Org headline level (1, 2, 3, ...). The dummy root is level 0. */
  level: number;

  /** Raw heading text, e.g. '"entity"@en (obo:BFO_0000001)' */
  title: string;

  /** Org tags on the headline, e.g. ["nodeclare"] */
  tags?: string[];

  // --- Identity (only for headings that declare an OWL entity) ---

  /** The OWL identifier extracted by entityFromHeader, or undefined for wrapper headings */
  uri?: string;

  /** The human-readable label from the heading title */
  label?: string;

  /** The RDF type determined from ancestor context, e.g. "owl:Class" */
  rdfType?: string;

  // --- Context properties (from property drawers) ---

  /** The :ID: property, if present */
  id?: string;

  /** :ELOT-context-type: value, present only on the ontology root heading */
  elotContextType?: string;

  /** :ELOT-context-localname: value */
  elotContextLocalname?: string;

  /** :ELOT-default-prefix: value */
  elotDefaultPrefix?: string;

  /** :resourcedefs: "yes" if this heading defines OWL resources */
  resourcedefs?: boolean;

  /** :prefixdefs: "yes" if this heading contains prefix definitions */
  prefixdefs?: boolean;

  /** OMN tangle target file path, extracted from :header-args:omn: :tangle ... */
  tangleTargetOmn?: string;

  // --- Content ---

  /** Prefix definitions extracted from the table under this heading */
  prefixes?: PrefixEntry[];

  /** Description list items (annotations + axioms + rdf:type) */
  descriptions?: DescriptionItem[];

  /** Raw content of #+begin_src omn blocks under this heading */
  omnSrcBlocks?: string[];

  /** Child nodes (sub-headings) */
  children?: ElotNode[];
}

/**
 * Entity type constants — OWL/RDF types determined from ancestor :ID: suffixes.
 */
export const ENTITY_TYPES = {
  CLASS: "owl:Class",
  OBJECT_PROPERTY: "owl:ObjectProperty",
  DATATYPE_PROPERTY: "owl:DatatypeProperty",
  ANNOTATION_PROPERTY: "owl:AnnotationProperty",
  NAMED_INDIVIDUAL: "owl:NamedIndividual",
  DATATYPE: "rdfs:Datatype",
  ONTOLOGY: "owl:Ontology",
} as const;

/**
 * Map from :ID: suffix to RDF type string.
 * Used by parseHierarchy to determine rdfType from ancestor context.
 */
export const ID_SUFFIX_TO_TYPE: Record<string, string> = {
  "-datatypes": ENTITY_TYPES.DATATYPE,
  "-class-hierarchy": ENTITY_TYPES.CLASS,
  "-object-property-hierarchy": ENTITY_TYPES.OBJECT_PROPERTY,
  "-data-property-hierarchy": ENTITY_TYPES.DATATYPE_PROPERTY,
  "-annotation-property-hierarchy": ENTITY_TYPES.ANNOTATION_PROPERTY,
  "-individuals": ENTITY_TYPES.NAMED_INDIVIDUAL,
};

/**
 * Map from rdfType to OMN frame keyword.
 * Used by omnFrame to emit the correct frame type.
 */
export const RDF_TYPE_TO_OMN_KEYWORD: Record<string, string> = {
  [ENTITY_TYPES.CLASS]: "Class",
  [ENTITY_TYPES.OBJECT_PROPERTY]: "ObjectProperty",
  [ENTITY_TYPES.DATATYPE_PROPERTY]: "DataProperty",
  [ENTITY_TYPES.ANNOTATION_PROPERTY]: "AnnotationProperty",
  [ENTITY_TYPES.NAMED_INDIVIDUAL]: "Individual",
  [ENTITY_TYPES.ONTOLOGY]: "Ontology",
  [ENTITY_TYPES.DATATYPE]: "Datatype",
};
