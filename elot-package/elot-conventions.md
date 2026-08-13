# ELOT authoring conventions

A cheat sheet for authors -- human or LLM -- of ELOT `.org` ontology
files.  Exposed verbatim to LLMs through the `elot_conventions` gptel
tool.

ELOT is a *literate* ontology authoring format: an Org-mode document
*is* the ontology source.  Headings declare resources; description
lists carry axioms and annotations; the outline encodes the taxonomy.
Consult this once when these conventions are not already available in the
current long-running session.

## 1. The cardinal rule: heading nesting carries `SubClassOf`

Nesting under a `:resourcedefs: yes` section *is* the taxonomy; the same
holds for `SubPropertyOf` in property sections.

```org
*** Animal (ex:animal)
**** Dog (ex:dog)             ; ex:dog SubClassOf ex:animal
***** Puppy (ex:puppy)        ; ex:puppy SubClassOf ex:dog
```

A description-list `SubClassOf ::` row is correct **only** for

- anonymous class expressions -- `- SubClassOf :: ex:hasPart some ex:Wheel`;
- *additional* named parents (multiple inheritance): nest under one
  parent, declare the rest as rows.  The
  `elot/subclass-in-description-list` warning may fire; keep the row.

Never duplicate the outline with a row, and never declare one resource
with two headings.

## 2. Heading shape: `Label (curie)`

```
*** Dog (ex:dog)
```

Text before the parens becomes `rdfs:label`; the CURIE is the
identifier, using a prefix declared in the prefix table.

## 3. Description lists carry annotations and axioms

`- key :: value` rows under a resource heading.  Nested description
lists express meta-annotations (axiom annotations).

| Key | Kind | Purpose |
|-----|------|---------|
| `rdfs:label` | annotation | Alternative / language-tagged labels. |
| `rdfs:comment` | annotation | Free text. |
| `skos:definition` | annotation | Formal definition. |
| `rdfs:isDefinedBy` | annotation | Origin pointer for reused terms (§4). |
| `Domain ::` / `Range ::` | OMN | Property domain / range. |
| `Characteristics ::` | OMN | `Functional`, `Transitive`, `Symmetric`, ... |
| `InverseOf ::` | OMN | Inverse property. |
| `DisjointWith ::` | OMN | Class disjointness (symmetric -- state once). |
| `EquivalentTo ::` | OMN | Class / property equivalence. |
| `Types ::` | OMN | Individual class membership. |
| `Facts ::` | OMN | Individual property assertions. |
| `SubClassOf ::` | OMN | Anonymous expressions / extra parents -- see §1. |

## 4. Reusing terms from another ontology

Declare the term as a normal heading with an `rdfs:isDefinedBy` row
pointing at the origin; the prefix must exist in the prefix table.
`elot_db_borrow_term` emits exactly this shape, ready to be re-levelled
under a `:resourcedefs: yes` heading.

### 4.1 Do not re-axiomatise imported resources

Advisory, not lint-enforced.  Before adding `Domain`, `Range`,
`SubClassOf`, `Characteristics`, `EquivalentTo`, ... to a term that
carries `rdfs:isDefinedBy`:

- **Source imported** (its axioms reach your reasoner via `owl:imports`):
  add **no** local logical axioms -- redefining an upstream term
  silently strengthens or contradicts it, and only the reasoner will
  notice.  Annotations are fine.
- **Source not imported** (identifier reuse only): local constraints are
  normally needed, and should mirror the source's own axioms as closely
  as possible.

Either way, run `elot_db_get_attributes id=<curie> source=<file>` first
-- the cheap pre-flight against accidental over-constraint.

## 5. The `:nodeclare:` tag

A heading tagged `:nodeclare:` declares no OWL entity: it is a narrative
divider between resource declarations.  Heading nesting skips it, so
children still attach to the nearest declaring ancestor.

## 6. Default-prefix mechanics

`:ELOT-default-prefix:` on the ontology heading names the prefix for
unprefixed CURIEs (`:Dog` -> `ex:Dog`).  That prefix still needs its own
row in the prefix table.

## 7. The default authoring loop

Ordering matters more than tool choice:

```
[elot_conventions once per session, if needed]
  -> elot_resources / elot_read_resource      (orient)
  -> elot_borrow_term / elot_db_borrow_term   (reuse before minting)
  -> elot_declare_resource | elot_insert_*    (declare)
  -> elot_axiom_check -> elot_edit_axioms     (axioms; dry_run first)
  -> elot_check                               (lint + parse + reason)
```

`elot_lint` catches most structural mistakes before ROBOT is invoked;
`elot_explain`, `elot_sparql_select`, `elot_diff`, `elot_metrics` are
the diagnostic follow-ups.

## 8. Worked exemplar

A self-contained ontology demonstrating every idiom above.  Note the
file skeleton it exhibits: an ontology heading with `:ELOT-*:`
properties, a `:prefixdefs: yes` prefix table, and the six standard
`:resourcedefs: yes` sections -- each with an `:ID:`, and preferably all
present even when empty.  Section ordering is conventional, not
enforced.

````org
#+title: Pets -- a minimal worked exemplar for ELOT idioms
#+author: ELOT
#+date: 2026

* pets
:PROPERTIES:
:ID: pets
:ELOT-context-type: ontology
:ELOT-context-localname: pets
:ELOT-id-scheme: slug
:ELOT-default-prefix: ex
:header-args:omn: :tangle ./pets.omn :noweb yes
:END:

** Prefixes
:PROPERTIES:
:prefixdefs: yes
:END:
#+name: prefix-table
| prefix   | uri                                       |
|----------+-------------------------------------------|
| owl:     | http://www.w3.org/2002/07/owl#            |
| rdf:     | http://www.w3.org/1999/02/22-rdf-syntax-ns# |
| rdfs:    | http://www.w3.org/2000/01/rdf-schema#     |
| xsd:     | http://www.w3.org/2001/XMLSchema#         |
| skos:    | http://www.w3.org/2004/02/skos/core#      |
| dcterms: | http://purl.org/dc/terms/                 |
| foaf:    | http://xmlns.com/foaf/0.1/                |
| ex:      | http://example.org/pets/                  |
| ont:     | http://example.org/ont/                   |

** pets ontology (ont:pets ont:pets/0.1)
:PROPERTIES:
:ID: pets-ontology-declaration
:resourcedefs: yes
:END:
 - dcterms:title :: "Pets ontology (worked exemplar)"@en
 - owl:versionInfo :: 0.1

** Datatypes
:PROPERTIES:
:resourcedefs: yes
:ID: pets-datatypes
:END:

** Classes
:PROPERTIES:
:resourcedefs: yes
:ID: pets-class-hierarchy
:END:
*** Living things                                              :nodeclare:
A narrative divider -- no OWL entity is produced, and the classes below
still attach to nothing above them.  Appears in HTML output.
**** Animal (ex:animal)
 - skos:definition :: "A living organism with sensory perception."@en
 - DisjointWith :: ex:plant
***** Dog (ex:dog)
# Nesting alone makes ex:dog SubClassOf ex:animal -- no row needed.
 - skos:definition :: "A domesticated carnivorous mammal."@en
 - skos:example :: "Fido, Rex"
****** Puppy (ex:puppy)
# The legitimate description-list SubClassOf: an anonymous expression.
 - skos:definition :: "A young dog."@en
 - SubClassOf :: ex:hasAge some xsd:integer[< 2]
***** Cat (ex:cat)
 - skos:definition :: "A small domesticated carnivorous mammal."@en
 - DisjointWith :: ex:dog
**** Plant (ex:plant)
 - skos:definition :: "A photosynthetic organism."@en
*** Person (foaf:Person)
# Reuse: a normal heading plus an origin pointer, not a redeclaration.
 - rdfs:isDefinedBy :: <http://xmlns.com/foaf/0.1/>

** Object properties
:PROPERTIES:
:resourcedefs: yes
:ID: pets-object-property-hierarchy
:END:
*** hasOwner (ex:hasOwner)
 - Domain :: ex:animal
 - Range :: foaf:Person
 - Characteristics :: Functional
 - InverseOf :: ex:owns
*** owns (ex:owns)
 - Domain :: foaf:Person
 - Range :: ex:animal

** Data properties
:PROPERTIES:
:resourcedefs: yes
:ID: pets-data-property-hierarchy
:END:
*** hasAge (ex:hasAge)
 - Domain :: ex:animal
 - Range :: xsd:integer

** Annotation properties
:PROPERTIES:
:resourcedefs: yes
:ID: pets-annotation-property-hierarchy
:END:
*** dcterms:title
 - rdfs:isDefinedBy :: <http://purl.org/dc/terms/>
*** skos:definition
 - rdfs:isDefinedBy :: <http://www.w3.org/2004/02/skos/core>
*** skos:example
 - rdfs:isDefinedBy :: <http://www.w3.org/2004/02/skos/core>

** Individuals
:PROPERTIES:
:resourcedefs: yes
:ID: pets-individuals
:END:
*** Fido (ex:fido)
 - Types :: ex:dog
 - Facts :: ex:hasOwner ex:alice
 - Facts :: ex:hasAge 4
*** Alice (ex:alice)
 - Types :: foaf:Person
 - Facts :: ex:owns ex:fido
````
