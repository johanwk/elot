# ELOT authoring conventions

A concise cheat sheet for authors -- human or LLM -- of ELOT `.org`
ontology files.  Exposed verbatim to LLMs through the
`elot_conventions` gptel tool.

ELOT is a *literate* ontology authoring format: an Org-mode document
*is* the ontology source.  Headings declare resources; description
lists carry their axioms and annotations; the outline structure
encodes the taxonomy.  Read the rules below before composing any
edit.

## 1. The cardinal rule: heading nesting carries `SubClassOf`

Heading nesting under a `:resourcedefs: yes` section is the *primary*
way to express the class taxonomy.  A class declared under another
class heading is automatically a `SubClassOf` of its parent.  Likewise
for property hierarchies (`SubPropertyOf`).

```org
*** Animal (ex:animal)
**** Dog (ex:dog)             ; ex:dog SubClassOf ex:animal
***** Puppy (ex:puppy)        ; ex:puppy SubClassOf ex:dog
**** Cat (ex:cat)             ; ex:cat SubClassOf ex:animal
```

Do **not** express `SubClassOf` to a named class via a description
list (`- SubClassOf :: ex:animal`).  ROBOT accepts it, but it
duplicates information the outline already carries and breaks tools
that rely on the outline (lint, navigation, future
`elot/subclass-in-description-list` checker).

Description-list `SubClassOf ::` is appropriate *only* for multiple
inheritance, or anonymous class expressions (`some`, `only`, `and`,
`or`, restrictions), e.g. `- SubClassOf :: ex:hasPart some ex:Wheel`.

**Multiple inheritance.** When a class or property has more than one
*named* superclass/superproperty, nest it under one parent in the
outline and declare the additional parent(s) with description-list
`SubClassOf ::` / `SubPropertyOf ::` rows. Do **not** create a
second heading for the same resource to avoid the description-list
form -- a resource must be declared by exactly one heading. The
`elot/subclass-in-description-list` lint warning may fire in this
case; keep the description-list row anyway, it is the correct
spelling for multi-parent resources.

## 2. Heading shape: `Label (curie)`

Every resource heading reads:

```
*** Dog (ex:dog)
```

The text before the parens becomes `rdfs:label`; the CURIE inside
the parens is the identifier.  Use the project's declared prefixes
(see Prefixes section).

## 3. Description-list entries carry annotations and axioms

Under a resource heading, a description list (`- key :: value`) holds
both annotations and OMN axioms.  

Nested description lists for meta-annotations are permitted.

Common annotation keys:

| Key                       | Purpose                                                            |
|---------------------------|--------------------------------------------------------------------|
| `rdfs:label`              | Alternative or language-tagged labels (heading already gives one). |
| `rdfs:comment`            | Free-text comment.                                                 |
| `skos:definition`         | Formal definition.                                                 |
| `rdfs:isDefinedBy`        | Origin pointer for *reused* terms (see section 5).                 |

Common keys for restrictions in OWL Manchester syntax:

| `Domain ::`               | Object/data property domain (Manchester axiom).                    |
| `Range ::`                | Property range.                                                    |
| `Characteristics ::`      | `Functional`, `Transitive`, `Symmetric`, ...                       |
| `InverseOf ::`            | Inverse property.                                                  |
| `DisjointWith ::`         | Class disjointness.                                                |
| `EquivalentTo ::`         | Class/property equivalence.                                        |
| `Types ::`                | For individuals: class membership (incl. anonymous expressions).   |
| `Facts ::`                | For individuals: property assertions.                              |
| `SubClassOf ::`           | Anonymous class expressions only -- see section 1.                 |

## 4. Ontology file structure

A typical ELOT ontology document has this skeleton:

```org
* my-ontology
:PROPERTIES:
:ID: my-ontology
:ELOT-context-type: ontology
:ELOT-context-localname: my-ontology
:ELOT-default-prefix: ex
:header-args:omn: :tangle ./my-ontology.omn :noweb yes
:END:

** Prefixes
:PROPERTIES:
:prefixdefs: yes
:END:
#+name: prefix-table
| prefix | uri                            |
|--------+--------------------------------|
| ex:    | http://example.org/resource/   |
| ont:   | http://example.org/ont/        |
| owl:   | http://www.w3.org/2002/07/owl# |
| ...    | ...                            |

** my-ontology ontology (ont:my-ontology ont:my-ontology/0.1)
:PROPERTIES:
:ID: my-ontology-ontology-declaration
:resourcedefs: yes
:END:
 - dcterms:title :: "My ontology"@en
 - owl:versionInfo :: 0.1

** Datatypes
:PROPERTIES:
:ID: my-ontology-datatypes
:resourcedefs: yes
:END:

** Classes
:PROPERTIES:
:ID: my-ontology-class-hierarchy
:resourcedefs: yes
:END:
*** Animal (ex:animal)
**** Dog (ex:dog)
...

** Object properties
:PROPERTIES:
:ID: my-ontology-object-property-hierarchy
:resourcedefs: yes
:END:
*** hasOwner (ex:hasOwner)
 - Domain :: ex:dog        << NOTE. declared in a heading under Classes
 - Range :: ex:person

** Data properties
:PROPERTIES:
:ID: my-ontology-data-property-hierarchy
:resourcedefs: yes
:END:

** Annotation properties
:PROPERTIES:
:ID: my-ontology-annotation-property-hierarchy
:resourcedefs: yes
:END:

** Individuals
:PROPERTIES:
:ID: my-ontology-individuals
:resourcedefs: yes
:END:
```

The `:resourcedefs: yes` drawer marks a section as a resource-
declaration section; the `:prefixdefs: yes` drawer marks the prefix
table.  Section ordering is conventional, not enforced.  Empty
sections (e.g. `Datatypes` with no children) are still preferred by
the standard structural lint.

## 5. Reusing terms from another ontology

To use a term from a different ontology while citing its origin,
declare it as a normal heading and add an `rdfs:isDefinedBy` row:

```org
*** Vehicle (transport:Vehicle)
 - rdfs:isDefinedBy :: <https://example.org/vocab/transport>
 - skos:definition :: "A means of carrying or transporting ..."
```

The CURIE prefix (`transport:` above) must be declared in the
project's prefix table.  After this declaration, the term is locally
known to lint, ROBOT, and the LLM tools, and the `isDefinedBy` entry
gives downstream consumers the attribution.

The `elot_db_borrow_term` tool emits exactly this shape, ready to be
re-leveled and pasted under a `:resourcedefs: yes` heading.

## 6. The `:nodeclare:` tag is for informative purposes

A heading tagged `:nodeclare:` is **suppressed from declaration**.
It is meant for *narrative* sub-headings interleaved between resource
declarations -- a section header that explains the following block of
classes, say, without itself becoming an OWL entity.

```org
*** Living things                                              :nodeclare:
The classes below cover the biological side of the domain.

**** Animal (ex:animal)
**** Plant (ex:plant)
```

## 7. Default-prefix mechanics

The `:ELOT-default-prefix:` property on the top-level ontology
heading names the prefix used for unprefixed CURIEs (e.g. `:Dog`
expands to `ex:Dog` when the default prefix is `ex`).  The same
prefix should still have an explicit row (`ex:`) in the prefix table.

## 8. Tooling cheat sheet

| Task                                       | Tool                                                      |
|--------------------------------------------|-----------------------------------------------------------|
| Find an existing term across all DBs       | `elot_db_search_label`                                    |
| Produce a borrow snippet for reuse         | `elot_db_borrow_term`                                     |
| Lint the file                              | `elot_lint`                                               |
| Parse + DL-profile check the OMN export    | `elot_omn_validate` (with `profile=DL`)                   |
| SPARQL query (read-only)                   | `elot_sparql_select`                                      |
| Consistency / unsatisfiability             | `elot_consistency`, `elot_unsatisfiable`                  |
| Explain an entailment                      | `elot_explain`                                            |
| Structural diff vs. a baseline snapshot    | `elot_diff`                                               |

When in doubt, run `elot_lint` first -- it catches the bulk of
structural mistakes (missing required sections, undeclared CURIEs in
axioms, malformed prefix table) before ROBOT ever sees the file.

## 9. Worked exemplar

The following is a single self-contained ELOT ontology that
demonstrates every idiom covered above.  Inline comments
(`;; ...` in the heading line, or narrative paragraphs between
sections) point at the convention being illustrated.  Paste it
into a scratch `.org` file if you want to experiment.

````org
#+title: Pets -- a minimal worked exemplar for ELOT idioms
#+author: ELOT
#+date: 2026

* pets
:PROPERTIES:
:ID: pets
:ELOT-context-type: ontology
:ELOT-context-localname: pets
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
 - rdfs:comment :: "Demonstrates ELOT authoring idioms."@en

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
The heading above is tagged :nodeclare: -- it is a *narrative*
divider, not a class declaration.  No OWL entity is produced for
"Living things". This heading and narrative will appear in HTML output.
**** Animal (ex:animal)
 - skos:definition :: "A living organism with sensory perception."@en
 - DisjointWith :: ex:plant
***** Dog (ex:dog)
# Heading nesting carries `SubClassOf`: `ex:dog` is a subclass of
# `ex:animal` by virtue of being nested under it.  No description-list
# `SubClassOf ::` entry is needed (and would duplicate information).
 - skos:definition :: "A domesticated carnivorous mammal."@en
 - skos:example :: "Fido, Rex"
****** Puppy (ex:puppy)
 - skos:definition :: "A young dog."@en
 - SubClassOf :: ex:hasAge some xsd:integer[< 2]
***** Cat (ex:cat)
# DisjointWith is symmetric; stating it once is enough -- no need to
# repeat `DisjointWith :: ex:cat` on the `ex:dog` heading.
 - skos:definition :: "A small domesticated carnivorous mammal."@en
 - DisjointWith :: ex:dog
**** Plant (ex:plant)
 - skos:definition :: "A photosynthetic organism."@en
*** Person (foaf:Person)
# Reusing a term from another ontology: declare it as a normal heading
# and add an `rdfs:isDefinedBy` row pointing at the origin.  Local code
# can refer to `foaf:Person` thereafter.
 - rdfs:isDefinedBy :: <http://xmlns.com/foaf/0.1/>
 - skos:definition :: "A human being."@en

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

Things to notice in the exemplar above:

- Each `resourcedefs` section has `:ID:` and `:resourcedefs: yes` drawers.
- The class taxonomy (`ex:animal` > `ex:dog` > `ex:puppy`) is carried
  entirely by heading nesting; no `SubClassOf ::` rows duplicate it.
- The one `SubClassOf ::` row on `ex:puppy` carries an *anonymous*
  class expression (`ex:hasAge some xsd:integer[< 2]`) -- the legitimate
  use of the description-list form.
- `foaf:Person` is reused with `rdfs:isDefinedBy`, not redeclared from
  scratch.
- The "Living things" heading is `:nodeclare:` because it's a narrative
  divider, not a class.  The two animal-subclass declarations under it
  still attach to `ex:animal` correctly because heading nesting skips
  `:nodeclare:` headings.
- The `Datatypes` section is empty but still declared -- the structural
  lint prefers all six standard sections (`Datatypes`, `Classes`,
  `Object properties`, `Data properties`, `Annotation properties`,
  `Individuals`) present even when one of them carries no children.
