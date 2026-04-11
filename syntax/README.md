# syntax/ — OWL Manchester Syntax Checker

This directory implements a **syntax checker** for
[OWL Manchester Syntax](https://www.w3.org/TR/owl2-manchester-syntax/) (OMN)
expressions, as used by the [Elot](../README.md) tool for authoring OWL
ontologies in Org-mode files.

## Purpose

When editing an Elot Org file, axiom values (e.g. `SubClassOf`, `Domain`,
`Range`, `SubPropertyChain`, etc.) are written as OWL Manchester Syntax
fragments.  This module parses those fragments to catch syntax errors early,
before the file is exported or processed by a reasoner.

## Grammar coverage

The grammar covers the following OMN constructs, based on sections
[2.3 (Property and Datatype Expressions)](https://www.w3.org/TR/owl2-manchester-syntax/#Property_and_Datatype_Expressions)
and [2.4 (Descriptions)](https://www.w3.org/TR/owl2-manchester-syntax/#Descriptions)
of the W3C specification:

| Entry point            | Used for axioms                                      |
|------------------------|------------------------------------------------------|
| `class-expression`     | SubClassOf, EquivalentTo, DisjointWith, Domain, Range, Types |
| `object-property-expression` | InverseOf, SubPropertyOf                       |
| `sub-property-chain`   | SubPropertyChain (two or more properties joined by `o`) |
| `data-range`           | Data ranges with faceted restrictions, conjunctions, disjunctions |
| `fact`                 | Facts (property assertions on individuals)            |
| `individual-iri-list`  | SameAs, DifferentFrom (comma-separated individuals)   |

### What is parsed

- **Class expressions**: conjunction (`and`), disjunction (`or`), negation (`not`),
  parenthesized sub-expressions, `Thing`, `Nothing`
- **Restrictions**: `some`, `only`, `value`, `min`, `max`, `exactly`, `Self`
- **Object property expressions**: prefixed IRIs, full IRIs (`<...>`), `inverse`
- **Enumerations**: `{ individual , ... }` (oneOf)
- **Literals**: `"string"`, `"string"^^datatype`, `"string"@lang`
- **Facts**: `prop individual`, `prop "literal"`, `not prop individual`
- **Individual lists**: `ind1 , ind2 , ...` (for SameAs/DifferentFrom)
- **Blank nodes**: `_:name` (in facts and individual lists)
- **Data ranges**: datatype IRIs, `{ literal , ... }` (oneOf), faceted restrictions
  (e.g. `xsd:integer[>= "0"^^xsd:integer, <= "100"^^xsd:integer]`), data
  `and`/`or`/`not`
- **SubPropertyChain**: `prop1 o prop2 o prop3`

### Design decision: merged object/data property restrictions

OWL distinguishes between *object property* restrictions (with class-expression
fillers) and *data property* restrictions (with data-range fillers).  However,
since `objectPropertyIRI` and `dataPropertyIRI` are **syntactically identical**
— both are just IRIs — they cannot be distinguished by a parser without access
to the ontology declarations.

Our grammar therefore **merges** both into a single set of restriction
alternatives.  PEG ordered choice handles ambiguity:
- `value` tries `Literal` first, then `IndividualIRI`
- Cardinality fillers use `PrimaryExpression`, covering both class and datatype IRIs

This is pragmatic and correct for syntax validation.  Semantic validation
(checking whether a property is declared as object vs. data) would require
ontology-level analysis, which is out of scope here.

### What is NOT parsed

- **Annotation** syntax (with datatypes, nested annotations, etc.)
- **Ontology-declaring frames** (`Ontology:`, `ObjectProperty:`, `Class:`, etc.)
- **Semantic validation** (e.g. whether an IRI refers to a class vs. a property)

## Files

| File                    | Description                                              |
|-------------------------|----------------------------------------------------------|
| `owl-manchester.peggy`  | Reference grammar in [Peggy](https://peggyjs.org/) format |
| `elot-owl-grammar.el`   | Hand-maintained [peg.el](https://www.gnu.org/software/emacs/manual/html_node/elisp/PEG.html) grammar (the executable one) |
| `peggy-to-peg.el`       | Converter from Peggy → peg.el (handles a useful subset)  |
| `test-grammar.el`       | Batch-mode test script                                   |
| `README.md`             | This file                                                |

### Relationship between `.peggy` and `.el` grammars

The `owl-manchester.peggy` file serves as a **reference specification** in a
widely-used PEG notation.  The `elot-owl-grammar.el` file is the **executable
grammar** used by Emacs' `peg.el` library.

The Peggy→peg.el converter (`peggy-to-peg.el`) can translate most of the
grammar automatically, but a few constructs are hand-maintained:

- **BareName keyword exclusion** — `!KeywordToken` negative lookahead is
  manually translated into `(not keyword-boundary)` with an explicit keyword
  list in the `.el` file.
- **`"o"` token boundary** in SubPropertyChain — uses `(not name-char)`
  lookahead to avoid matching `"o"` as a prefix of longer identifiers.
- **Facet keywords** (`length`, `minLength`, etc.) — added to the keyword
  boundary list to prevent them from being parsed as bare-name IRIs.

## Running the tests

```sh
cd syntax && emacs --batch -l test-grammar.el
```

This runs all positive and negative test cases for:
- Class expressions (46 tests)
- SubPropertyChain expressions (9 tests)
- Data range expressions (16 tests)
- Fact expressions (17 tests)
- Individual IRI lists (11 tests)

## Usage in Elot

The grammar provides three main entry-point functions (defined in
`test-grammar.el` and intended for integration into `elot-mode`):

```elisp
(elot-parse-class-expression "ex:A and ex:B")          ;; → t
(elot-parse-property-expression "inverse ex:partOf")    ;; → t
(elot-parse-sub-property-chain "ex:p1 o ex:p2")         ;; → t
(elot-parse-data-range "xsd:integer [>= \"0\"^^xsd:integer]") ;; → t
(elot-parse-fact "hasWife Mary")                        ;; → t
(elot-parse-fact "not hasChild Susan")                  ;; → t
(elot-parse-fact "hasAge \"33\"^^xsd:integer")           ;; → t
(elot-parse-individual-iri-list "ex:John , ex:Mary")    ;; → t
```

Each function returns `t` if the input is a valid OMN expression, `nil`
otherwise.

## Integration with `elot-lint`

The grammar is used by `elot-lint.el` to validate axiom values as you
edit.  When you run `M-x elot-org-lint` (or use the ELOT menu →
"Check for common problems"), description list entries whose tags are
OMN keywords are automatically parsed against the grammar and flagged
if invalid.

The mapping from keyword to parser entry point is defined by
`elot-omn-keyword-parser-alist` in `elot-lint.el`:

| OMN keyword       | Parser function                      |
|--------------------|--------------------------------------|
| SubClassOf         | `elot-parse-class-expression`        |
| EquivalentTo       | `elot-parse-class-expression`        |
| DisjointWith       | `elot-parse-class-expression`        |
| DisjointUnionOf    | `elot-parse-class-expression`        |
| Domain             | `elot-parse-class-expression`        |
| Range              | `elot-parse-class-expression`        |
| Types              | `elot-parse-class-expression`        |
| InverseOf          | `elot-parse-property-expression`     |
| SubPropertyOf      | `elot-parse-property-expression`     |
| SubPropertyChain   | `elot-parse-sub-property-chain`      |
| Facts              | `elot-parse-fact`                    |
| SameAs             | `elot-parse-individual-iri-list`     |
| DifferentFrom      | `elot-parse-individual-iri-list`     |

## Planned: TypeScript version

A TypeScript implementation of the same grammar will be added later, for use
with the [`tools/elot-cli/`](../tools/elot-cli/) VS Code extension / CLI tool.
The `owl-manchester.peggy` file is written in
[Peggy](https://peggyjs.org/) format specifically to facilitate this — Peggy is
a JavaScript/TypeScript PEG parser generator, so the `.peggy` file can be used
directly (with minor adaptations for the keyword boundary handling) to generate
a TypeScript parser.
