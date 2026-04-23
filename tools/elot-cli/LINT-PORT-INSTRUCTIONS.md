# Porting elot-lint.el to VS Code TypeScript — Staged LLM Instructions

## Context

The Emacs package `elot-package/elot-lint.el` implements 8 ELOT-specific lint
checkers that validate OWL ontology files authored in Org-mode. The VS Code
extension `tools/elot-cli/` already has:

- **Org parser**: `parseOrgWasm.ts` — parses Org text into an `ElotNode` tree
  via a Rust/WASM crate, enriched with `rdfType`, `uri`, `label`, etc.
- **OMN syntax checker**: `omnSyntaxCheck.ts` — validates Manchester Syntax
  axiom values using a Peggy parser (shared grammar with Elisp).
- **Diagnostics provider**: `diagnosticsProvider.ts` — the VS Code plumbing
  that collects errors and maps them to squiggly-underline `Diagnostic` objects.
  Currently only reports OMN syntax errors (one checker).
- **Slurp map**: `buildSlurp.ts` — walks the `ElotNode` tree and builds
  `Map<string, SlurpEntry>` for CURIE→label+type+properties lookup.
- **OMN keywords**: `omnKeywords.ts` — `propertyKeywords`, `miscKeywords`,
  `allKeywords` with `isPropertyKeyword()`, `isMiscKeyword()`, `isOmnKeyword()`.
- **Types**: `types.ts` — `ElotNode`, `DescriptionItem`, `ID_SUFFIX_TO_TYPE`,
  `ENTITY_TYPES`, `RDF_TYPE_TO_OMN_KEYWORD`.
- **Entity extraction**: `entityFromHeader.ts` — extracts CURIE/URI + label
  from heading titles.
- **Prefix handling**: `prefixes.ts` — builds prefix→URI map.

The goal is to port **all 8 Elisp lint checkers** into the VS Code extension so
the same errors/warnings appear as diagnostics (squiggly underlines) in VS Code.

---

## Elisp Checker Inventory (from `elot-lint.el`)

| # | Elisp checker function | What it does | Severity |
|---|---|---|---|
| 1 | `elot-check-ontology-presence` | Ensure ≥1 top-level heading has `:ELOT-context-type: ontology` | ERROR |
| 2 | `elot-check-ontology-header` | Top-level ontology heading has `:ID:`, matching `:ELOT-context-localname:`, valid `:header-args:omn:`, prefix in abbrev list | ERROR/WARNING |
| 3 | `elot-check-prefix-table` | Prefix table exists and is non-empty under ontology heading | ERROR |
| 4 | `elot-check-required-sections` | All 7 standard section IDs exist (e.g. `<localname>-class-hierarchy`) with `:resourcedefs: yes` | WARNING |
| 5 | `elot-check-nodeclare-id-prefix-label` | Each heading under `:resourcedefs:` has a valid CURIE/URI with known prefix and proper label, or is tagged `:nodeclare:` | ERROR/WARNING |
| 6 | `elot-check-description-list-curies` | CURIE-shaped description list tags are declared annotation properties | WARNING |
| 7 | `elot-check-axiom-value-curies` | CURIEs in axiom values are declared, not annotation properties in non-annotation sections; balanced parens | WARNING |
| 8 | `elot-check-omn-keyword-appropriateness` | OMN keywords are valid for their section type (e.g. `Domain` not under Classes) | ERROR |

Plus the **already-ported** OMN syntax checker (equivalent to
`elot-check-omn-syntax` in Elisp), which is in `omnSyntaxCheck.ts` +
`diagnosticsProvider.ts`.

---

## Scoping Rules (apply to all checkers)

The Elisp code uses two guard predicates:

1. **`elot--inside-ontology-context-p`**: only check content under a heading
   with `:ELOT-context-type: ontology`. Sibling outlines (e.g., "Diagrams")
   are ignored.
2. **`elot--inside-resourcedefs-p`**: only check content under headings
   inheriting `:resourcedefs: yes`.

In the `ElotNode` tree, the equivalent is:
- A node is "inside ontology context" if any ancestor (walking up to root) has
  `elotContextType === "ontology"`.
- A node is "inside resourcedefs" if any ancestor (or itself) has
  `resourcedefs === true`.

These scoping checks must be built as **tree-walking helpers** in Stage 1 before
any checker can use them.

---

## Stage 1: Scoping helpers and diagnostic infrastructure

### Files to create/modify

- **Create `src/elotLintHelpers.ts`** — shared helper functions for all lint
  checkers.

### What to implement

1. **`isInsideOntologyContext(node, ancestors)`** — given a node and its
   ancestor chain, return `true` if any ancestor has
   `elotContextType === "ontology"`.

2. **`isInsideResourcedefs(node, ancestors)`** — return `true` if any ancestor
   (or the node itself) has `resourcedefs === true`.

3. **`isInsideElotScope(node, ancestors)`** — combined guard: both of the above
   must be true.

4. **`getSectionSuffix(node, ancestors)`** — walk ancestors to find the nearest
   node with a well-known `:ID:` suffix (from `ID_SUFFIX_TO_TYPE` keys). Return
   the suffix string (e.g. `"-class-hierarchy"`) or `null`.

5. **`findOntologyRoots(root)`** — return all level-1 children of root that
   have `elotContextType === "ontology"`.

6. **Define a `LintDiagnostic` interface** — `{ node: ElotNode; line?: number;
   message: string; severity: "error" | "warning" }`.

7. **`walkResourceNodes(root, callback)`** — iterative tree walker that calls
   `callback(node, ancestors)` for every node that passes `isInsideElotScope`.
   The `ancestors` array is maintained as a stack during traversal. This is the
   standard iteration pattern used by all per-node checkers.

### Testing

- Write `src/tests/elotLintHelpers.test.ts` with a small hand-crafted
  `ElotNode` tree that has:
  - An ontology heading with `elotContextType: "ontology"` and children with
    `resourcedefs: true`
  - A sibling non-ontology heading (like "Diagrams") with children
  - Verify `isInsideElotScope` returns true only for nodes under the ontology +
    resourcedefs path.

### Important notes

- Do NOT modify `diagnosticsProvider.ts` yet — that comes in Stage 7.
- Do NOT import `vscode` in `elotLintHelpers.ts` — keep it pure TypeScript so
  it's testable without VS Code.

---

## Stage 2: Structural checkers (ontology presence + header + prefix table)

### Files to create

- **Create `src/lintStructural.ts`** — checkers 1–3.

### What to implement

Implement three functions, each returning `LintDiagnostic[]`:

1. **`checkOntologyPresence(root)`** (checker #1)
   - Walk level-1 children of `root`.
   - If none has `elotContextType === "ontology"`, return one ERROR diagnostic.

2. **`checkOntologyHeader(root)`** (checker #2)
   - For each ontology root (level-1, `elotContextType === "ontology"`):
     - ERROR if `id` is missing or empty.
     - WARNING if `id !== elotContextLocalname`.
     - ERROR if `tangleTargetOmn` is missing or doesn't end in `.omn`.
     - WARNING if `elotDefaultPrefix` is not found in the prefix table
       (`node.prefixes` or descendant with `prefixdefs`).
   - Mirror the checks from `elot-check-ontology-header` and
     `elot--check-omn-args`.

3. **`checkPrefixTable(root)`** (checker #3)
   - For each ontology root, find the descendant with `prefixdefs === true`.
   - ERROR if no such descendant exists, or if `prefixes` is empty.

### Key differences from Elisp

- In Elisp, `org-link-abbrev-alist-local` is the runtime prefix map. In TS, the
  equivalent is the `prefixes` array on the `prefixdefs` node, or
  `getPrefixMap(root)` from `parseOrgWasm.ts`.
- The `:header-args:omn:` check maps to the `tangleTargetOmn` field (already
  extracted by the parser).
- The `:noweb yes` check from Elisp cannot be directly ported because the WASM
  parser doesn't extract arbitrary header-args. For now, skip this sub-check
  and add a TODO comment.

### Testing

- Write `src/tests/lintStructural.test.ts`.
- Test with: (a) a valid ontology, (b) an Org file with no ontology heading,
  (c) an ontology heading missing `:ID:`, (d) missing prefix table.

---

## Stage 3: Required sections checker

### Files to create

- **Create `src/lintRequiredSections.ts`** — checker #4.

### What to implement

**`checkRequiredSections(root)`** returning `LintDiagnostic[]`:

- For each ontology root, get `elotContextLocalname` (e.g. `"pizza"`).
- Define the 7 required suffixes:
  ```
  -ontology-declaration, -datatypes, -class-hierarchy,
  -object-property-hierarchy, -data-property-hierarchy,
  -annotation-property-hierarchy, -individuals
  ```
- Walk children (level-2 headings) and collect their `:ID:` values.
- For each required ID (`localname + suffix`):
  - WARNING if missing entirely.
  - WARNING if present but `resourcedefs` is not `true`.

### Key differences from Elisp

- The Elisp version walks the full tree with `org-element-map`. In TS, we only
  need to check the direct children (or shallow descendants) of the ontology
  root, since sections are typically at level 2.
- Actually, walk the entire subtree under the ontology root to find matching
  IDs, since section headings may be at varying levels.

### Testing

- Write `src/tests/lintRequiredSections.test.ts`.
- Test with: (a) all sections present, (b) missing `-datatypes`, (c) a section
  present but without `resourcedefs: true`.

---

## Stage 4: Heading identity checker (ID + prefix + label)

### Files to create

- **Create `src/lintHeadingIdentity.ts`** — checker #5.

### What to implement

**`checkHeadingIdentity(root)`** returning `LintDiagnostic[]`:

- Use `walkResourceNodes(root, callback)` from Stage 1.
- For each node inside ELOT scope that is NOT itself a section root (i.e.,
  does not have `resourcedefs === true` directly):
  - Skip if tagged `:nodeclare:`.
  - ERROR if `uri` is `undefined` or `null` (no identifier found — heading
    should have a CURIE/URI or be tagged `:nodeclare:`).
  - ERROR if `uri` contains an unknown prefix — use `getPrefixMap(root)` to
    resolve the prefix part of the CURIE; if the prefix is not in the map,
    flag it.
  - WARNING if the label contains unbalanced quotes or doesn't match the
    `"text"@lang` pattern when it contains quotes.

### Key details

- The `entityFromHeader` logic is already applied during parsing
  (`parseOrgWasm.ts` → `mapRawNode`), so `node.uri` and `node.label` are
  already populated. We just need to validate them.
- Prefix validation: split the CURIE on `:` to get the prefix part, check it
  against the prefix map.
- The Elisp uses `elot-unprefix-uri` for prefix checking. In TS, implement a
  simple `isCurieKnown(curie, prefixMap)` helper in `elotLintHelpers.ts`.

### Testing

- Write `src/tests/lintHeadingIdentity.test.ts`.
- Test with: (a) valid headings, (b) heading with no CURIE, (c) heading with
  unknown prefix, (d) heading tagged `:nodeclare:` (should be skipped).

---

## Stage 5: Description list CURIE checker (annotation property validation)

### Files to create

- **Create `src/lintDescriptionCuries.ts`** — checker #6.

### What to implement

**`checkDescriptionListCuries(root)`** returning `LintDiagnostic[]`:

- Use `walkResourceNodes(root, callback)`.
- For each node, iterate over `node.descriptions`.
- For each description item where the `tag` looks like a CURIE
  (`/^[-_./\w]*:[-_./\w]*$/` — letters, digits, hyphens, underscores, dots,
  slashes around a colon):
  - Skip if it's an OMN keyword (`isOmnKeyword(tag)`) — those are axioms, not
    annotation properties.
  - Skip if it's in the **known annotation properties** list:
    `["rdfs:label", "rdfs:comment", "rdfs:seeAlso", "rdfs:isDefinedBy"]`.
  - Skip if the slurp map contains it and its `rdfType` is
    `"owl:AnnotationProperty"`.
  - Otherwise: WARNING — "Unknown or invalid annotation property: <tag>".

### Key details

- Build the slurp map once with `buildSlurp(root)` and pass it into the
  checker, or build it inside the checker.
- The CURIE regex should match the same pattern as the Elisp:
  `"\\`[-_./[:alnum:]]*:[-_/.[:alnum:]]*\\'"`.

### Testing

- Write `src/tests/lintDescriptionCuries.test.ts`.
- Test with: (a) `rdfs:comment` (should pass), (b) `skos:definition` declared
  as AnnotationProperty in the slurp (should pass), (c) `foo:unknown` not in
  slurp (should warn), (d) `SubClassOf` (OMN keyword, should be skipped).

---

## Stage 6: Axiom value CURIE checker + OMN keyword appropriateness

### Files to create

- **Create `src/lintAxiomValues.ts`** — checkers #7 and #8.

### What to implement

#### 6a. `checkAxiomValueCuries(root)` (checker #7)

- Use `walkResourceNodes(root, callback)`.
- For each node, iterate over `node.descriptions`.
- For each description item where `isOmnKeyword(tag)` is true (i.e., it's an
  axiom line like `SubClassOf`, `Domain`, etc.):
  - Extract all CURIE-like tokens from the `value` string (split on whitespace
    and commas, filter to CURIE pattern, exclude `http://` URLs).
  - For each CURIE token:
    - Skip if it's in a built-in list (the OWL/RDF reserved vocabulary — create
      a `BUILTIN_RESOURCES` set with items like `owl:Thing`, `owl:Nothing`,
      `owl:topObjectProperty`, `owl:bottomObjectProperty`, `xsd:string`,
      `xsd:integer`, `xsd:boolean`, `xsd:dateTime`, `rdfs:Literal`, etc.).
    - Look it up in the slurp map.
    - WARNING if not found: "Unknown CURIE in axiom: <curie>".
    - WARNING if found but its `rdfType` is `"owl:AnnotationProperty"` and
      the enclosing section is NOT an annotation-property section:
      "Annotation property used in axiom: <curie>".
  - Also check balanced parentheses in the value string:
    - WARNING if parentheses are unbalanced.

#### 6b. `checkOmnKeywordAppropriateness(root)` (checker #8)

- Use `walkResourceNodes(root, callback)`.
- For each node, iterate over `node.descriptions`.
- For each description item where the `tag` is an OMN keyword:
  - Skip misc keywords (use `isMiscKeyword(tag)`) — they're valid anywhere.
  - Determine the section type using `getSectionSuffix(node, ancestors)` from
    Stage 1.
  - Look up the allowed keywords for that section suffix in a constant map
    (port `elot-omn-keywords-by-section` from Elisp):

    ```typescript
    const KEYWORDS_BY_SECTION: Record<string, string[]> = {
      "-datatypes": ["EquivalentTo"],
      "-class-hierarchy": ["SubClassOf", "EquivalentTo", "DisjointWith",
                           "DisjointUnionOf", "HasKey"],
      "-object-property-hierarchy": ["SubPropertyOf", "EquivalentTo",
        "DisjointWith", "Domain", "Range", "Characteristics", "InverseOf",
        "SubPropertyChain"],
      "-data-property-hierarchy": ["SubPropertyOf", "EquivalentTo",
        "DisjointWith", "Domain", "Range", "Characteristics"],
      "-annotation-property-hierarchy": ["SubPropertyOf", "Domain", "Range"],
      "-individuals": ["Types", "Facts", "SameAs", "DifferentFrom"],
    };
    ```

  - ERROR if the keyword is not in the allowed list for the section:
    `"<keyword>" is not valid in <SectionName> section (allowed: X, Y, Z)`.

### Key details

- The "builtin resources" list in Elisp is `elot-owl-builtin-resources`. You
  need to define a similar constant in TS. The exact list can be extracted from
  the Elisp source or defined pragmatically as OWL2 reserved names.
- For the section-suffix detection, use `getSectionSuffix()` from Stage 1,
  which walks the ancestor chain and checks `:ID:` suffixes.

### Testing

- Write `src/tests/lintAxiomValues.test.ts`.
- Test `checkAxiomValueCuries` with: (a) valid CURIEs, (b) unknown CURIE,
  (c) annotation property in axiom position, (d) unbalanced parens.
- Test `checkOmnKeywordAppropriateness` with: (a) `SubClassOf` under classes
  (ok), (b) `Domain` under classes (error), (c) `DisjointClasses` anywhere
  (misc, ok).

---

## Stage 7: Wire all checkers into `diagnosticsProvider.ts`

### Files to modify

- **`src/diagnosticsProvider.ts`** — extend to call all lint checkers.

### What to implement

1. **Import** all checker functions from Stages 2–6.

2. **Create `collectAllLintErrors(root)`** — calls each checker function and
   merges all `LintDiagnostic[]` results into one array.

3. **Map `LintDiagnostic` to VS Code `Diagnostic`** — the current
   `mapToDiagnostics` uses line-by-line text matching to locate errors.
   For the new lint diagnostics, you need a strategy to map `ElotNode` back to
   line numbers:

   **Option A (recommended)**: Add a line-number lookup phase. After parsing
   with `parseOrg()`, scan the raw document text to build a map from
   heading-title → line number and description-tag → line number. Each
   `LintDiagnostic` should carry enough context (node title, description tag,
   value) to be located in the document.

   **Option B**: Extend `LintDiagnostic` to carry a search string, then use the
   existing `mapToDiagnostics` pattern of scanning lines.

   **Option C**: Extend the WASM parser to emit byte offsets per node (a larger
   change, save for later).

   For now, use **Option A** — build a line-number index and look up
   diagnostics. Add a helper `findNodeLine(doc, nodeTitle)` and
   `findDescriptionLine(doc, tag, value)`.

4. **Merge** the existing OMN syntax diagnostics with the new lint diagnostics
   into the same `DiagnosticCollection("elot-omn")`, or create a second
   collection `"elot-lint"` to keep them separable. Using a single collection
   is simpler.

5. **Severity mapping**: `"error"` → `DiagnosticSeverity.Error`, `"warning"` →
   `DiagnosticSeverity.Warning`.

6. **Set `diag.source = "elot"`** on all diagnostics so they show the source
   in the Problems panel.

### Key consideration

The current `diagnosticsProvider.ts` calls `parseOrg()` on every change (with
500ms debounce). All the new checkers will also operate on the same
`ElotNode` tree, so parsing only happens once. The slurp map should also be
built once per update cycle and passed to the checkers that need it.

### Testing

- At this stage, manual testing in VS Code is most effective.
- Create a test `.org` file with known errors (missing ontology, bad prefix,
  `Domain` under classes, unknown CURIE in axiom) and verify diagnostics appear.
- Write `src/tests/lintIntegration.test.ts` that constructs an `ElotNode` tree
  with known issues and verifies the combined `collectAllLintErrors` output.

---

## Stage 8: Refinement and `package.json` updates

### Files to modify

- **`package.json`** — add a configuration option for enabling/disabling lint
  checkers:

  ```json
  "elot.lint.enabled": {
    "type": "boolean",
    "default": true,
    "description": "Enable ELOT ontology lint checks (structural, CURIE validation, keyword appropriateness)"
  }
  ```

- **`diagnosticsProvider.ts`** — read the setting and skip lint checkers if
  disabled.

### Optional enhancements (not required for initial port)

- **Quick Fix actions** for common errors (e.g., "Add `:nodeclare:` tag" for
  headings missing identifiers).
- **Code Actions** to add missing required sections.
- **Diagnostic tags** — mark warnings as `DiagnosticTag.Unnecessary` or
  `DiagnosticTag.Deprecated` where appropriate.

---

## Summary: File creation order

| Stage | New files | Modified files |
|-------|-----------|----------------|
| 1 | `src/elotLintHelpers.ts`, `src/tests/elotLintHelpers.test.ts` | — |
| 2 | `src/lintStructural.ts`, `src/tests/lintStructural.test.ts` | — |
| 3 | `src/lintRequiredSections.ts`, `src/tests/lintRequiredSections.test.ts` | — |
| 4 | `src/lintHeadingIdentity.ts`, `src/tests/lintHeadingIdentity.test.ts` | `src/elotLintHelpers.ts` |
| 5 | `src/lintDescriptionCuries.ts`, `src/tests/lintDescriptionCuries.test.ts` | — |
| 6 | `src/lintAxiomValues.ts`, `src/tests/lintAxiomValues.test.ts` | — |
| 7 | `src/tests/lintIntegration.test.ts` | `src/diagnosticsProvider.ts` |
| 8 | — | `package.json`, `src/diagnosticsProvider.ts` |

## Key principles

- **Keep checker functions pure** — they take `ElotNode` (+ slurp map +
  prefix map) and return `LintDiagnostic[]`. No VS Code imports.
- **One test file per stage** — small, focused, runnable with `npx tsx`.
- **Reuse existing infrastructure** — `omnKeywords.ts`, `buildSlurp.ts`,
  `types.ts`, `entityFromHeader.ts`, `prefixes.ts` are already ported.
- **The only VS Code coupling** is in `diagnosticsProvider.ts` (Stage 7),
  which maps pure `LintDiagnostic` objects to VS Code `Diagnostic` objects.
