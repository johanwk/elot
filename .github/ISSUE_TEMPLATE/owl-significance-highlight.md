---
name: OWL-Significance Highlighting
about: Visual annotations showing which parts of an ELOT file impact OMN output
title: Add visual highlighting for OWL-significant content
labels: enhancement
---

## Problem

Users are sometimes confused about which parts of an ELOT Org file actually impact the generated OWL Manchester Syntax (OMN) output, and which are purely documentation, metadata, or comments.

Currently:
- **`elot-lint`** helps find errors
- **Structure** is implicit in the Org outline
- **But there's no visual feedback** about what will/won't appear in the final OMN

## Solution

Use the existing `peg.el` grammar (and `owl-manchester.peggy`) to **visually annotate** which parts of description list items will impact OMN output.

### Example

```org
* Classes
** Dog (ex:Dog)
 - SubClassOf :: ex:Animal some ex:hasFood
   - rdfs:comment :: "Dogs eat meat"
   - iof-av:usageNote :: "Domestic variety preferred"
 - rdfs:label :: "Dog"@en
```

**Annotation behavior:**
- ✅ Green/bold: `ex:Animal some ex:hasFood` — parses successfully, **will appear in OMN**
- ✅ Green: `rdfs:label :: "Dog"@en` — annotation property, **will appear in OMN** (as meta-annotation)
- 💬 Dimmed/gray: `rdfs:comment`, `iof-av:usageNote` (level-2 sublists) — **meta-annotations that WILL appear in OMN output as RDF annotations**
- ⚠️ Yellow/red: unparseable content (already caught by lint)

### Implementation approach

1. **Parsing layer** (already exists):
   - Reuse `elot-owl-grammar.el` + `peg.el` to validate Manchester Syntax
   - Already validated per description list item in `elot-lint.el` (see `elot-check-omn-syntax`)

2. **Annotation/display layer** (new):
   - Add `font-lock` rule or Emacs `overlay` that marks axiom values with parse success/failure status
   - Distinguish between level-1 sublists (meta-annotations that **will** appear in OMN) and comments (that won't)
   - Highlight `:resourcedefs: yes`, `:nodeclare:` context to show structural scope

3. **Optional: Interactive explanation**
   - Command like `M-x elot-explain-line` to show why a particular line is/isn't OWL-significant
   - Parse the OMN fragment and explain its role

### Phases

- **Phase 1 (MVP):** Font-lock coloring of valid OMN axiom values (green = parses, red = fails)
- **Phase 2:** Dimming/graying of meta-annotation sublists to show they ARE significant
- **Phase 3:** Context-aware annotations (show `:resourcedefs:`, `:nodeclare:` scope)
- **Phase 4:** Interactive explanation command

### Related files

- `elot-package/elot-lint.el` — lint checkers, especially `elot-check-omn-syntax`
- `elot-package/elot-owl-grammar.el` — PEG parser (machine-generated from `syntax/owl-manchester.peggy`)
- `elot-package/elot-tangle.el` — tangling logic that selectively includes/excludes content
- `syntax/owl-manchester.peggy` — grammar source of truth

### Acceptance criteria

- [ ] Visual feedback distinguishes OWL-significant from non-significant content
- [ ] Works in Emacs with `elot-mode`
- [ ] Reuses existing `peg.el` grammar (no new parser)
- [ ] Handles meta-annotations correctly (level-2 sublists with URIs tags **do** appear in OMN)
- [ ] Optional: Could be ported to VS Code via TypeScript/Peggy parser later

---

**Note:** Level-2 sublist items with URI tags are meta-annotations on axioms/annotations and **do** appear in OWL output—this is a key ELOT feature.
