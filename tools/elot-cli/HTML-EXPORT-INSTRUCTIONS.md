# Exporting ELOT Org Files to HTML from VS Code — LLM Instructions

## Goal

Add an "Export to HTML" command to the ELOT VS Code extension (`tools/elot-cli/`)
so that users can produce styled HTML documentation from their ELOT Org ontology
files — without needing Emacs.

In Emacs, HTML export uses `org-export` (`C-c C-e h h`), which understands all
Org markup, heading properties, description lists, source blocks, tables, links,
etc. The ELOT Emacs package provides two HTML themes via `#+SETUPFILE:`:

- **`theme-elot.setup`** — references `elot-style.css` + `elot-nav.js`
  (sidebar TOC with collapsible sections)
- **`theme-readtheorg.setup`** — references the ReadTheOrg theme by Fniessen

Both are just `#+HTML_HEAD:` lines injecting CSS/JS into Org's built-in HTML
exporter. The CSS (`elot-style.css`) provides a sidebar layout; the JS
(`elot-nav.js`) moves the table of contents into a fixed sidebar and adds
toggle/collapse behavior.

The VS Code extension needs an equivalent pipeline that:
1. Converts Org markup → HTML (headings, description lists, tables, links, bold,
   italic, code, source blocks, etc.)
2. Applies the ELOT CSS/JS theme
3. Writes the result to a `.html` file

---

## Context: What Already Exists

### In the VS Code extension (`tools/elot-cli/`)

- **Org parser**: `parseOrgWasm.ts` — Rust/WASM crate (`elot-orgize/`) that
  parses Org text into an `ElotNode` tree. Currently extracts headings,
  property drawers, description lists, prefix tables, and `#+begin_src omn`
  blocks. It does NOT produce a full Org AST — it's tuned for OMN generation.

- **CLI**: `cli.ts` — command-line tool that reads `.org` → generates `.omn`.
  A similar pattern could be followed for HTML export.

- **Extension commands**: `extension.ts` — registers commands like `elot.tangle`.
  A new `elot.exportHtml` command would go here.

- **`package.json`** — declares commands, keybindings, settings.

### In the Emacs package (`elot-package/`)

- **`elot-style.css`** — ELOT HTML theme (sidebar layout, typography)
- **`elot-nav.js`** — sidebar TOC behavior (moves `#table-of-contents` into
  a fixed sidebar, adds collapsible sub-lists, hides CURIE identifiers from
  TOC entries, replaces deep section numbers with thin spaces)
- **`theme-elot.setup`** — Org setup file referencing the above via CDN URLs
- **`theme-readtheorg.setup`** — alternative theme (ReadTheOrg by Fniessen)

### The `ElotNode` tree shape (from `types.ts`)

```typescript
interface ElotNode {
  level: number;           // 0 = root, 1+ = headings
  title: string;           // raw heading text
  tags?: string[];         // e.g. ["nodeclare"]
  uri?: string;            // OWL identifier (CURIE or <URI>)
  label?: string;          // human-readable label
  rdfType?: string;        // e.g. "owl:Class"
  id?: string;             // :ID: property
  elotContextType?: string;
  elotContextLocalname?: string;
  resourcedefs?: boolean;
  prefixdefs?: boolean;
  tangleTargetOmn?: string;
  prefixes?: PrefixEntry[];
  descriptions?: DescriptionItem[];   // tag :: value pairs
  omnSrcBlocks?: string[];
  children?: ElotNode[];
}
```

**Important limitation**: The WASM parser (`elot-orgize/`) extracts only what's
needed for OMN generation. It does NOT extract:
- Inline markup (bold, italic, code, links within paragraph text)
- Paragraphs / body text between headings
- Arbitrary `#+begin_...` blocks (only `#+begin_src omn`)
- `#+SETUPFILE:`, `#+TITLE:`, `#+AUTHOR:`, `#+OPTIONS:` keywords
- Footnotes, horizontal rules, verse/quote blocks

This is the central design decision: **the existing parser is insufficient for
HTML export**. You need either (a) a different/extended parser, or (b) an
external tool like Pandoc.

---

## Design Options

### Option A: Pandoc (recommended)

Use [Pandoc](https://pandoc.org/) as the Org→HTML converter. Pandoc has
excellent Org-mode support (headings, description lists, tables, markup, source
blocks, links, metadata keywords).

**Approach:**
1. Invoke Pandoc as a subprocess: `pandoc -f org -t html5 --standalone`
2. Inject the ELOT CSS/JS via Pandoc's `--css` and `--include-in-header` flags,
   or via a `--template` that embeds them.
3. Optionally bundle the CSS/JS into the extension so it works offline.

**Pros:**
- Pandoc handles ALL Org markup (inline, tables, links, footnotes, etc.)
- Battle-tested, widely available
- Minimal code to write — just subprocess invocation + template
- Supports additional output formats (PDF, DOCX, etc.) for free

**Cons:**
- External dependency — user must install Pandoc (or extension auto-downloads it)
- Pandoc's Org support has minor differences from Emacs `org-export` (e.g.,
  `#+SETUPFILE:` is NOT followed by Pandoc; property drawers are not natively
  rendered; description list syntax may need verification)
- No access to ELOT-specific semantics (rdfType, slurp map, etc.) unless
  pre-processed

**Pandoc limitations with ELOT Org files (to investigate):**
- Pandoc does NOT process `#+SETUPFILE:` includes — the CSS/JS must be injected
  via Pandoc flags instead
- Property drawers (`:PROPERTIES: ... :END:`) — Pandoc may ignore or render
  them literally; verify behavior
- `#+begin_src omn` blocks — Pandoc renders these as `<pre><code>` with
  `class="omn"` (good enough)
- Org link abbreviations (`[[prefix:localname]]`) — Pandoc won't expand these;
  may need pre-processing to resolve CURIE links using the prefix map
- Description lists with `- tag :: value` — Pandoc renders these as `<dl>`;
  verify it handles ELOT's multi-line values correctly

### Option B: Extend the WASM Parser

Extend the Rust `elot-orgize` crate (which already uses the `orgize` Rust
library) to emit full HTML. The `orgize` crate has built-in HTML export support.

**Approach:**
1. Add a `parse_org_to_html()` function in the Rust crate that uses `orgize`'s
   HTML rendering.
2. Inject ELOT CSS/JS as a wrapper around the HTML output.

**Pros:**
- No external dependencies
- Full control over output
- Uses the same Org parser already in the extension

**Cons:**
- Significant Rust development effort
- `orgize`'s HTML export may not handle all ELOT-specific patterns
- Must handle template/CSS injection at the Rust level

### Option C: Use a TypeScript Org→HTML Library

Use a JavaScript/TypeScript Org parser that produces HTML (e.g., `orgajs`,
`org-mode-parser`, or write a custom renderer walking the `orgize` AST).

**Pros:**
- No external dependencies
- Pure TypeScript

**Cons:**
- JS Org parsers are generally incomplete (tables, description lists, etc.)
- Large development effort to get right
- The existing `orga` dependency was already replaced by the WASM parser

---

## Recommended Plan: Pandoc-based Export

### Stage 1: Basic Pandoc invocation

#### Files to create/modify

- **Create `src/exportHtml.ts`** — the export logic (pure TypeScript, no
  `vscode` imports, so it can also be used from `cli.ts`).
- **Modify `src/extension.ts`** — register `elot.exportHtml` command.
- **Modify `package.json`** — declare the command and keybinding.

#### What to implement

1. **`exportOrgToHtml(orgFilePath: string, options?: ExportOptions): Promise<string>`**
   - Spawns `pandoc` as a child process:
     ```
     pandoc <input.org> -f org -t html5 --standalone
       --metadata title="<title from #+TITLE: or first heading>"
       --toc --toc-depth=3
       --css <path-to-elot-style.css>
       --include-in-header <path-to-elot-nav-script-tag.html>
       -o <output.html>
     ```
   - Returns the output file path.
   - Throws if Pandoc is not found (with a helpful error message suggesting
     installation).

2. **`findPandoc(): string | null`** — locates the Pandoc executable:
   - Check `elot.pandocPath` setting (new setting)
   - Check `PATH`
   - Return `null` if not found

3. **Template/CSS injection strategy:**
   - Bundle `elot-style.css` and `elot-nav.js` into the extension (copy from
     `elot-package/` at build time, or embed as string constants).
   - Create a small HTML snippet file or template that wraps the Pandoc output
     with the ELOT CSS and JS.
   - Alternatively, use Pandoc's `--template` flag with a custom HTML5 template.

#### VS Code command

```typescript
vscode.commands.registerCommand("elot.exportHtml", async () => {
  const editor = vscode.window.activeTextEditor;
  if (!editor || !editor.document.fileName.endsWith(".org")) {
    vscode.window.showErrorMessage("Not an Org file");
    return;
  }
  try {
    const outputPath = await exportOrgToHtml(editor.document.fileName);
    vscode.window.showInformationMessage(`Exported to ${outputPath}`);
    // Optionally open in browser or VS Code preview
  } catch (err) {
    vscode.window.showErrorMessage(`Export failed: ${err.message}`);
  }
});
```

#### package.json additions

```json
{
  "command": "elot.exportHtml",
  "title": "Elot: Export to HTML"
}
```

```json
"elot.pandocPath": {
  "type": "string",
  "default": "pandoc",
  "description": "Path to the Pandoc executable (required for HTML export)"
}
```

### Stage 2: Pandoc template + ELOT theme

#### What to implement

1. **Create an ELOT Pandoc HTML5 template** (`elot-template.html5`) that:
   - Includes `<link>` to `elot-style.css` (inline or file reference)
   - Includes `<script>` for `elot-nav.js` (inline or file reference)
   - Uses Pandoc's `$toc$` variable for the table of contents
   - Has a clean `<body>` structure matching what `elot-nav.js` expects
     (specifically, it needs a `<div id="table-of-contents">` wrapping the TOC)

2. **Bundle static assets** — decide between:
   - **Inline**: embed CSS/JS directly in the template (self-contained HTML)
   - **CDN**: reference the GitHub Pages URLs (like the Emacs `.setup` files do)
   - **Local file**: copy CSS/JS next to the HTML output
   - Recommendation: **inline for self-contained output**, with a setting to
     switch to CDN mode.

3. **Handle `elot-nav.js` expectations:**
   - The JS expects a `<div id="table-of-contents">` containing nested `<ul>`
     lists — Pandoc's `--toc` generates `<nav id="TOC">` instead.
   - Either adapt `elot-nav.js` to accept both, or rename the Pandoc TOC div
     in the template.

### Stage 3: Pre-processing for ELOT-specific features

Some ELOT Org conventions need pre-processing before Pandoc, because Pandoc
doesn't understand them:

1. **Org link abbreviations** — ELOT Org files define prefix abbreviations in
   the prefix table and use them as Org links: `[[obo:BFO_0000001]]`. Pandoc
   doesn't expand these. Options:
   - Pre-process the Org text to expand link abbreviations using the prefix map
     before feeding it to Pandoc.
   - Or leave them as literal text (acceptable for a first version).

2. **Property drawers** — `:PROPERTIES: ... :END:` blocks are typically not
   shown in HTML. Pandoc may render them as text. Options:
   - Pre-process to strip them (they're already parsed into node properties).
   - Or use a Pandoc Lua filter to hide them.

3. **`#+SETUPFILE:`** — Pandoc ignores this. The template handles the CSS/JS
   injection, so this is a non-issue.

4. **Section numbering and TOC filtering** — `elot-nav.js` hides CURIE
   identifiers from TOC entries and adjusts section numbering for deep levels.
   This should work as-is since it operates on the DOM.

### Stage 4: CLI support

Extend `cli.ts` to support HTML export:

```
elot <input.org> --html [output.html]
```

This allows CI/CD pipelines to generate HTML documentation.

### Stage 5: Preview in VS Code

Optionally, after export:
- Open the HTML file in the default browser: `vscode.env.openExternal()`
- Or show a webview panel within VS Code with live preview

---

## Key Files to Read Before Starting

| File | Why |
|------|-----|
| `tools/elot-cli/src/extension.ts` | See how commands are registered |
| `tools/elot-cli/src/cli.ts` | CLI pattern to follow |
| `tools/elot-cli/package.json` | Command/setting declarations |
| `tools/elot-cli/src/parseOrgWasm.ts` | Understand what the parser extracts (and doesn't) |
| `tools/elot-cli/src/types.ts` | `ElotNode` shape |
| `elot-package/elot-style.css` | The ELOT HTML theme CSS |
| `elot-package/elot-nav.js` | Sidebar TOC behavior |
| `elot-package/theme-elot.setup` | How Emacs injects the theme |

---

## Open Questions for the Implementer

1. **Pandoc as hard dependency vs. optional?** — Should the extension prompt to
   install Pandoc, or just show a "Pandoc not found" error? Consider adding a
   "Download Pandoc" button in the error message.

2. **Inline CSS/JS vs. CDN vs. local files?** — Self-contained HTML (inline) is
   most portable but produces larger files. CDN requires internet. Local files
   require copying assets. Recommend offering a setting.

3. **Pre-processing Org link abbreviations** — Is this important for the first
   version? In ELOT Org files, CURIE links like `[[obo:BFO_0000001]]` appear
   in body text and description list values. Without expansion, they'll render
   as broken links or literal text in HTML.

4. **Which Pandoc version to target?** — Pandoc 2.x vs. 3.x have different
   defaults for some features. Pandoc 3.x is current. The `--standalone` and
   `--toc` flags are stable across versions.

5. **Should property drawer content be visible in HTML?** — In Emacs export,
   property drawers are hidden by default. The same behavior is probably
   expected.

6. **Description list rendering** — Verify that Pandoc correctly renders ELOT's
   description lists (which use `- tag :: value` with multi-line values and
   optional meta-annotations indented under them). This is critical for the
   ontology documentation quality.

---

## Summary

The recommended approach is **Pandoc as an external tool**, invoked from the
VS Code extension and CLI, with a custom HTML5 template that includes the ELOT
CSS/JS theme. This gives high-fidelity Org→HTML conversion with minimal code,
and Pandoc's broad format support means PDF/DOCX export can be added later.

The main work is:
1. Subprocess management (spawning Pandoc, error handling)
2. A custom Pandoc HTML5 template with ELOT theme
3. Optional pre-processing of ELOT-specific Org conventions
4. VS Code command + CLI flag integration
