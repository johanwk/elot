// src/diagnosticsProvider.ts
//
// VS Code diagnostics for ELOT Org-mode ontology files.
//
// 1. **OMN syntax errors** — validates Manchester Syntax axiom values
//    using the Peggy parser. Errors are reported on the value portion
//    of the description list line.
//
// 2. **ELOT lint errors** — 8 structural/semantic checkers ported from
//    elot-lint.el. Diagnostics are mapped to line numbers by searching
//    for heading titles or description list tags in the raw document.
//
// For now (Option A from LINT-PORT-INSTRUCTIONS), the diagnostic range
// is located by scanning the raw document text for the relevant heading
// or description-list line. Source-offset precision can be improved
// later by propagating orgize byte offsets.

import * as vscode from "vscode";
import { parseOrg } from "./parseOrgWasm.js";
import { checkOmnSyntax } from "./omnSyntaxCheck.js";
import { isPropertyKeyword } from "./omnKeywords.js";
import type { ElotNode, DescriptionItem } from "./types.js";
import type { LintDiagnostic } from "./elotLintHelpers.js";
import { collectAllLintErrors } from "./collectLintErrors.js";

// ─── Regex to locate description-list items in raw Org text ──────

/**
 * Matches a description list item:  `  - Tag :: value`
 * Captures: (1) tag, (2) value text (rest of line)
 *
 * The value may continue on following lines, but for diagnostics we
 * report on the first line that contains the tag.
 */
const DESC_LINE_RE = /^(\s*- )(.+?)( :: )(.*)$/;

// ─── Regex to locate Org headings ────────────────────────────────

/**
 * Matches an Org heading: `** Title  :tag1:tag2:`
 * Captures: (1) stars+space, (2) heading text (may include trailing tags)
 */
const HEADING_RE = /^(\*+ )(.+)$/;

// ─── Collect OMN diagnostics ─────────────────────────────────────

interface OmnDiagnostic {
  /** The OMN keyword, e.g. "SubClassOf" */
  keyword: string;
  /** The axiom value that was checked */
  value: string;
  /** Error message from the parser */
  message: string;
  /** Character offset within the value where the error was detected */
  offsetInValue: number;
}

/**
 * Walk an ElotNode tree and collect all description items with OMN keywords
 * that have syntax errors.
 */
function collectOmnErrors(root: ElotNode): OmnDiagnostic[] {
  const errors: OmnDiagnostic[] = [];
  const stack: ElotNode[] = [root];

  while (stack.length > 0) {
    const node = stack.pop()!;
    for (const desc of node.descriptions ?? []) {
      if (isPropertyKeyword(desc.tag)) {
        const result = checkOmnSyntax(desc.tag, desc.value);
        if (!result.ok) {
          errors.push({
            keyword: desc.tag,
            value: desc.value,
            message: result.message,
            offsetInValue: result.offset,
          });
        }
      }
    }
    // Push children in reverse for document order
    const children = node.children ?? [];
    for (let i = children.length - 1; i >= 0; i--) {
      stack.push(children[i]);
    }
  }

  return errors;
}

/**
 * Given a document and a list of OmnDiagnostic, locate each error in
 * the raw text and produce VS Code Diagnostic objects.
 *
 * Strategy: for each error, search the document for a line matching
 *   `- <keyword> :: <value>`
 * and place the diagnostic on the value portion.  If the same keyword+value
 * appears multiple times, we match them in document order.
 */
function mapOmnToDiagnostics(
  doc: vscode.TextDocument,
  errors: OmnDiagnostic[],
): vscode.Diagnostic[] {
  if (errors.length === 0) return [];

  const diagnostics: vscode.Diagnostic[] = [];

  // Build a lookup: for each (keyword, value) pair, which lines match?
  // We iterate the document once, collecting candidate lines.
  const lineIndex = new Map<string, number[]>(); // key → [lineNumbers]

  for (let i = 0; i < doc.lineCount; i++) {
    const line = doc.lineAt(i);
    const m = DESC_LINE_RE.exec(line.text);
    if (!m) continue;
    const tag = m[2].trim();
    const val = m[4]; // raw value on this line
    const key = `${tag}\0${val}`;
    let arr = lineIndex.get(key);
    if (!arr) {
      arr = [];
      lineIndex.set(key, arr);
    }
    arr.push(i);
  }

  // Track consumption: if the same keyword+value appears multiple times,
  // match errors in order.
  const consumed = new Map<string, number>(); // key → next-index-into-arr

  for (const err of errors) {
    const trimmedValue = err.value.trim();
    const key = `${err.keyword}\0${trimmedValue}`;
    const candidates = lineIndex.get(key);
    if (!candidates || candidates.length === 0) {
      // Couldn't locate the line — try a broader search
      // Fall back: search all lines for the keyword
      for (let i = 0; i < doc.lineCount; i++) {
        const line = doc.lineAt(i);
        const m = DESC_LINE_RE.exec(line.text);
        if (m && m[2].trim() === err.keyword) {
          const valStart = m[1].length + m[2].length + m[3].length;
          const range = new vscode.Range(i, valStart, i, line.text.length);
          const diag = new vscode.Diagnostic(
            range,
            `OMN syntax error: ${err.message}`,
            vscode.DiagnosticSeverity.Error,
          );
          diag.source = "elot";
          diagnostics.push(diag);
          break;
        }
      }
      continue;
    }

    const idx = consumed.get(key) ?? 0;
    consumed.set(key, idx + 1);
    const lineNo = candidates[idx % candidates.length];
    const line = doc.lineAt(lineNo);
    const m = DESC_LINE_RE.exec(line.text);
    if (m) {
      // Place the diagnostic on the value portion of the line
      const valStart = m[1].length + m[2].length + m[3].length;
      const range = new vscode.Range(lineNo, valStart, lineNo, line.text.length);
      const diag = new vscode.Diagnostic(
        range,
        `OMN syntax error in ${err.keyword}: ${err.message}`,
        vscode.DiagnosticSeverity.Error,
      );
      diag.source = "elot";
      diagnostics.push(diag);
    }
  }

  return diagnostics;
}

// ─── Map lint diagnostics to VS Code diagnostics ─────────────────

/**
 * Build a line-number index from the raw document text.
 *
 * Returns two maps:
 * - headingLines: heading title text → [line numbers] (for node-level diagnostics)
 * - descTagLines: "tag\0value" → [line numbers] (for description-item diagnostics)
 */
function buildLineIndex(doc: vscode.TextDocument): {
  headingLines: Map<string, number[]>;
  descTagLines: Map<string, number[]>;
} {
  const headingLines = new Map<string, number[]>();
  const descTagLines = new Map<string, number[]>();

  for (let i = 0; i < doc.lineCount; i++) {
    const text = doc.lineAt(i).text;

    // Check for heading
    const hm = HEADING_RE.exec(text);
    if (hm) {
      // Strip trailing tags for matching:  "Title  :tag1:tag2:" → "Title"
      const rawTitle = hm[2].replace(/\s+:[\w:]+:\s*$/, "").trim();
      let arr = headingLines.get(rawTitle);
      if (!arr) {
        arr = [];
        headingLines.set(rawTitle, arr);
      }
      arr.push(i);
      continue;
    }

    // Check for description list item
    const dm = DESC_LINE_RE.exec(text);
    if (dm) {
      const tag = dm[2].trim();
      const val = dm[4];
      const key = `${tag}\0${val}`;
      let arr = descTagLines.get(key);
      if (!arr) {
        arr = [];
        descTagLines.set(key, arr);
      }
      arr.push(i);
    }
  }

  return { headingLines, descTagLines };
}

/**
 * Find the line number for a heading title in the document.
 * Consumes from the `consumed` map to handle duplicate headings.
 */
function findNodeLine(
  title: string,
  headingLines: Map<string, number[]>,
  consumed: Map<string, number>,
): number | null {
  // Strip trailing tags for matching
  const cleanTitle = title.replace(/\s+:[\w:]+:\s*$/, "").trim();
  const candidates = headingLines.get(cleanTitle);
  if (!candidates || candidates.length === 0) return null;
  const idx = consumed.get(cleanTitle) ?? 0;
  // Don't advance consumed — a node-level diagnostic should anchor to
  // the first (or Nth) occurrence. Multiple diagnostics on the same node
  // use the same line.
  return candidates[Math.min(idx, candidates.length - 1)];
}

/**
 * Map pure LintDiagnostic objects to VS Code Diagnostic objects,
 * locating each one in the raw document text.
 */
function mapLintToDiagnostics(
  doc: vscode.TextDocument,
  lintErrors: LintDiagnostic[],
): vscode.Diagnostic[] {
  if (lintErrors.length === 0) return [];

  const { headingLines } = buildLineIndex(doc);
  const diagnostics: vscode.Diagnostic[] = [];

  // Track consumption for heading lookups
  const consumed = new Map<string, number>();

  for (const lint of lintErrors) {
    // Try to use the explicit line number if provided
    let lineNo: number | null = lint.line != null ? lint.line - 1 : null;

    // If no explicit line, find the heading in the document
    if (lineNo == null && lint.node.title) {
      lineNo = findNodeLine(lint.node.title, headingLines, consumed);
    }

    // Default to line 0 if we can't find it (root-level diagnostics)
    if (lineNo == null) lineNo = 0;

    // Build the range — highlight the whole line
    const line = doc.lineAt(Math.min(lineNo, doc.lineCount - 1));
    const range = new vscode.Range(
      line.lineNumber,
      line.firstNonWhitespaceCharacterIndex,
      line.lineNumber,
      line.text.length,
    );

    const severity =
      lint.severity === "error"
        ? vscode.DiagnosticSeverity.Error
        : vscode.DiagnosticSeverity.Warning;

    const diag = new vscode.Diagnostic(range, lint.message, severity);
    diag.source = "elot";
    diagnostics.push(diag);
  }

  return diagnostics;
}

// ─── Provider registration ───────────────────────────────────────

/**
 * Register the ELOT diagnostics provider.
 *
 * Combines OMN syntax checking (Peggy parser) with the 8 ELOT lint
 * checkers to produce a unified set of VS Code diagnostics.
 *
 * Diagnostics are updated:
 *   - On document save
 *   - On text change (debounced, 500ms)
 *   - When a document is opened
 *
 * Call this from `activate()` in extension.ts.
 */
export function registerDiagnosticsProvider(
  context: vscode.ExtensionContext,
): vscode.Disposable {
  const collection = vscode.languages.createDiagnosticCollection("elot-omn");

  function updateDiagnostics(doc: vscode.TextDocument): void {
    if (doc.languageId !== "org" && !doc.fileName.endsWith(".org")) {
      return;
    }

    try {
      const orgText = doc.getText();
      const root = parseOrg(orgText);

      // OMN syntax diagnostics (existing)
      const omnErrors = collectOmnErrors(root);
      const omnDiagnostics = mapOmnToDiagnostics(doc, omnErrors);

      // ELOT lint diagnostics (new — checkers #1–#8)
      const lintEnabled = vscode.workspace
        .getConfiguration("elot.lint")
        .get<boolean>("enabled", true);
      const lintErrors = lintEnabled ? collectAllLintErrors(root) : [];
      const lintDiagnostics = mapLintToDiagnostics(doc, lintErrors);

      // Merge into one collection
      collection.set(doc.uri, [...omnDiagnostics, ...lintDiagnostics]);
    } catch (e) {
      // If parsing fails entirely, clear diagnostics rather than crash
      collection.delete(doc.uri);
      console.error("elot diagnostics error:", e);
    }
  }

  // Update on save
  const onSave = vscode.workspace.onDidSaveTextDocument((doc) => {
    updateDiagnostics(doc);
  });

  // Update on text change (debounced)
  let timer: ReturnType<typeof setTimeout> | undefined;
  const onChange = vscode.workspace.onDidChangeTextDocument((event) => {
    const doc = event.document;
    if (doc.languageId !== "org" && !doc.fileName.endsWith(".org")) return;
    if (timer) clearTimeout(timer);
    timer = setTimeout(() => updateDiagnostics(doc), 500);
  });

  // Update when a document is opened
  const onOpen = vscode.workspace.onDidOpenTextDocument((doc) => {
    updateDiagnostics(doc);
  });

  // Clean up when a document is closed
  const onClose = vscode.workspace.onDidCloseTextDocument((doc) => {
    collection.delete(doc.uri);
  });

  // Run on all currently open documents
  for (const doc of vscode.workspace.textDocuments) {
    updateDiagnostics(doc);
  }

  const disposable = vscode.Disposable.from(
    collection,
    onSave,
    onChange,
    onOpen,
    onClose,
  );

  context.subscriptions.push(disposable);
  return disposable;
}
