// src/diagnosticsProvider.ts
//
// VS Code diagnostics for OMN (OWL Manchester Syntax) axioms.
//
// Walks the parsed ElotNode tree, finds description list items whose
// tag is an OMN keyword (SubClassOf, Domain, Facts, …), and runs the
// Peggy parser on the value.  Syntax errors are reported as VS Code
// Diagnostic objects with squiggly underlines.
//
// For now (Option C → heading-level), the diagnostic range covers the
// entire description-list line.  Source-offset precision can be improved
// later by propagating orgize byte offsets (Option A).

import * as vscode from "vscode";
import { parseOrg } from "./parseOrgWasm.js";
import { checkOmnSyntax } from "./omnSyntaxCheck.js";
import { isPropertyKeyword } from "./omnKeywords.js";
import type { ElotNode, DescriptionItem } from "./types.js";

// ─── Regex to locate description-list items in raw Org text ──────

/**
 * Matches a description list item:  `  - Tag :: value`
 * Captures: (1) tag, (2) value text (rest of line)
 *
 * The value may continue on following lines, but for diagnostics we
 * report on the first line that contains the tag.
 */
const DESC_LINE_RE = /^(\s*- )(.+?)( :: )(.*)$/;

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
function mapToDiagnostics(
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
          diagnostics.push(
            new vscode.Diagnostic(range, `OMN syntax error: ${err.message}`, vscode.DiagnosticSeverity.Error),
          );
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

// ─── Provider registration ───────────────────────────────────────

/**
 * Register the OMN syntax diagnostics provider.
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
      const errors = collectOmnErrors(root);
      const diagnostics = mapToDiagnostics(doc, errors);
      collection.set(doc.uri, diagnostics);
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
