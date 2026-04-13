// src/headlineBoldDecorations.ts
//
// Bold fontification for Org-mode headlines.
//
// All Org headlines (lines starting with one or more `*` followed by
// a space) are rendered in bold.  This is an always-on decoration
// for Org files — no toggle needed.
//
// Uses a single decoration type with `fontWeight: "bold"` only —
// no font-size tricks, no textDecoration CSS injection.  This avoids
// conflicts with other decorations (label display, org-indent) that
// use textDecoration for hiding/replacing text.

import * as vscode from "vscode";

// ─── Headline regex ──────────────────────────────────────────────

/** Matches an Org headline: one or more `*` followed by a space. */
const HEADLINE_RE = /^(\*+)\s/;

// ─── Decoration type ─────────────────────────────────────────────

/**
 * Single decoration type for all headline levels — just bold, nothing else.
 */
let headlineDecorationType: vscode.TextEditorDecorationType | undefined;

function getHeadlineDecorationType(): vscode.TextEditorDecorationType {
  if (!headlineDecorationType) {
    headlineDecorationType = vscode.window.createTextEditorDecorationType({
      fontWeight: "bold",
    });
  }
  return headlineDecorationType;
}

// ─── Apply decorations ──────────────────────────────────────────

function applyToEditor(editor: vscode.TextEditor): void {
  const doc = editor.document;
  if (doc.languageId !== "org" && !doc.fileName.endsWith(".org")) {
    return;
  }

  const dt = getHeadlineDecorationType();
  const ranges: vscode.Range[] = [];

  for (let i = 0; i < doc.lineCount; i++) {
    const line = doc.lineAt(i);
    const m = HEADLINE_RE.exec(line.text);
    if (m) {
      ranges.push(new vscode.Range(i, 0, i, line.text.length));
    }
  }

  editor.setDecorations(dt, ranges);
}

function applyToAllEditors(): void {
  for (const editor of vscode.window.visibleTextEditors) {
    applyToEditor(editor);
  }
}

// ─── Registration ────────────────────────────────────────────────

/**
 * Register headline bold fontification.
 *
 * This is always-on for Org files — no toggle command needed.
 * Pushes disposables into context.subscriptions.
 */
export function registerHeadlineBoldDecorations(
  context: vscode.ExtensionContext,
): void {
  // Apply immediately to any open editors
  applyToAllEditors();

  // Re-apply when active editor changes
  const onEditorChange = vscode.window.onDidChangeActiveTextEditor(
    (editor) => {
      if (editor) {
        applyToEditor(editor);
      }
    },
  );

  // Re-apply when document text changes (debounced)
  let updateTimer: ReturnType<typeof setTimeout> | undefined;
  const onDocChange = vscode.workspace.onDidChangeTextDocument((event) => {
    const doc = event.document;
    if (doc.languageId !== "org" && !doc.fileName.endsWith(".org")) return;

    if (updateTimer) clearTimeout(updateTimer);
    updateTimer = setTimeout(() => {
      for (const editor of vscode.window.visibleTextEditors) {
        if (editor.document === doc) {
          applyToEditor(editor);
        }
      }
    }, 300);
  });

  // Re-apply when visible editors change (split view, etc.)
  const onVisibleChange = vscode.window.onDidChangeVisibleTextEditors(() => {
    applyToAllEditors();
  });

  // Clean-up
  const cleanup = new vscode.Disposable(() => {
    if (updateTimer) clearTimeout(updateTimer);
    if (headlineDecorationType) {
      for (const editor of vscode.window.visibleTextEditors) {
        editor.setDecorations(headlineDecorationType, []);
      }
      headlineDecorationType.dispose();
      headlineDecorationType = undefined;
    }
  });

  context.subscriptions.push(
    onEditorChange,
    onDocChange,
    onVisibleChange,
    cleanup,
  );
}
