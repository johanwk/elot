// src/descriptionListDecorations.ts
//
// Fontification for Org-mode description list tags.
//
// In Org-mode, a description list item looks like:
//
//   - tag :: description text
//
// This provider decorates the tag portion (from the `- ` up to and
// including the ` :: `) in a subdued/muted colour, so the
// description value stands out more clearly.
//
// The decoration is always active for Org files (no toggle needed),
// since it's a purely aesthetic enhancement that doesn't change
// semantics.

import * as vscode from "vscode";

// ─── Regex ───────────────────────────────────────────────────────

/**
 * Matches an Org description list tag line.
 *
 * Captures:
 *   - Group 0 (full match): the entire `- tag :: ` portion
 *   - We decorate from the start of `- ` to the end of ` :: `
 *
 * Pattern breakdown:
 *   ^(\s*- )   — leading whitespace + list marker (` - `)
 *   (.+?)      — the tag text (non-greedy)
 *   ( :: )     — the description separator
 */
const DESC_TAG_RE = /^(\s*- )(.+?)( :: )/;

// ─── Decoration types ────────────────────────────────────────────

/**
 * Decoration for the tag portion of a description list.
 * Uses a ThemeColor so it adapts to light/dark themes.
 * `descriptionForeground` is VS Code's built-in muted text colour —
 * the same one used for parameter hints, breadcrumbs secondary text, etc.
 */
let tagDecorationType: vscode.TextEditorDecorationType | undefined;

function getTagDecorationType(): vscode.TextEditorDecorationType {
  if (!tagDecorationType) {
    tagDecorationType = vscode.window.createTextEditorDecorationType({
      color: new vscode.ThemeColor("descriptionForeground"),
      fontStyle: "italic",
    });
  }
  return tagDecorationType;
}

// ─── Apply decorations ──────────────────────────────────────────

function applyToEditor(editor: vscode.TextEditor): void {
  const doc = editor.document;
  if (doc.languageId !== "org" && !doc.fileName.endsWith(".org")) {
    return;
  }

  const ranges: vscode.Range[] = [];

  for (let i = 0; i < doc.lineCount; i++) {
    const line = doc.lineAt(i);
    const m = DESC_TAG_RE.exec(line.text);
    if (m) {
      // m[1] = `  - `, m[2] = tag text, m[3] = ` :: `
      // Decorate from the start of `- ` to end of ` :: `
      const startChar = m[1].length - m[1].trimStart().length === m[1].length
        ? 0
        : m[1].length - m[1].trimStart().length;
      // Actually we want to include the `- ` and ` :: `, i.e. the full match
      const matchStart = line.text.indexOf(m[0]);
      const matchEnd = matchStart + m[0].length;
      ranges.push(new vscode.Range(i, matchStart, i, matchEnd));
    }
  }

  editor.setDecorations(getTagDecorationType(), ranges);
}

function applyToAllEditors(): void {
  for (const editor of vscode.window.visibleTextEditors) {
    applyToEditor(editor);
  }
}

// ─── Registration ────────────────────────────────────────────────

/**
 * Register description list tag fontification.
 *
 * This is always-on for Org files — no toggle command needed.
 * Pushes disposables into context.subscriptions.
 */
export function registerDescriptionListDecorations(
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
    if (tagDecorationType) {
      // Clear from all editors
      for (const editor of vscode.window.visibleTextEditors) {
        editor.setDecorations(tagDecorationType, []);
      }
      tagDecorationType.dispose();
      tagDecorationType = undefined;
    }
  });

  context.subscriptions.push(
    onEditorChange,
    onDocChange,
    onVisibleChange,
    cleanup,
  );
}
