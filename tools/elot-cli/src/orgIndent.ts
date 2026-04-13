// src/orgIndent.ts
//
// Virtual indentation for Org-mode files — the VS Code equivalent of
// Emacs's `org-indent-mode`.
//
// When enabled, each headline and its body text are visually indented
// proportional to the headline level.  No actual spaces are added to
// the file — the indentation is purely decorative, using VS Code's
// TextEditorDecorationType `before` pseudo-elements to prepend
// whitespace.
//
//   * Level 1           → no indent  (0 extra spaces)
//   ** Level 2          → 2 spaces of visual indent
//   *** Level 3         → 4 spaces of visual indent
//   body under level 3  → 4 spaces of visual indent (same as its heading)
//
// The stars on headlines are also visually hidden (replaced by
// whitespace) so that the headline text itself appears at the
// indented position — mimicking org-indent-mode's "hide leading
// stars" behaviour.
//
// Toggle with the `elot.toggleOrgIndent` command.

import * as vscode from "vscode";

// ─── Configuration ───────────────────────────────────────────────

/** Number of spaces of visual indent per headline level (beyond level 1). */
const INDENT_PER_LEVEL = 2;

// ─── Headline regex ──────────────────────────────────────────────

/** Matches an Org headline: one or more `*` followed by a space. */
const HEADLINE_RE = /^(\*+)\s/;

// ─── State ───────────────────────────────────────────────────────

let orgIndentEnabled = false;
let statusBarItem: vscode.StatusBarItem | undefined;

/**
 * Decoration types for line-leading indent, keyed by the number of
 * spaces of indent (e.g. 0, 2, 4, 6, …).
 *
 * Each type uses a `before` pseudo-element to render the indent.
 */
const indentDecorationTypes = new Map<number, vscode.TextEditorDecorationType>();

/**
 * Decoration types that hide the leading stars on headlines, keyed
 * by the number of stars to hide (= level - 1, since the last star
 * stays visible as a bullet marker).
 *
 * For a `*** Heading`, we hide the first 2 stars and let the last
 * `* ` remain visible (shifted right by the indent).
 */
const hideStarsDecorationTypes = new Map<number, vscode.TextEditorDecorationType>();

// ─── Decoration type factories ───────────────────────────────────

function getIndentDecorationType(spaces: number): vscode.TextEditorDecorationType {
  let dt = indentDecorationTypes.get(spaces);
  if (!dt) {
    // Use a non-breaking space string as the before-content.
    // We use U+2002 (en space) which has a consistent width.
    const spacer = "\u2002".repeat(spaces);
    dt = vscode.window.createTextEditorDecorationType({
      before: {
        contentText: spacer,
        // Use a monospace font so spacing is predictable
        fontWeight: "normal",
        fontStyle: "normal",
        color: "transparent",
      },
      // Ensure no extra styling on the line itself
    });
    indentDecorationTypes.set(spaces, dt);
  }
  return dt;
}

function getHideStarsDecorationType(starCount: number): vscode.TextEditorDecorationType {
  let dt = hideStarsDecorationTypes.get(starCount);
  if (!dt) {
    dt = vscode.window.createTextEditorDecorationType({
      // Make the leading stars invisible (but still occupy no space)
      textDecoration: "none; font-size: 0; letter-spacing: -1em; overflow: hidden;",
    });
    hideStarsDecorationTypes.set(starCount, dt);
  }
  return dt;
}

// ─── Core logic ──────────────────────────────────────────────────

/**
 * Compute the indent level (number of visual indent spaces) for
 * a given headline level.
 *
 *   level 1 → 0 spaces  (top-level headings stay at the left margin)
 *   level 2 → 2 spaces
 *   level 3 → 4 spaces
 *   ...
 */
function indentForLevel(level: number): number {
  return Math.max(0, (level - 1) * INDENT_PER_LEVEL);
}

/**
 * Apply org-indent decorations to a single editor.
 */
function applyOrgIndentToEditor(editor: vscode.TextEditor): void {
  const doc = editor.document;
  if (doc.languageId !== "org" && !doc.fileName.endsWith(".org")) {
    return;
  }

  if (!orgIndentEnabled) {
    clearDecorationsFromEditor(editor);
    return;
  }

  // Scan the document to determine each line's indent level
  // based on the headline hierarchy.
  const lineCount = doc.lineCount;

  // First pass: find headline levels
  interface HeadlineInfo {
    line: number;
    level: number;
  }
  const headlines: HeadlineInfo[] = [];

  for (let i = 0; i < lineCount; i++) {
    const text = doc.lineAt(i).text;
    const m = HEADLINE_RE.exec(text);
    if (m) {
      headlines.push({ line: i, level: m[1].length });
    }
  }

  // Build a per-line indent map.
  // Lines before the first headline get 0 indent.
  // Each headline and its body lines get indent based on the headline level.
  const lineIndent = new Array<number>(lineCount).fill(0);
  const lineHeadlineLevel = new Array<number>(lineCount).fill(0); // >0 if line is a headline

  for (let h = 0; h < headlines.length; h++) {
    const startLine = headlines[h].line;
    const endLine = h + 1 < headlines.length ? headlines[h + 1].line : lineCount;
    const indent = indentForLevel(headlines[h].level);

    for (let i = startLine; i < endLine; i++) {
      lineIndent[i] = indent;
    }
    lineHeadlineLevel[startLine] = headlines[h].level;
  }

  // Group lines by indent level (for the before-pseudo indent decoration)
  const linesByIndent = new Map<number, vscode.Range[]>();
  // Group headline lines that need star-hiding, by number of stars to hide
  const linesByHiddenStars = new Map<number, vscode.Range[]>();

  for (let i = 0; i < lineCount; i++) {
    const indent = lineIndent[i];
    if (indent > 0) {
      // Apply indent to the beginning of the line (zero-width range)
      const range = new vscode.Range(i, 0, i, 0);
      let arr = linesByIndent.get(indent);
      if (!arr) {
        arr = [];
        linesByIndent.set(indent, arr);
      }
      arr.push(range);
    }

    const headlineLevel = lineHeadlineLevel[i];
    if (headlineLevel > 1) {
      // Hide the leading stars (all but the last one).
      // For `*** Heading`, hide chars 0..1 (the first 2 stars),
      // leaving `* Heading` visible.
      const starsToHide = headlineLevel - 1;
      const range = new vscode.Range(i, 0, i, starsToHide);
      let arr = linesByHiddenStars.get(starsToHide);
      if (!arr) {
        arr = [];
        linesByHiddenStars.set(starsToHide, arr);
      }
      arr.push(range);
    }
  }

  // Clear previous decorations
  clearDecorationsFromEditor(editor);

  // Apply indent decorations
  for (const [indent, ranges] of linesByIndent) {
    const dt = getIndentDecorationType(indent);
    editor.setDecorations(dt, ranges);
  }

  // Apply star-hiding decorations
  for (const [stars, ranges] of linesByHiddenStars) {
    const dt = getHideStarsDecorationType(stars);
    editor.setDecorations(dt, ranges);
  }
}

/**
 * Clear all org-indent decorations from a single editor.
 */
function clearDecorationsFromEditor(editor: vscode.TextEditor): void {
  for (const dt of indentDecorationTypes.values()) {
    editor.setDecorations(dt, []);
  }
  for (const dt of hideStarsDecorationTypes.values()) {
    editor.setDecorations(dt, []);
  }
}

/**
 * Clear and dispose all decoration types.
 */
function clearAllDecorations(): void {
  for (const editor of vscode.window.visibleTextEditors) {
    clearDecorationsFromEditor(editor);
  }
  for (const dt of indentDecorationTypes.values()) {
    dt.dispose();
  }
  indentDecorationTypes.clear();
  for (const dt of hideStarsDecorationTypes.values()) {
    dt.dispose();
  }
  hideStarsDecorationTypes.clear();
}

/**
 * Apply org-indent decorations to all visible Org editors.
 */
function applyToAllEditors(): void {
  for (const editor of vscode.window.visibleTextEditors) {
    applyOrgIndentToEditor(editor);
  }
}

// ─── Toggle command ──────────────────────────────────────────────

export function toggleOrgIndent(): void {
  orgIndentEnabled = !orgIndentEnabled;

  if (orgIndentEnabled) {
    applyToAllEditors();
    vscode.window.setStatusBarMessage("$(indent) ELOT: Org Indent ON", 3000);
  } else {
    clearAllDecorations();
    vscode.window.setStatusBarMessage("$(indent) ELOT: Org Indent OFF", 3000);
  }

  updateStatusBarItem();
}

export function isOrgIndentEnabled(): boolean {
  return orgIndentEnabled;
}

// ─── Status bar ──────────────────────────────────────────────────

function updateStatusBarItem(): void {
  if (!statusBarItem) return;

  if (orgIndentEnabled) {
    statusBarItem.text = "$(indent) Indent";
    statusBarItem.tooltip = "ELOT: Org Indent is ON — click to toggle";
    statusBarItem.backgroundColor = new vscode.ThemeColor(
      "statusBarItem.warningBackground",
    );
  } else {
    statusBarItem.text = "$(list-flat) Flat";
    statusBarItem.tooltip = "ELOT: Org Indent is OFF — click to toggle";
    statusBarItem.backgroundColor = undefined;
  }
}

function updateStatusBarVisibility(): void {
  if (!statusBarItem) return;
  const editor = vscode.window.activeTextEditor;
  if (
    editor &&
    (editor.document.languageId === "org" ||
      editor.document.fileName.endsWith(".org"))
  ) {
    statusBarItem.show();
  } else {
    statusBarItem.hide();
  }
}

// ─── Registration ────────────────────────────────────────────────

/**
 * Register the org-indent feature:
 *   - The toggle command
 *   - Listeners to re-apply decorations when editors/documents change
 *
 * Returns nothing; pushes disposables into context.subscriptions.
 */
export function registerOrgIndent(context: vscode.ExtensionContext): void {
  // Status bar item
  statusBarItem = vscode.window.createStatusBarItem(
    vscode.StatusBarAlignment.Right,
    99, // just left of the label toggle (priority 100)
  );
  statusBarItem.command = "elot.toggleOrgIndent";
  updateStatusBarItem();
  updateStatusBarVisibility();

  // Toggle command
  const cmd = vscode.commands.registerCommand(
    "elot.toggleOrgIndent",
    toggleOrgIndent,
  );

  // Re-apply when active editor changes
  const onEditorChange = vscode.window.onDidChangeActiveTextEditor(
    (editor) => {
      updateStatusBarVisibility();
      if (editor && orgIndentEnabled) {
        applyOrgIndentToEditor(editor);
      }
    },
  );

  // Re-apply when document text changes (debounced)
  let updateTimer: ReturnType<typeof setTimeout> | undefined;
  const onDocChange = vscode.workspace.onDidChangeTextDocument((event) => {
    if (!orgIndentEnabled) return;
    const doc = event.document;
    if (doc.languageId !== "org" && !doc.fileName.endsWith(".org")) return;

    if (updateTimer) clearTimeout(updateTimer);
    updateTimer = setTimeout(() => {
      for (const editor of vscode.window.visibleTextEditors) {
        if (editor.document === doc) {
          applyOrgIndentToEditor(editor);
        }
      }
    }, 300);
  });

  // Re-apply when visible editors change (split view, etc.)
  const onVisibleChange = vscode.window.onDidChangeVisibleTextEditors(() => {
    if (orgIndentEnabled) {
      applyToAllEditors();
    }
  });

  // Clean-up
  const cleanup = new vscode.Disposable(() => {
    clearAllDecorations();
    if (updateTimer) clearTimeout(updateTimer);
    orgIndentEnabled = false;
    if (statusBarItem) {
      statusBarItem.dispose();
      statusBarItem = undefined;
    }
  });

  context.subscriptions.push(
    cmd,
    statusBarItem,
    onEditorChange,
    onDocChange,
    onVisibleChange,
    cleanup,
  );
}
