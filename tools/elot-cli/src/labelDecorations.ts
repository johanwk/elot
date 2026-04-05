// src/labelDecorations.ts
//
// Decoration-based label display toggle for ELOT Org files.
//
// When enabled, CURIEs in the editor are visually replaced by their
// human-readable labels using TextEditorDecorationType with:
//   - The original CURIE text hidden (font-size: 0, letter-spacing collapsed)
//   - A `before` pseudo-element showing the label text
//
// This mimics Emacs' `display` text property behaviour — the underlying
// buffer content is unchanged, but the user sees labels instead of CURIEs.
//
// Toggle with the `elot.toggleLabels` command (F5 by default).

import * as vscode from "vscode";
import type { SlurpEntry } from "./buildSlurp.js";
import { buildSlurp } from "./buildSlurp.js";
import { parseOrg } from "./parseOrgWasm.js";

// ─── CURIE regex (same as hoverProvider.ts) ──────────────────────

/**
 * Global regex matching CURIEs like obo:BFO_0000001 or :localName.
 * Must use /g flag for iterating over matches.
 */
const CURIE_GLOBAL_RE = /(?:[a-zA-Z][-a-zA-Z0-9_.]*|):[-\w_./]+/g;

// ─── State ───────────────────────────────────────────────────────

/** Whether label display is currently active */
let labelsVisible = false;

/** Status bar item showing current label state */
let statusBarItem: vscode.StatusBarItem | undefined;

/**
 * Per-CURIE decoration types.
 * We create one decoration type per unique label text, because the
 * `before.contentText` is fixed at type-creation time.
 */
const decorationTypes = new Map<string, vscode.TextEditorDecorationType>();

/**
 * Get or create a decoration type that hides the original text and
 * shows `labelText` as a before pseudo-element.
 */
function getDecorationType(labelText: string): vscode.TextEditorDecorationType {
  let dt = decorationTypes.get(labelText);
  if (!dt) {
    dt = vscode.window.createTextEditorDecorationType({
      // Hide the original CURIE text
      textDecoration: "none; font-size: 0; letter-spacing: -1em; overflow: hidden;",
      before: {
        contentText: labelText,
        fontStyle: "italic",
        color: new vscode.ThemeColor("editor.foreground"),
      },
    });
    decorationTypes.set(labelText, dt);
  }
  return dt;
}

/**
 * Clear all decorations from all visible editors and dispose types.
 */
function clearAllDecorations(): void {
  for (const editor of vscode.window.visibleTextEditors) {
    for (const dt of decorationTypes.values()) {
      editor.setDecorations(dt, []);
    }
  }
  for (const dt of decorationTypes.values()) {
    dt.dispose();
  }
  decorationTypes.clear();
}

// ─── Apply decorations ──────────────────────────────────────────

/**
 * Apply label decorations to all visible Org editors.
 */
function applyDecorations(): void {
  for (const editor of vscode.window.visibleTextEditors) {
    applyDecorationsToEditor(editor);
  }
}

/**
 * Apply label decorations to a single editor.
 */
function applyDecorationsToEditor(editor: vscode.TextEditor): void {
  const doc = editor.document;
  if (doc.languageId !== "org" && !doc.fileName.endsWith(".org")) {
    return;
  }

  if (!labelsVisible) {
    // Clear any existing decorations
    for (const dt of decorationTypes.values()) {
      editor.setDecorations(dt, []);
    }
    return;
  }

  // Build slurp map for this document
  let slurpMap: Map<string, SlurpEntry>;
  try {
    const root = parseOrg(doc.getText());
    slurpMap = buildSlurp(root);
  } catch (err) {
    console.error("elot labelDecorations: failed to parse document", err);
    return;
  }

  if (slurpMap.size === 0) return;

  // Group ranges by label text (since each decoration type has one label)
  const rangesByLabel = new Map<string, vscode.Range[]>();

  const text = doc.getText();

  // Reset regex lastIndex
  CURIE_GLOBAL_RE.lastIndex = 0;
  let match: RegExpExecArray | null;

  while ((match = CURIE_GLOBAL_RE.exec(text)) !== null) {
    const curie = match[0];
    const entry = slurpMap.get(curie);
    if (!entry) continue;

    // Don't decorate if label is same as URI (no benefit)
    if (entry.label === entry.uri) continue;

    const startPos = doc.positionAt(match.index);
    const endPos = doc.positionAt(match.index + curie.length);
    const range = new vscode.Range(startPos, endPos);

    const label = entry.label;
    let ranges = rangesByLabel.get(label);
    if (!ranges) {
      ranges = [];
      rangesByLabel.set(label, ranges);
    }
    ranges.push(range);
  }

  // First, clear decorations for labels no longer present
  for (const dt of decorationTypes.values()) {
    editor.setDecorations(dt, []);
  }

  // Apply decorations grouped by label
  for (const [label, ranges] of rangesByLabel) {
    const dt = getDecorationType(label);
    editor.setDecorations(dt, ranges);
  }
}

// ─── Toggle command ──────────────────────────────────────────────

/**
 * Toggle label display on/off. This is the command handler for
 * `elot.toggleLabels`.
 */
export function toggleLabels(): void {
  labelsVisible = !labelsVisible;

  if (labelsVisible) {
    applyDecorations();
    vscode.window.setStatusBarMessage("$(tag) ELOT: Labels ON", 3000);
  } else {
    clearAllDecorations();
    vscode.window.setStatusBarMessage("$(code) ELOT: Labels OFF", 3000);
  }

  updateStatusBarItem();
}

/**
 * Returns the current label-display state (for testing / status bar).
 */
export function isLabelsVisible(): boolean {
  return labelsVisible;
}

// ─── Status bar ──────────────────────────────────────────────────

/**
 * Update the status bar item to reflect the current label-display state.
 */
function updateStatusBarItem(): void {
  if (!statusBarItem) return;

  if (labelsVisible) {
    statusBarItem.text = "$(tag) Labels";
    statusBarItem.tooltip = "ELOT: Label display is ON — click or press F5 to toggle";
    statusBarItem.backgroundColor = new vscode.ThemeColor(
      "statusBarItem.warningBackground",
    );
  } else {
    statusBarItem.text = "$(code) CURIEs";
    statusBarItem.tooltip = "ELOT: Label display is OFF — click or press F5 to toggle";
    statusBarItem.backgroundColor = undefined;
  }
}

/**
 * Show/hide the status bar item based on the active editor.
 * Only shown when an Org file is active.
 */
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
 * Register the label decoration feature:
 *   - The toggle command
 *   - Listeners to re-apply decorations when editors change
 *
 * Returns a Disposable that cleans up everything.
 */
export function registerLabelDecorations(
  context: vscode.ExtensionContext,
): void {
  // Create status bar item
  statusBarItem = vscode.window.createStatusBarItem(
    vscode.StatusBarAlignment.Right,
    100,
  );
  statusBarItem.command = "elot.toggleLabels";
  updateStatusBarItem();
  updateStatusBarVisibility();

  // Register the toggle command
  const cmd = vscode.commands.registerCommand(
    "elot.toggleLabels",
    toggleLabels,
  );

  // Re-apply when the active editor changes
  const onEditorChange = vscode.window.onDidChangeActiveTextEditor(
    (editor) => {
      updateStatusBarVisibility();
      if (editor && labelsVisible) {
        applyDecorationsToEditor(editor);
      }
    },
  );

  // Re-apply when document text changes (debounced via a small delay)
  let updateTimer: ReturnType<typeof setTimeout> | undefined;
  const onDocChange = vscode.workspace.onDidChangeTextDocument((event) => {
    if (!labelsVisible) return;
    // Only for Org files
    const doc = event.document;
    if (doc.languageId !== "org" && !doc.fileName.endsWith(".org")) return;

    if (updateTimer) clearTimeout(updateTimer);
    updateTimer = setTimeout(() => {
      // Find visible editors for this document and re-apply
      for (const editor of vscode.window.visibleTextEditors) {
        if (editor.document === doc) {
          // Clear old decoration types first since labels may have changed
          clearAllDecorations();
          applyDecorations();
          break;
        }
      }
    }, 500);
  });

  // Re-apply when visible editors change (e.g. split view)
  const onVisibleChange = vscode.window.onDidChangeVisibleTextEditors(() => {
    if (labelsVisible) {
      applyDecorations();
    }
  });

  // Clean-up disposable
  const cleanup = new vscode.Disposable(() => {
    clearAllDecorations();
    if (updateTimer) clearTimeout(updateTimer);
    labelsVisible = false;
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
