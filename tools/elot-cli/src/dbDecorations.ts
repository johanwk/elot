// src/dbDecorations.ts
//
// Step 2.3.3: DB-backed label decorations for non-Org files.
//
// Mirrors `labelDecorations.ts` (the Org-side toggle) but pulls
// labels from the read-only `ElotDbBridge` instead of the
// document-local slurp map.  Provides:
//
//   - Command `elot.toggleGlobalLabels` (F5 in non-Org buffers)
//   - Per-label `TextEditorDecorationType` with `before:` pseudo +
//     zeroed-font-size hiding -- visually identical to the Org side
//   - Status-bar indicator that shows DB state (sources / entities)
//     and label-display state, plus a "too many" warning when the
//     scan cap is reached.
//   - Reload on bridge change / settings change / editor switch /
//     document edit (debounced).
//
// Coexistence:
//   - .org files are skipped entirely (Org has its own provider).
//   - The selector is gated on `elot.globalLabelDisplay.includeLanguages`
//     (empty -> the same default set as the hover provider).
//
// Performance:
//   - Scan capped at `elot.globalLabelDisplay.maxIds` (default 500).
//   - One `applyDecorationsToEditor` per visible editor per change.
//   - DecorationTypes are pooled by label text and reused across edits.

import * as vscode from "vscode";
import type { ElotDbBridge } from "./db/bridge.js";
import {
  normalizeActiveSources,
  getEffectiveLanguagePrefs,
} from "./activeSources.js";
import { scanTokens, ScanHit } from "./dbDecorations/scan.js";

// Same default include set as the hover provider (kept in sync manually).
const DEFAULT_INCLUDE_LANGUAGES = new Set([
  "plaintext",
  "markdown",
  "typescript",
  "javascript",
  "python",
  "json",
  "jsonc",
  "yaml",
  "xml",
  "turtle",
  "sparql",
]);

function readIncludeLanguages(): Set<string> {
  const cfg = vscode.workspace.getConfiguration("elot.globalLabelDisplay");
  const raw = cfg.get<unknown>("includeLanguages");
  if (Array.isArray(raw)) {
    const langs = raw.filter(
      (x): x is string => typeof x === "string" && x.length > 0,
    );
    if (langs.length > 0) return new Set(langs);
  }
  return DEFAULT_INCLUDE_LANGUAGES;
}

function readMaxIds(): number {
  const cfg = vscode.workspace.getConfiguration("elot.globalLabelDisplay");
  const v = cfg.get<number>("maxIds", 500);
  return Number.isFinite(v) && v > 0 ? Math.floor(v) : 500;
}

function readFontStyle(): "italic" | "normal" | "oblique" {
  const cfg = vscode.workspace.getConfiguration("elot.labelDisplay");
  const s = cfg.get<string>("fontStyle", "italic");
  return s === "normal" || s === "oblique" ? s : "italic";
}

function readActiveSources() {
  const cfg = vscode.workspace.getConfiguration("elot");
  return normalizeActiveSources(cfg.get<unknown>("activeLabelSources"));
}

function readPrefs() {
  const cfg = vscode.workspace.getConfiguration("elot");
  return getEffectiveLanguagePrefs(cfg.get<unknown>("preferredLanguages"));
}

/** Should this document participate in DB-backed decorations? */
function isEligibleDocument(doc: vscode.TextDocument): boolean {
  if (doc.languageId === "org") return false;
  if (doc.fileName.endsWith(".org")) return false;
  return readIncludeLanguages().has(doc.languageId);
}

// ---- decoration-type pool ----------------------------------------

/** Per-label decoration type cache (label text -> DT). */
const decorationTypes = new Map<string, vscode.TextEditorDecorationType>();
let currentFontStyle: string = "italic";

function getDecorationType(label: string): vscode.TextEditorDecorationType {
  let dt = decorationTypes.get(label);
  if (!dt) {
    dt = vscode.window.createTextEditorDecorationType({
      textDecoration:
        "none; font-size: 0; letter-spacing: -1em; overflow: hidden;",
      before: {
        contentText: label,
        fontStyle: currentFontStyle,
        color: new vscode.ThemeColor("editor.foreground"),
      },
    });
    decorationTypes.set(label, dt);
  }
  return dt;
}

function disposeAllDecorationTypes(): void {
  for (const editor of vscode.window.visibleTextEditors) {
    for (const dt of decorationTypes.values()) {
      editor.setDecorations(dt, []);
    }
  }
  for (const dt of decorationTypes.values()) dt.dispose();
  decorationTypes.clear();
}

function clearEditorDecorations(editor: vscode.TextEditor): void {
  for (const dt of decorationTypes.values()) editor.setDecorations(dt, []);
}

// ---- state -------------------------------------------------------

let visible = false;
let bridgeRef: ElotDbBridge | null = null;
let statusBarItem: vscode.StatusBarItem | undefined;
/** Was the last scan capped by maxIds? */
let capReached = false;
/** Per-editor debounce timers keyed by document URI string. */
const pendingTimers = new Map<string, ReturnType<typeof setTimeout>>();

// ---- core: apply ------------------------------------------------

async function applyDecorationsToEditor(
  editor: vscode.TextEditor,
): Promise<void> {
  const doc = editor.document;
  if (!isEligibleDocument(doc)) {
    clearEditorDecorations(editor);
    return;
  }
  if (!visible) {
    clearEditorDecorations(editor);
    return;
  }

  const db = await bridgeRef?.get();
  if (!db) {
    clearEditorDecorations(editor);
    return;
  }
  const active = readActiveSources();
  if (active.length === 0) {
    clearEditorDecorations(editor);
    return;
  }
  const prefs = readPrefs();

  const text = doc.getText();
  const max = readMaxIds();
  const hits = scanTokens(text, max);
  capReached = hits.length >= max;

  const rangesByLabel = new Map<string, vscode.Range[]>();
  for (const h of hits) {
    const label = db.getLabelAny(h.token, active, prefs);
    if (label == null) continue;
    if (label === h.token) continue; // no benefit
    const startPos = doc.positionAt(h.start);
    const endPos = doc.positionAt(h.end);
    const range = new vscode.Range(startPos, endPos);
    let arr = rangesByLabel.get(label);
    if (!arr) {
      arr = [];
      rangesByLabel.set(label, arr);
    }
    arr.push(range);
  }

  // Clear any decoration types not used this round.
  for (const dt of decorationTypes.values()) editor.setDecorations(dt, []);

  for (const [label, ranges] of rangesByLabel) {
    const dt = getDecorationType(label);
    editor.setDecorations(dt, ranges);
  }

  updateStatusBarItem();
}

async function applyAll(): Promise<void> {
  for (const editor of vscode.window.visibleTextEditors) {
    await applyDecorationsToEditor(editor);
  }
}

function scheduleApply(editor: vscode.TextEditor, delay = 250): void {
  const key = editor.document.uri.toString();
  const existing = pendingTimers.get(key);
  if (existing) clearTimeout(existing);
  const t = setTimeout(() => {
    pendingTimers.delete(key);
    applyDecorationsToEditor(editor).catch((err) =>
      console.error("[elot] dbDecorations apply failed:", err),
    );
  }, delay);
  pendingTimers.set(key, t);
}

// ---- toggle command ---------------------------------------------

async function toggleCommand(): Promise<void> {
  // If invoked from an .org buffer the existing Org toggle should
  // win.  Fall through to its command for parity.
  const editor = vscode.window.activeTextEditor;
  if (
    editor &&
    (editor.document.languageId === "org" ||
      editor.document.fileName.endsWith(".org"))
  ) {
    await vscode.commands.executeCommand("elot.toggleLabels");
    return;
  }

  visible = !visible;

  if (visible) {
    await applyAll();
    vscode.window.setStatusBarMessage("$(tag) ELOT: Global labels ON", 3000);
  } else {
    disposeAllDecorationTypes();
    vscode.window.setStatusBarMessage("$(code) ELOT: Global labels OFF", 3000);
  }
  updateStatusBarItem();
}

export function isGlobalLabelsVisible(): boolean {
  return visible;
}

// ---- status bar -------------------------------------------------

async function updateStatusBarItem(): Promise<void> {
  if (!statusBarItem) return;

  const db = await bridgeRef?.get();
  const active = readActiveSources();

  if (!db) {
    statusBarItem.text = "$(database) ELOT: no DB";
    statusBarItem.tooltip =
      "ELOT label DB not loaded. Use `elot-cli db register ...` to populate it, then run `Elot: Label DB Info` to verify.";
    statusBarItem.backgroundColor = undefined;
    return;
  }

  const allSources = db.listSources();
  const totalSources = allSources.length;
  const activeCount = active.length;

  // Count entities across active sources only -- that is the count
  // that actually drives hover/decoration lookups.  Falls back to the
  // grand total when no sources are active so the user still sees
  // DB magnitude.
  let activeEntities = 0;
  for (const a of active) {
    activeEntities += db.sourceEntityCount(a.source, a.dataSource ?? "");
  }
  let totalEntities = 0;
  for (const s of allSources) {
    totalEntities += db.sourceEntityCount(s.source, s.dataSource ?? "");
  }

  const labelStateIcon = visible ? "$(tag)" : "$(code)";
  const labelStateText = visible ? "Labels" : "CURIEs";

  // Compact id count (e.g. 985, 1.2k, 12k).
  const idsText = formatCount(activeCount > 0 ? activeEntities : totalEntities);
  const srcText =
    activeCount === totalSources
      ? `${totalSources} src`
      : `${activeCount}/${totalSources} src`;

  statusBarItem.text =
    `${labelStateIcon} ${labelStateText} ` +
    `(${srcText}, ${idsText} ids${capReached ? ", capped" : ""})`;

  // Per-source breakdown lines (cap at 10 to keep tooltip tidy).
  const srcLines: string[] = [];
  const isActive = (src: string, ds: string): boolean =>
    active.some((a) => a.source === src && (a.dataSource ?? "") === ds);
  const shown = allSources.slice(0, 10);
  for (const s of shown) {
    const ds = s.dataSource ?? "";
    const n = db.sourceEntityCount(s.source, ds);
    const marker = isActive(s.source, ds) ? "*" : " ";
    srcLines.push(`  ${marker} ${s.source}${ds ? ` <${ds}>` : ""}: ${n}`);
  }
  if (allSources.length > shown.length) {
    srcLines.push(`  ... (+${allSources.length - shown.length} more)`);
  }

  const lines = [
    `ELOT label DB`,
    `  ${totalSources} source${totalSources === 1 ? "" : "s"} registered, ${totalEntities} ids total`,
    `  ${activeCount} active source${activeCount === 1 ? "" : "s"}, ${activeEntities} ids in scope`,
    `Sources (* = active):`,
    ...srcLines,
    ``,
    `Display: ${visible ? "ON" : "OFF"} (click or F5 to toggle in non-Org files)`,
  ];
  if (capReached) {
    const max = readMaxIds();
    lines.push(
      `Note: scan capped at ${max} ids (elot.globalLabelDisplay.maxIds).`,
    );
  }
  statusBarItem.tooltip = lines.join("\n");
  statusBarItem.backgroundColor = capReached
    ? new vscode.ThemeColor("statusBarItem.warningBackground")
    : visible
      ? new vscode.ThemeColor("statusBarItem.prominentBackground")
      : undefined;
}

function formatCount(n: number): string {
  if (n < 1000) return String(n);
  if (n < 10000) return (n / 1000).toFixed(1).replace(/\.0$/, "") + "k";
  return Math.round(n / 1000) + "k";
}

function updateStatusBarVisibility(): void {
  if (!statusBarItem) return;
  const editor = vscode.window.activeTextEditor;
  if (!editor) {
    statusBarItem.hide();
    return;
  }
  // Show whenever the DB feature is meaningful: any non-Org buffer
  // covered by the include-languages set, OR any buffer when the
  // user has registered active sources (so they always see DB
  // status if they've opted in).
  if (isEligibleDocument(editor.document)) {
    statusBarItem.show();
    return;
  }
  if (readActiveSources().length > 0) {
    statusBarItem.show();
    return;
  }
  statusBarItem.hide();
}

// ---- registration -----------------------------------------------

export function registerDbDecorations(
  context: vscode.ExtensionContext,
  bridge: ElotDbBridge,
): vscode.Disposable {
  bridgeRef = bridge;
  currentFontStyle = readFontStyle();

  statusBarItem = vscode.window.createStatusBarItem(
    vscode.StatusBarAlignment.Right,
    99, // sit just left of the Org-side item (priority 100)
  );
  statusBarItem.command = "elot.toggleGlobalLabels";
  updateStatusBarItem();
  updateStatusBarVisibility();

  const cmd = vscode.commands.registerCommand(
    "elot.toggleGlobalLabels",
    toggleCommand,
  );

  // Re-apply on editor / visibility / document changes.
  const onActive = vscode.window.onDidChangeActiveTextEditor((editor) => {
    updateStatusBarVisibility();
    if (editor && visible) scheduleApply(editor, 0);
  });
  const onVisible = vscode.window.onDidChangeVisibleTextEditors(() => {
    if (visible) {
      for (const editor of vscode.window.visibleTextEditors) {
        scheduleApply(editor, 0);
      }
    }
  });
  const onDocChange = vscode.workspace.onDidChangeTextDocument((event) => {
    if (!visible) return;
    for (const editor of vscode.window.visibleTextEditors) {
      if (editor.document === event.document) scheduleApply(editor, 250);
    }
  });

  // Re-apply on DB change.
  const offBridge = bridge.onDidChange(() => {
    updateStatusBarItem();
    if (visible) applyAll().catch(() => {});
  });

  // Re-apply on relevant settings changes.
  const onCfg = vscode.workspace.onDidChangeConfiguration((e) => {
    const interesting =
      e.affectsConfiguration("elot.activeLabelSources") ||
      e.affectsConfiguration("elot.preferredLanguages") ||
      e.affectsConfiguration("elot.globalLabelDisplay.includeLanguages") ||
      e.affectsConfiguration("elot.globalLabelDisplay.maxIds") ||
      e.affectsConfiguration("elot.labelDisplay.fontStyle");
    if (!interesting) return;
    // Font-style change requires recreating decoration types.
    if (e.affectsConfiguration("elot.labelDisplay.fontStyle")) {
      const newStyle = readFontStyle();
      if (newStyle !== currentFontStyle) {
        currentFontStyle = newStyle;
        disposeAllDecorationTypes();
      }
    }
    updateStatusBarItem();
    updateStatusBarVisibility();
    if (visible) applyAll().catch(() => {});
  });

  const cleanup = new vscode.Disposable(() => {
    for (const t of pendingTimers.values()) clearTimeout(t);
    pendingTimers.clear();
    disposeAllDecorationTypes();
    if (statusBarItem) {
      statusBarItem.dispose();
      statusBarItem = undefined;
    }
    visible = false;
    bridgeRef = null;
  });

  context.subscriptions.push(
    cmd,
    onActive,
    onVisible,
    onDocChange,
    onCfg,
    cleanup,
    { dispose: offBridge },
    statusBarItem,
  );

  return cleanup;
}
