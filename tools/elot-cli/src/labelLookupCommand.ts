// src/labelLookupCommand.ts
//
// Step 2.4.1: VS Code commands for label lookup.
//
//   - elot.labelLookup           (default scope from settings)
//   - elot.labelLookupLocal      (scope = local)
//   - elot.labelLookupExternal   (scope = external)
//
// Pure logic lives in `./labelLookup/items.ts`; this file only does
// the VS Code wiring (active editor / settings / QuickPick / insert).

import * as vscode from "vscode";
import type { ElotDbBridge } from "./db/bridge.js";
import type { ElotDb, ActiveSource } from "./db/sqljs.js";
import {
  normalizeActiveSources,
  getEffectiveLanguagePrefs,
} from "./activeSources.js";
import {
  buildLookupItems,
  insertTokenForId,
  type DbLookupView,
  type LocalEntry,
  type LookupItem,
} from "./labelLookup/items.js";
import { parseOrg } from "./parseOrgWasm.js";
import { buildSlurp } from "./buildSlurp.js";
import { getPrefixMap } from "./parseOrgWasm.js";

const SETTING_SCOPE = "labelLookup.scope";

type Scope = "local" | "external" | "both";

function readScope(): Scope {
  const cfg = vscode.workspace.getConfiguration("elot");
  const v = cfg.get<string>(SETTING_SCOPE, "both");
  return v === "local" || v === "external" ? v : "both";
}

function readActive(): ActiveSource[] {
  const cfg = vscode.workspace.getConfiguration("elot");
  return normalizeActiveSources(cfg.get<unknown>("activeLabelSources")).map(
    (s) => ({ source: s.source, dataSource: s.dataSource ?? "" }),
  );
}

function readPrefs() {
  const cfg = vscode.workspace.getConfiguration("elot");
  return getEffectiveLanguagePrefs(cfg.get<unknown>("preferredLanguages"));
}

// ─── Local slurp extraction ──────────────────────────────────

interface LocalContext {
  entries: LocalEntry[];
  /** uri -> CURIE contracter using the buffer's prefix table. */
  contract: ((uri: string) => string | null) | null;
}

const EMPTY_LOCAL: LocalContext = { entries: [], contract: null };

/** Strip enclosing "..."@lang quotation form a slurp label. */
function cleanSlurpLabel(s: string): string {
  const m = s.match(/^"(.*)"(?:@[A-Za-z-]+)?$/);
  return m ? m[1] : s;
}

function localContext(doc: vscode.TextDocument): LocalContext {
  if (doc.languageId !== "org" && !doc.fileName.endsWith(".org"))
    return EMPTY_LOCAL;
  try {
    const root = parseOrg(doc.getText());
    const slurp = buildSlurp(root);
    const entries: LocalEntry[] = [];
    for (const [uri, e] of slurp) {
      entries.push({
        id: uri,
        label: cleanSlurpLabel(e.label),
        rdfType: e.rdfType,
      });
    }
    const prefMap = getPrefixMap(root);
    let contract: ((uri: string) => string | null) | null = null;
    if (prefMap && prefMap.size > 0) {
      // Sort prefix entries by descending expansion length for longest-match.
      const pairs = [...prefMap.entries()].sort(
        (a, b) => b[1].length - a[1].length,
      );
      contract = (uri: string): string | null => {
        for (const [prefix, expansion] of pairs) {
          if (uri.length > expansion.length && uri.startsWith(expansion)) {
            return `${prefix}:${uri.slice(expansion.length)}`;
          }
        }
        return null;
      };
    }
    return { entries, contract };
  } catch (err) {
    console.error("[elot] labelLookup: local slurp failed:", err);
    return EMPTY_LOCAL;
  }
}

// ─── DB view adaptor ─────────────────────────────────────────

function dbView(db: ElotDb, active: ActiveSource[], prefs: ReturnType<typeof readPrefs>): DbLookupView {
  return {
    idsByLabel: db.allActiveLabels(active, prefs),
    contractUri(uri: string): string | null {
      const c = db.contractUri(uri, active);
      return c.length > 0 ? c[0] : null;
    },
    labelVariants(id: string) {
      return db.labelVariants(id, active);
    },
    rdfTypeForId(id: string): string | null {
      return db.getAttr(id, "rdf:type", active, prefs);
    },
  };
}

// ─── Insertion ───────────────────────────────────────────────

async function insertAtCursor(token: string): Promise<void> {
  const ed = vscode.window.activeTextEditor;
  if (!ed) return;
  await ed.edit((b) => {
    for (const sel of ed.selections) {
      if (sel.isEmpty) {
        b.insert(sel.active, token);
      } else {
        b.replace(sel, token);
      }
    }
  });
}

// ─── Main flow ───────────────────────────────────────────────

interface QPItem extends vscode.QuickPickItem {
  _id: string;
}

function toQpItem(it: LookupItem): QPItem {
  return {
    label: it.label,
    description: it.rdfType ? it.rdfType : undefined,
    detail: it.detail ? it.detail : undefined,
    _id: it.id,
  };
}

async function runLookup(
  bridge: ElotDbBridge,
  scopeOverride?: Scope,
): Promise<void> {
  const ed = vscode.window.activeTextEditor;
  if (!ed) {
    vscode.window.showWarningMessage("ELOT: no active editor.");
    return;
  }
  const scope: Scope = scopeOverride ?? readScope();

  const localCtx = localContext(ed.document);
  const isOrg =
    ed.document.languageId === "org" ||
    ed.document.fileName.endsWith(".org");

  // In non-Org buffers, "local" and "both" collapse to DB-only,
  // since no slurp is available.
  const effectiveScope: Scope =
    isOrg
      ? scope
      : scope === "local"
        ? "external"
        : scope === "both"
          ? "external"
          : scope;

  const active = readActive();
  const prefs = readPrefs();

  let view: DbLookupView | null = null;
  if (effectiveScope !== "local" && active.length > 0) {
    const db = await bridge.get();
    if (db) view = dbView(db, active, prefs);
  }

  if (effectiveScope === "local" && localCtx.entries.length === 0) {
    vscode.window.showInformationMessage(
      "ELOT: no local labels in this buffer (slurp empty).",
    );
    return;
  }
  if (effectiveScope === "external" && view === null) {
    vscode.window.showInformationMessage(
      "ELOT: no active label sources (set elot.activeLabelSources in settings).",
    );
    return;
  }

  const items = buildLookupItems({
    scope: effectiveScope,
    local: localCtx.entries,
    db: view,
    prefs,
  });
  if (items.length === 0) {
    vscode.window.showInformationMessage(
      "ELOT: no candidates available for label lookup.",
    );
    return;
  }
  // Sort alphabetically (stable) so similar labels group together.
  items.sort((a, b) =>
    a.label.localeCompare(b.label, undefined, { sensitivity: "base" }),
  );

  const picked = await vscode.window.showQuickPick(items.map(toQpItem), {
    title: `ELOT: Label lookup (${effectiveScope})`,
    placeHolder: "Type to filter; Enter to insert CURIE/IRI at cursor",
    matchOnDescription: true,
    matchOnDetail: true,
  });
  if (!picked) return;

  const token = insertTokenForId(picked._id, view, localCtx.contract);
  await insertAtCursor(token);
}

// ─── Registration ────────────────────────────────────────────

export function registerLabelLookupCommands(
  bridge: ElotDbBridge,
): vscode.Disposable {
  const a = vscode.commands.registerCommand("elot.labelLookup", () =>
    runLookup(bridge),
  );
  const b = vscode.commands.registerCommand("elot.labelLookupLocal", () =>
    runLookup(bridge, "local"),
  );
  const c = vscode.commands.registerCommand(
    "elot.labelLookupExternal",
    () => runLookup(bridge, "external"),
  );
  return vscode.Disposable.from(a, b, c);
}
