// src/dbHoverProvider.ts
//
// Step 2.3.2: VS Code HoverProvider backed by the ELOT label DB.
//
// Activated for non-Org files (the existing Org `hoverProvider.ts`
// handles those).  When the user hovers over a CURIE / IRI / bare
// URL, this provider:
//
//   1. detects the token via `detectTokenAtOffset`
//   2. queries `ElotDb.getLabelAny` + `getAllAttrsAny` against the
//      effective active sources + language prefs
//   3. renders a Markdown panel via `renderHoverMarkdown`
//
// All settings reads go through `vscode.workspace.getConfiguration`
// so the provider stays in sync with workspace/user setting edits.

import * as vscode from "vscode";
import type { ElotDbBridge } from "./db/bridge.js";
import {
  normalizeActiveSources,
  getEffectiveLanguagePrefs,
} from "./activeSources.js";
import { detectTokenAtOffset } from "./dbHover/token.js";
import { renderHoverMarkdown } from "./dbHover/render.js";

/** Languages we register the DB hover for.  Org is excluded so the
 *  existing Org hover provider keeps full ownership of `.org` files. */
const DEFAULT_INCLUDE_LANGUAGES = [
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
];

function readIncludeLanguages(): string[] {
  const cfg = vscode.workspace.getConfiguration("elot.globalLabelDisplay");
  const raw = cfg.get<unknown>("includeLanguages");
  if (Array.isArray(raw)) {
    const langs = raw.filter(
      (x): x is string => typeof x === "string" && x.length > 0,
    );
    if (langs.length > 0) return langs;
  }
  return DEFAULT_INCLUDE_LANGUAGES;
}

function isHoverEnabled(): boolean {
  const cfg = vscode.workspace.getConfiguration("elot.globalLabelDisplay");
  return cfg.get<boolean>("hoverEnabled", true);
}

function readActiveSources(): ReturnType<typeof normalizeActiveSources> {
  const cfg = vscode.workspace.getConfiguration("elot");
  return normalizeActiveSources(cfg.get<unknown>("activeLabelSources"));
}

function readPreferredLanguages(): ReturnType<typeof getEffectiveLanguagePrefs> {
  const cfg = vscode.workspace.getConfiguration("elot");
  return getEffectiveLanguagePrefs(cfg.get<unknown>("preferredLanguages"));
}

export class ElotDbHoverProvider implements vscode.HoverProvider {
  constructor(private readonly bridge: ElotDbBridge) {}

  async provideHover(
    document: vscode.TextDocument,
    position: vscode.Position,
    _token: vscode.CancellationToken,
  ): Promise<vscode.Hover | null> {
    if (!isHoverEnabled()) return null;
    // Org files are handled by the existing provider; defensive
    // guard in case the selector is mis-edited via settings.
    if (document.languageId === "org") return null;

    const line = document.lineAt(position.line).text;
    const hit = detectTokenAtOffset(line, position.character);
    if (!hit) return null;

    const active = readActiveSources();
    if (active.length === 0) return null;

    const db = await this.bridge.get();
    if (!db) return null;

    const prefs = readPreferredLanguages();
    const label = db.getLabelAny(hit.token, active);
    const all = db.getAllAttrsAny(hit.token, active, prefs);

    if (label === null && (all === null || all.entries.length === 0)) {
      return null;
    }

    const md = renderHoverMarkdown({
      token: hit.token,
      label,
      attrs: all?.entries ?? [],
      origin: all?.sourceOrigin ?? null,
    });

    const ms = new vscode.MarkdownString(md);
    ms.isTrusted = false;
    ms.supportHtml = false;

    const range = new vscode.Range(
      position.line,
      hit.start,
      position.line,
      hit.end,
    );
    return new vscode.Hover(ms, range);
  }
}

/**
 * Register the DB hover provider.  Returns a Disposable bundling the
 * provider registration plus a `workspace.onDidChangeConfiguration`
 * watcher that re-registers if `includeLanguages` changes (so users
 * don't need to reload the window).
 */
export function registerDbHoverProvider(
  context: vscode.ExtensionContext,
  bridge: ElotDbBridge,
): vscode.Disposable {
  const provider = new ElotDbHoverProvider(bridge);
  let providerReg: vscode.Disposable | null = null;

  const register = () => {
    if (providerReg) {
      providerReg.dispose();
      providerReg = null;
    }
    const langs = readIncludeLanguages().filter((l) => l !== "org");
    if (langs.length === 0) return;
    const selectors: vscode.DocumentSelector = langs.map((l) => ({
      language: l,
    }));
    providerReg = vscode.languages.registerHoverProvider(selectors, provider);
    context.subscriptions.push(providerReg);
  };

  register();

  const cfgWatch = vscode.workspace.onDidChangeConfiguration((e) => {
    if (e.affectsConfiguration("elot.globalLabelDisplay.includeLanguages")) {
      register();
    }
  });

  return {
    dispose() {
      cfgWatch.dispose();
      if (providerReg) providerReg.dispose();
    },
  };
}
