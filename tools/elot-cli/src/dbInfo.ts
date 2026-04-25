// src/dbInfo.ts
//
// `elot.dbInfo` command: pops up a quick diagnostic showing the
// resolved DB path, whether the bridge has it open, the number of
// sources it contains, and the user's current settings for active
// sources / preferred languages / hover enable / include languages.
//
// Designed for first-run / "why doesn't hover work" troubleshooting.

import * as vscode from "vscode";
import type { ElotDbBridge } from "./db/bridge.js";
import {
  normalizeActiveSources,
  normalizePreferredLanguages,
} from "./activeSources.js";

export function registerDbInfoCommand(
  bridge: ElotDbBridge,
): vscode.Disposable {
  return vscode.commands.registerCommand("elot.dbInfo", async () => {
    const cfg = vscode.workspace.getConfiguration("elot");
    const gCfg = vscode.workspace.getConfiguration("elot.globalLabelDisplay");
    const active = normalizeActiveSources(cfg.get<unknown>("activeLabelSources"));
    const prefs = normalizePreferredLanguages(
      cfg.get<unknown>("preferredLanguages"),
    );
    const hoverEnabled = gCfg.get<boolean>("hoverEnabled", true);
    const includeLangs = gCfg.get<unknown>("includeLanguages");

    const path = bridge.path;
    const db = await bridge.get();
    const sources = db ? db.listSources() : [];
    const activeNames = active.map((a) =>
      a.dataSource ? `${a.source} (${a.dataSource})` : a.source,
    );

    const lines: string[] = [];
    lines.push(`DB path:           ${path ?? "(unresolved)"}`);
    lines.push(`Bridge open:       ${bridge.isOpen ? "yes" : "no"}`);
    lines.push(`Sources in DB:     ${sources.length}`);
    if (sources.length > 0) {
      const names = sources
        .map((s) => (s.dataSource ? `${s.source}(${s.dataSource})` : s.source))
        .join(", ");
      lines.push(`                   ${names}`);
    }
    lines.push("");
    lines.push(`activeLabelSources: ${activeNames.length === 0
      ? "(empty - hover will return nothing!)"
      : activeNames.join(", ")}`);
    lines.push(`preferredLanguages: ${prefs.length === 0 ? "(default)" : prefs.join(", ")}`);
    lines.push(`hoverEnabled:       ${hoverEnabled}`);
    lines.push(
      `includeLanguages:   ${
        Array.isArray(includeLangs) && includeLangs.length > 0
          ? (includeLangs as string[]).join(", ")
          : "(default set)"
      }`,
    );

    const body = lines.join("\n");

    // Show in an output channel so the user can copy/paste.
    const ch = vscode.window.createOutputChannel("ELOT: DB Info");
    ch.clear();
    ch.appendLine("=== ELOT label database diagnostic ===");
    ch.appendLine(body);
    ch.show(true);

    // Also a brief modal summary for at-a-glance triage.
    const summary =
      `DB: ${path ?? "(unresolved)"}\n` +
      `Open: ${bridge.isOpen ? "yes" : "no"}    ` +
      `Sources: ${sources.length}    ` +
      `Active: ${activeNames.length}`;
    vscode.window.showInformationMessage(summary, "Show details").then((c) => {
      if (c === "Show details") ch.show(true);
    });
  });
}
