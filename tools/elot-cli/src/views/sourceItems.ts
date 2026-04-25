// src/views/sourceItems.ts
//
// Step 2.3.7a: pure builders that shape DB rows + the active-sources
// list into render-ready records for the side-bar TreeViews.
//
// No vscode import - tsx-testable.  The vscode wrappers
// (sourcesTreeProvider.ts / activeSourcesTreeProvider.ts) map these
// records to TreeItem instances at the last possible moment.

import type { SourceKey } from "../sourceCommands/reorder.js";
import { keyEquals } from "../sourceCommands/reorder.js";
import type { SourceRow } from "../db/sqljs.js";

/**
 * One row in the "Sources in DB" view.
 */
export interface DbSourceItem {
  key: SourceKey;
  label: string;            // source name
  /** Right-aligned tree description, e.g. "csv * 985 ids". */
  description: string;
  /** Long tooltip with full data_source path + last-updated time. */
  tooltip: string;
  /** Whether this source appears in elot.activeLabelSources. */
  isActive: boolean;
  /**
   * Stable contextValue used by package.json menu `when` clauses.
   *   - "elotDbSource.active"   - shown with checkmark, deactivate icon
   *   - "elotDbSource.inactive" - dim, activate icon
   */
  contextValue: "elotDbSource.active" | "elotDbSource.inactive";
}

/**
 * One row in the "Active Sources" view.
 */
export interface ActiveSourceItem {
  key: SourceKey;
  /** 1-based priority order display, e.g. "1." */
  prefix: string;
  label: string;
  description: string;
  tooltip: string;
  /** Whether this row can move up (i.e. not at index 0). */
  canMoveUp: boolean;
  /** Whether this row can move down (i.e. not at last index). */
  canMoveDown: boolean;
  /**
   * contextValue gates per-row inline buttons.  Format:
   *   elotActiveSource.{first|last|middle|only}
   * "only"  -> single item, no move buttons
   * "first" -> top, only move-down + deactivate
   * "last"  -> bottom, only move-up + deactivate
   * "middle"-> both move buttons + deactivate
   */
  contextValue:
    | "elotActiveSource.first"
    | "elotActiveSource.last"
    | "elotActiveSource.middle"
    | "elotActiveSource.only";
}

function fmtCount(n: number): string {
  if (n < 1000) return `${n}`;
  if (n < 10_000) return `${(n / 1000).toFixed(1)}k`;
  return `${Math.round(n / 1000)}k`;
}

function fmtRelativeTime(epochSeconds: number | null | undefined, now = Date.now() / 1000): string {
  if (epochSeconds == null || epochSeconds <= 0) return "never";
  const diff = now - epochSeconds;
  if (diff < 0) return "just now";
  if (diff < 60) return `${Math.floor(diff)}s ago`;
  if (diff < 3600) return `${Math.floor(diff / 60)}m ago`;
  if (diff < 86_400) return `${Math.floor(diff / 3600)}h ago`;
  return `${Math.floor(diff / 86_400)}d ago`;
}

/**
 * Build "Sources in DB" view items.  Sorted by source name.  When a
 * source is active, contextValue indicates so for menu binding; the
 * description gets a leading bullet so the user sees the active-state
 * at a glance independent of icon themes.
 *
 * `entityCounts` is an optional id->count map keyed by `keyOf()` so
 * tests can inject without instantiating ElotDb.
 */
export function buildDbSourceItems(
  sources: SourceRow[],
  active: SourceKey[],
  entityCounts?: Map<string, number>,
  now?: number,
): DbSourceItem[] {
  const out: DbSourceItem[] = [];
  for (const r of sources) {
    const key: SourceKey = { source: r.source, dataSource: r.dataSource ?? "" };
    const isActive = active.some((a) => keyEquals(a, key));
    const ck = `${key.source}\u0000${key.dataSource}`;
    const count = entityCounts?.get(ck);
    const parts: string[] = [];
    if (r.type) parts.push(r.type);
    if (count != null) parts.push(`${fmtCount(count)} ids`);
    if (isActive) parts.unshift("*");
    const description = parts.join(" | ");
    const tipLines = [
      `Source: ${r.source}`,
      r.dataSource ? `Data source: ${r.dataSource}` : "Data source: (none)",
      r.type ? `Type: ${r.type}` : "Type: (unknown)",
      count != null ? `Entities: ${count}` : null,
      `Last updated: ${fmtRelativeTime(r.lastUpdated, now)}`,
      isActive ? "Active in current settings" : "Not active",
    ].filter((x): x is string => !!x);
    out.push({
      key,
      label: r.source,
      description,
      tooltip: tipLines.join("\n"),
      isActive,
      contextValue: isActive ? "elotDbSource.active" : "elotDbSource.inactive",
    });
  }
  return out;
}

/**
 * Build "Active Sources" view items, in priority order.  No DB access
 * needed - the active list IS the source of truth here.
 *
 * `dbSources` is optional; when supplied, items missing from the DB
 * are flagged in the description ("not in DB") so the user can
 * recognise stale settings entries.
 */
export function buildActiveSourceItems(
  active: SourceKey[],
  dbSources?: SourceRow[],
): ActiveSourceItem[] {
  const inDb = new Set<string>();
  if (dbSources) {
    for (const r of dbSources) {
      inDb.add(`${r.source}\u0000${r.dataSource ?? ""}`);
    }
  }
  const n = active.length;
  return active.map((k, i) => {
    const ck = `${k.source}\u0000${k.dataSource ?? ""}`;
    const missing = dbSources != null && !inDb.has(ck);
    const descParts: string[] = [];
    if (k.dataSource) descParts.push(k.dataSource);
    if (missing) descParts.push("(not in DB)");
    const tipLines = [
      `Priority: ${i + 1} of ${n}`,
      `Source: ${k.source}`,
      k.dataSource ? `Data source: ${k.dataSource}` : "Data source: (none)",
      missing ? "Warning: not currently registered in the DB." : null,
    ].filter((x): x is string => !!x);
    let contextValue: ActiveSourceItem["contextValue"];
    if (n === 1) contextValue = "elotActiveSource.only";
    else if (i === 0) contextValue = "elotActiveSource.first";
    else if (i === n - 1) contextValue = "elotActiveSource.last";
    else contextValue = "elotActiveSource.middle";
    return {
      key: k,
      prefix: `${i + 1}.`,
      label: k.source,
      description: descParts.join(" | "),
      tooltip: tipLines.join("\n"),
      canMoveUp: i > 0,
      canMoveDown: i < n - 1,
      contextValue,
    };
  });
}
