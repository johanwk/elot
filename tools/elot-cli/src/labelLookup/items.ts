// src/labelLookup/items.ts
//
// Step 2.4.1: pure helpers for the `elot.labelLookup` family of
// commands.  Builds a flat list of QuickPick items from:
//
//   - the buffer's local slurp (Org-only; empty otherwise), and/or
//   - the DB-side `allActiveLabels()` over the active sources,
//
// applying scope-aware filtering, local-wins dedup by id, multi-id
// disambiguation via CURIE/IRI suffixes, and the Step 1.16.8
// language-suffix annotation on lang-only singletons.  Pure --
// no vscode import; testable under tsx.

import type { LangRow, LangPref } from "../db/langPicker.js";
import { selectByLanguage, looksLikeUriP } from "../db/langPicker.js";

/** Where a candidate id was sourced from. */
export type LookupOrigin = "local" | "external" | "both";

/** A buffer-local entry harvested from the Org slurp pipeline. */
export interface LocalEntry {
  /** Id as produced by buildSlurp: CURIE, or "<URI>" with brackets. */
  id: string;
  /** Plain label (no enclosing quotes / lang tag). */
  label: string;
  /** rdf:type CURIE or URI when known; otherwise undefined. */
  rdfType?: string;
}

/** Data the item builder needs from the DB read surface. */
export interface DbLookupView {
  /** label -> list of ids (Step 1.15 shape from allActiveLabels). */
  idsByLabel: Map<string, string[]>;
  /** Picks a CURIE for an IRI given the active sources, or null. */
  contractUri(uri: string): string | null;
  /** rdfs:label rows for an id (Step 1.16.8); null when none. */
  labelVariants(id: string): LangRow[] | null;
  /** rdf:type for an id (first source wins); null when unknown. */
  rdfTypeForId(id: string): string | null;
}

/** A single QuickPick row to render. */
export interface LookupItem {
  /** Visible label (always = entity label, no suffix). */
  label: string;
  /** Right-hand description (rdf:type or empty). */
  rdfType: string;
  /**
   * Faint right-hand detail.  Contains the disambiguation suffix
   * (CURIE or IRI tail) when the label is collision-prone, the
   * "@LANG" suffix per Step 1.16.8 when the singleton id has
   * multiple language variants, and an `[external]` / `[local]`
   * origin marker when scope = both.  Empty when nothing applies.
   */
  detail: string;
  /** Id to feed into the insert formatter. */
  id: string;
  /** Provenance for the right-hand origin marker. */
  origin: LookupOrigin;
}

/** Inputs for the item builder. */
export interface BuildItemsInput {
  scope: "local" | "external" | "both";
  /** Buffer-local slurp entries (empty in non-Org files). */
  local: readonly LocalEntry[];
  /** DB read surface, or null when no DB / no active sources. */
  db: DbLookupView | null;
  /** Language prefs effective for label-variant selection. */
  prefs?: readonly LangPref[] | null;
}

// ─── helpers ──────────────────────────────────────────────────

/** Strip leading/trailing angle brackets from a single bracketed URI. */
function stripAngleBrackets(id: string): string {
  if (id.length >= 2 && id.startsWith("<") && id.endsWith(">")) {
    const inner = id.slice(1, -1);
    if (!/[\s<>]/.test(inner)) return inner;
  }
  return id;
}

/** Canonical form for id-based dedup across local and DB. */
export function canonicalId(id: string): string {
  return stripAngleBrackets(id);
}

/** Last path/fragment segment of a URI, used as a last-resort suffix. */
function uriTail(uri: string): string {
  const m = /[#/]([^#/]+)\/?$/.exec(uri);
  return m ? m[1] : uri;
}

/**
 * Disambiguation suffix for an id, suitable for the `detail` field.
 * Tries the CURIE contract first; falls back to the URI tail.  For
 * a CURIE id, returns the id itself.
 */
export function suffixForId(id: string, db: DbLookupView | null): string {
  if (looksLikeUriP(id)) {
    if (db) {
      const c = db.contractUri(id);
      if (c) return c;
    }
    return uriTail(id);
  }
  return id;
}

/**
 * Step 1.16.8: for a singleton id whose `rdfs:label` has multiple
 * language variants, return "@LANG" of the language-picker winner;
 * otherwise return "".
 */
export function langSuffixForSingleton(
  label: string,
  id: string,
  db: DbLookupView | null,
  prefs?: readonly LangPref[] | null,
): string {
  if (!db) return "";
  const variants = db.labelVariants(id);
  if (!variants || variants.length <= 1) return "";
  const winner = selectByLanguage(variants, prefs);
  if (
    winner &&
    winner.value === label &&
    winner.lang &&
    winner.lang.length > 0
  ) {
    return "@" + winner.lang;
  }
  return "";
}

/**
 * Insertion token for an id.  CURIE preferred; otherwise the
 * angle-bracketed full IRI.  When LOCAL-PREFIX-CONTRACT is supplied
 * (callable: uri -> CURIE | null), it is consulted /before/ the DB
 * contract for local IRIs so the buffer's own prefix table wins.
 */
export function insertTokenForId(
  id: string,
  db: DbLookupView | null,
  localContract?: ((uri: string) => string | null) | null,
): string {
  const bare = canonicalId(id);
  if (!looksLikeUriP(bare)) {
    // Already a CURIE.
    return bare;
  }
  if (localContract) {
    const c = localContract(bare);
    if (c) return c;
  }
  if (db) {
    const c = db.contractUri(bare);
    if (c) return c;
  }
  return "<" + bare + ">";
}

// ─── item builder ─────────────────────────────────────────────

/**
 * Build the flat list of QuickPick items.  One item per (label, id)
 * pair, with disambiguation only applied when needed.
 *
 * Local wins over DB when the canonical id matches: the local
 * label/rdf:type are kept and the origin becomes "both".
 *
 * No sorting -- VS Code's QuickPick handles fuzzy match order and
 * preserves item order for ties.  Caller may sort by label first
 * if desired.
 */
export function buildLookupItems(inp: BuildItemsInput): LookupItem[] {
  const useLocal = inp.scope === "local" || inp.scope === "both";
  const useDb = inp.scope === "external" || inp.scope === "both";

  // 1) Index local by canonical id.
  interface Cell {
    label: string;
    rdfType: string;
    id: string;
    origin: LookupOrigin;
  }
  const byId = new Map<string, Cell>();
  const order: string[] = []; // canonical-id insertion order

  if (useLocal) {
    for (const e of inp.local) {
      const cid = canonicalId(e.id);
      if (cid.length === 0) continue;
      if (!byId.has(cid)) {
        byId.set(cid, {
          label: e.label,
          rdfType: e.rdfType ?? "",
          id: cid,
          origin: "local",
        });
        order.push(cid);
      }
    }
  }

  // 2) Merge DB labels.  When canonical id collides with a local
  //    cell, mark origin "both" but keep the local label / rdf:type.
  if (useDb && inp.db) {
    for (const [label, ids] of inp.db.idsByLabel) {
      for (const rawId of ids) {
        const cid = canonicalId(rawId);
        if (cid.length === 0) continue;
        const existing = byId.get(cid);
        if (existing) {
          if (existing.origin === "local") existing.origin = "both";
          continue;
        }
        const rt = inp.db.rdfTypeForId(cid) ?? "";
        byId.set(cid, {
          label,
          rdfType: rt,
          id: cid,
          origin: "external",
        });
        order.push(cid);
      }
    }
  }

  // 3) Group by label to detect collisions; per-label decide whether
  //    to add a CURIE/IRI-tail suffix to detail.
  const cells = order.map((id) => byId.get(id)!);
  const labelCounts = new Map<string, number>();
  for (const c of cells) {
    labelCounts.set(c.label, (labelCounts.get(c.label) ?? 0) + 1);
  }

  // 4) Compose items.
  const items: LookupItem[] = [];
  for (const c of cells) {
    let detail = "";
    const colliding = (labelCounts.get(c.label) ?? 0) > 1;
    if (colliding) {
      const suf = suffixForId(c.id, inp.db);
      if (suf.length > 0) detail = suf;
    } else {
      // Step 1.16.8: lang-only multi-variant gets @LANG suffix.
      const lang = langSuffixForSingleton(c.label, c.id, inp.db, inp.prefs);
      if (lang.length > 0) detail = lang;
    }
    // Origin marker (only meaningful in scope = both).
    if (inp.scope === "both") {
      const tag =
        c.origin === "external"
          ? "[external]"
          : c.origin === "both"
            ? "+external"
            : "";
      if (tag.length > 0) {
        detail = detail ? `${detail}  ${tag}` : tag;
      }
    }
    items.push({
      label: c.label,
      rdfType: c.rdfType,
      detail,
      id: c.id,
      origin: c.origin,
    });
  }
  return items;
}
