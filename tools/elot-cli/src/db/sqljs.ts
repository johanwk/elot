// src/db/sqljs.ts
//
// ElotDb: sql.js-backed read-only port of the elot-db Elisp API.
// Step 2.2.1 scope: read methods + helpers only.  The writer and
// full-file save path land in Step 2.2.2.
//
// Mirrors elot-package/elot-db.el function-for-function where sensible.
// Schema is loaded verbatim from elot-package/schema.sql (Step 2.1);
// on an existing DB, schema_version is asserted to equal 3.

import { readFileSync, existsSync } from "fs";
import { resolve } from "path";
import initSqlJs, {
  Database as SqlJsDatabase,
  SqlJsStatic,
} from "sql.js";
import {
  LangPref,
  LangRow,
  pickValueByLang,
  looksLikeUriP,
  looksLikeCurieP,
} from "./langPicker.js";

export const ELOT_DB_SCHEMA_VERSION = 3;

/** A (source, data_source) pair.  data_source "" is the sentinel. */
export interface ActiveSource {
  source: string;
  dataSource?: string | null;
}

/** Flat plist-like attribute record: "prop" -> value, plus origin. */
export interface EntityAttrs {
  /** Ordered list of [prop, value] pairs in first-seen order. */
  entries: Array<[string, string]>;
  /** Winning source cell (source, data_source). */
  sourceOrigin: { source: string; dataSource: string };
}

export interface SourceRow {
  source: string;
  dataSource: string;
  type: string | null;
  lastModified: number | null;
  lastUpdated: number | null;
}

export interface PrefixRow {
  source: string;
  dataSource: string;
  prefix: string;
  expansion: string;
}

function normDs(ds: string | null | undefined): string {
  return ds ?? "";
}

/**
 * Locate elot-package/schema.sql relative to this module.  Falls back
 * to CLI-local copies if needed; exported so tests can override.
 */
export function locateSchemaSql(): string {
  // tsconfig targets CommonJS (Node16 without package "type":"module"),
  // so __dirname is defined at runtime under tsx and in the esbuild bundle.
  const here = __dirname;
  const candidates = [
    // repo layout: tools/elot-cli/src/db/ -> ../../../../elot-package/schema.sql
    resolve(here, "..", "..", "..", "..", "elot-package", "schema.sql"),
    // bundled: tools/elot-cli/dist/ -> ../../../elot-package/schema.sql
    resolve(here, "..", "..", "..", "elot-package", "schema.sql"),
    // explicitly co-located copy (for a vendored install)
    resolve(here, "..", "schema.sql"),
    resolve(here, "schema.sql"),
  ];
  for (const c of candidates) {
    if (existsSync(c)) return c;
  }
  throw new Error(
    `ElotDb: could not locate schema.sql; tried:\n  ${candidates.join("\n  ")}`,
  );
}

/**
 * Read-only ElotDb over sql.js.  Load with ElotDb.open(path).  All
 * write methods are deliberately absent in Step 2.2.1; they will be
 * added in Step 2.2.2.
 */
export class ElotDb {
  private constructor(
    private readonly db: SqlJsDatabase,
    public readonly path: string | null,
  ) {}

  /**
   * Open (or initialise) an ElotDb at PATH.  When PATH is null or the
   * file does not exist, a fresh in-memory DB is created and the
   * schema is applied.  When PATH exists, the file is loaded and
   * schema_version is asserted to equal 3.
   */
  static async open(path: string | null): Promise<ElotDb> {
    const SQL: SqlJsStatic = await initSqlJs({
      // sql.js ships sql-wasm.wasm in its package root; point at it
      // explicitly so Node invocations from any cwd can find it.
      locateFile: (file: string) => {
        try {
          // Requires `require` (available under our CJS build / tsx).
          // eslint-disable-next-line @typescript-eslint/no-require-imports
          const req: NodeRequire =
            typeof require !== "undefined"
              ? require
              : // eslint-disable-next-line @typescript-eslint/no-var-requires
                (eval("require") as NodeRequire);
          return req.resolve(`sql.js/dist/${file}`);
        } catch {
          return file;
        }
      },
    });
    let db: SqlJsDatabase;
    if (path && existsSync(path)) {
      const bytes = readFileSync(path);
      db = new SQL.Database(bytes);
      ElotDb.assertSchemaV3(db);
    } else {
      db = new SQL.Database();
      ElotDb.applySchema(db);
      db.run("INSERT INTO schema_version (version) VALUES (?)", [
        ELOT_DB_SCHEMA_VERSION,
      ]);
    }
    db.run("PRAGMA foreign_keys = ON");
    return new ElotDb(db, path);
  }

  private static applySchema(db: SqlJsDatabase): void {
    const ddl = readFileSync(locateSchemaSql(), "utf-8");
    db.exec(ddl);
  }

  private static assertSchemaV3(db: SqlJsDatabase): void {
    let stored: unknown;
    try {
      const res = db.exec("SELECT version FROM schema_version LIMIT 1");
      stored = res[0]?.values?.[0]?.[0];
    } catch {
      throw new Error(
        "ElotDb: database is missing the schema_version table; " +
          "refusing to open (pre-v3 DBs are not supported by the CLI; " +
          "open it in Emacs to migrate).",
      );
    }
    if (stored !== ELOT_DB_SCHEMA_VERSION) {
      throw new Error(
        `ElotDb: schema version mismatch (stored ${String(stored)}, ` +
          `expected ${ELOT_DB_SCHEMA_VERSION}); refusing to open. ` +
          `Pre-v3 DBs are not supported by the CLI; open in Emacs to migrate.`,
      );
    }
  }

  /** Close the underlying sql.js handle.  No file is written. */
  close(): void {
    this.db.close();
  }

  /** Raw access -- use sparingly; prefer the typed methods. */
  get raw(): SqlJsDatabase {
    return this.db;
  }

  // ─── helpers ──────────────────────────────────────────────────

  /** Run a SELECT with positional bindings; return rows as tuples. */
  private select(sql: string, params: unknown[] = []): unknown[][] {
    const stmt = this.db.prepare(sql);
    try {
      stmt.bind(params as any);
      const out: unknown[][] = [];
      while (stmt.step()) out.push(stmt.get() as unknown[]);
      return out;
    } finally {
      stmt.free();
    }
  }

  // ─── lookups: sources / prefixes ──────────────────────────────

  listSources(): SourceRow[] {
    return this.select(
      `SELECT source, data_source, type, last_modified, last_updated
         FROM sources
         ORDER BY source, data_source`,
    ).map((r) => ({
      source: r[0] as string,
      dataSource: r[1] as string,
      type: (r[2] as string | null) ?? null,
      lastModified: (r[3] as number | null) ?? null,
      lastUpdated: (r[4] as number | null) ?? null,
    }));
  }

  sourceExistsP(source: string, dataSource?: string | null): boolean {
    const ds = normDs(dataSource);
    const rows = this.select(
      `SELECT 1 FROM sources
        WHERE source = ? AND data_source = ? LIMIT 1`,
      [source, ds],
    );
    return rows.length > 0;
  }

  sourceEntityCount(source: string, dataSource?: string | null): number {
    const ds = normDs(dataSource);
    const rows = this.select(
      `SELECT COUNT(*) FROM entities
        WHERE source = ? AND data_source = ?`,
      [source, ds],
    );
    return (rows[0]?.[0] as number) ?? 0;
  }

  /**
   * Returns true if FILE-MTIME is newer than sources.last_modified,
   * or if the source is unknown.  (Caller supplies the filesystem
   * mtime; the CLI is platform-independent and does not stat here.)
   */
  sourceNeedsUpdateP(
    source: string,
    fileMtime: number | null,
    dataSource?: string | null,
  ): boolean {
    const ds = normDs(dataSource);
    const rows = this.select(
      `SELECT last_modified FROM sources
        WHERE source = ? AND data_source = ? LIMIT 1`,
      [source, ds],
    );
    const dbMtime = rows[0]?.[0] as number | null | undefined;
    if (dbMtime == null) return true;
    if (fileMtime == null) return false;
    return fileMtime > dbMtime;
  }

  listPrefixes(source?: string, dataSource?: string | null): PrefixRow[] {
    let rows: unknown[][];
    if (source != null && dataSource != null) {
      rows = this.select(
        `SELECT source, data_source, prefix, expansion FROM prefixes
          WHERE source = ? AND data_source = ?
          ORDER BY prefix`,
        [source, normDs(dataSource)],
      );
    } else if (source != null) {
      rows = this.select(
        `SELECT source, data_source, prefix, expansion FROM prefixes
          WHERE source = ?
          ORDER BY data_source, prefix`,
        [source],
      );
    } else {
      rows = this.select(
        `SELECT source, data_source, prefix, expansion FROM prefixes
          ORDER BY source, data_source, prefix`,
      );
    }
    return rows.map((r) => ({
      source: r[0] as string,
      dataSource: r[1] as string,
      prefix: r[2] as string,
      expansion: r[3] as string,
    }));
  }

  // ─── CURIE/URI helpers ─────────────────────────────────────────

  private expansionInSources(
    prefix: string,
    active: readonly ActiveSource[],
  ): string | null {
    for (const entry of active) {
      const rows = this.select(
        `SELECT expansion FROM prefixes
          WHERE prefix = ? AND source = ? AND data_source = ? LIMIT 1`,
        [prefix, entry.source, normDs(entry.dataSource)],
      );
      if (rows.length > 0) return rows[0][0] as string;
    }
    return null;
  }

  private expansionGlobal(prefix: string): string | null {
    const rows = this.select(
      `SELECT expansion FROM global_prefixes WHERE prefix = ? LIMIT 1`,
      [prefix],
    );
    return rows.length > 0 ? (rows[0][0] as string) : null;
  }

  /**
   * Expand CURIE ("prefix:local") to a full URI.  ACTIVE first, then
   * global_prefixes.  Returns null when CURIE does not look like one
   * or no expansion is found.
   */
  expandCurie(
    curie: string,
    active: readonly ActiveSource[] = [],
  ): string | null {
    if (typeof curie !== "string") return null;
    const m = /^([^:]*):(.*)$/.exec(curie);
    if (!m) return null;
    const prefix = m[1];
    const local = m[2];
    const exp =
      this.expansionInSources(prefix, active) ?? this.expansionGlobal(prefix);
    return exp ? exp + local : null;
  }

  /**
   * Return candidate CURIE strings for URI ordered by longest
   * expansion first, then by active-source priority, then globals.
   */
  contractUri(
    uri: string,
    active: readonly ActiveSource[] = [],
  ): string[] {
    const candidates: Array<{ len: number; curie: string }> = [];
    for (const entry of active) {
      const rows = this.select(
        `SELECT prefix, expansion FROM prefixes
          WHERE source = ? AND data_source = ?`,
        [entry.source, normDs(entry.dataSource)],
      );
      for (const r of rows) {
        const p = r[0] as string;
        const e = r[1] as string;
        if (uri.length > e.length && uri.startsWith(e)) {
          candidates.push({
            len: e.length,
            curie: `${p}:${uri.slice(e.length)}`,
          });
        }
      }
    }
    const globals = this.select(
      `SELECT prefix, expansion FROM global_prefixes`,
    );
    for (const r of globals) {
      const p = r[0] as string;
      const e = r[1] as string;
      if (uri.length > e.length && uri.startsWith(e)) {
        candidates.push({ len: e.length, curie: `${p}:${uri.slice(e.length)}` });
      }
    }
    // Stable sort by descending expansion length.
    const indexed = candidates.map((c, i) => ({ ...c, i }));
    indexed.sort((a, b) => b.len - a.len || a.i - b.i);
    return indexed.map((c) => c.curie);
  }

  // ─── label lookups ─────────────────────────────────────────────

  private labelInSources(
    id: string,
    active: readonly ActiveSource[],
  ): string | null {
    for (const entry of active) {
      const rows = this.select(
        `SELECT label FROM entities
          WHERE id = ? AND source = ? AND data_source = ? LIMIT 1`,
        [id, entry.source, normDs(entry.dataSource)],
      );
      if (rows.length > 0) return (rows[0][0] as string | null) ?? null;
    }
    return null;
  }

  /**
   * Priority-aware label lookup.  When the winning source has
   * rdfs:label attribute rows, they are consulted first and
   * resolved via the language picker; otherwise the denormalised
   * entities.label is returned.  Mirrors elot-db-get-label.
   */
  getLabel(
    id: string,
    active: readonly ActiveSource[],
    prefs?: readonly LangPref[] | null,
  ): string | null {
    for (const entry of active) {
      const ds = normDs(entry.dataSource);
      const entRows = this.select(
        `SELECT label FROM entities
          WHERE id = ? AND source = ? AND data_source = ? LIMIT 1`,
        [id, entry.source, ds],
      );
      if (entRows.length === 0) continue;
      const attrRows = this.select(
        `SELECT value, lang FROM attributes
          WHERE id = ? AND prop = 'rdfs:label'
            AND source = ? AND data_source = ?`,
        [id, entry.source, ds],
      );
      if (attrRows.length > 0) {
        const picked = pickValueByLang(
          attrRows.map((r) => ({
            value: r[0] as string,
            lang: (r[1] as string | null) ?? "",
          })),
          prefs,
        );
        return picked;
      }
      return (entRows[0][0] as string | null) ?? null;
    }
    return null;
  }

  /**
   * Two-pass label lookup.  Tries TOKEN as a literal id, then as a
   * CURIE expanded to a URI, then as a URI contracted to candidate
   * CURIEs.  Mirrors elot-db-get-label-any.
   */
  getLabelAny(
    token: string,
    active: readonly ActiveSource[],
  ): string | null {
    const direct = this.labelInSources(token, active);
    if (direct !== null) return direct;
    if (looksLikeCurieP(token)) {
      const uri = this.expandCurie(token, active);
      if (uri) {
        const hit = this.labelInSources(uri, active);
        if (hit !== null) return hit;
      }
    }
    if (looksLikeUriP(token)) {
      for (const curie of this.contractUri(token, active)) {
        const hit = this.labelInSources(curie, active);
        if (hit !== null) return hit;
      }
    }
    return null;
  }

  /**
   * Get attribute value for (id, prop) across ACTIVE in priority order.
   * First source with any matching row wins; within the source,
   * language picker collapses multiple rows.  Mirrors elot-db-get-attr.
   */
  getAttr(
    id: string,
    prop: string,
    active: readonly ActiveSource[],
    prefs?: readonly LangPref[] | null,
  ): string | null {
    for (const entry of active) {
      const rows = this.select(
        `SELECT value, lang FROM attributes
          WHERE id = ? AND prop = ?
            AND source = ? AND data_source = ?`,
        [id, prop, entry.source, normDs(entry.dataSource)],
      );
      if (rows.length > 0) {
        return pickValueByLang(
          rows.map((r) => ({
            value: r[0] as string,
            lang: (r[1] as string | null) ?? "",
          })),
          prefs,
        );
      }
    }
    return null;
  }

  /**
   * Get all attributes for id from the first source that has any.
   * Mirrors elot-db-get-all-attrs: first-source-wins, preserves
   * first-seen prop order, collapses multi-row props via the picker.
   */
  getAllAttrs(
    id: string,
    active: readonly ActiveSource[],
    prefs?: readonly LangPref[] | null,
  ): EntityAttrs | null {
    for (const entry of active) {
      const ds = normDs(entry.dataSource);
      const rows = this.select(
        `SELECT prop, value, lang FROM attributes
          WHERE id = ? AND source = ? AND data_source = ?`,
        [id, entry.source, ds],
      );
      if (rows.length === 0) continue;
      const byProp = new Map<string, LangRow[]>();
      const order: string[] = [];
      for (const r of rows) {
        const p = r[0] as string;
        const v = r[1] as string;
        const l = (r[2] as string | null) ?? "";
        if (!byProp.has(p)) {
          byProp.set(p, []);
          order.push(p);
        }
        byProp.get(p)!.push({ value: v, lang: l });
      }
      const entries: Array<[string, string]> = [];
      for (const p of order) {
        const picked = pickValueByLang(byProp.get(p)!, prefs);
        if (picked !== null) entries.push([p, picked]);
      }
      return {
        entries,
        sourceOrigin: { source: entry.source, dataSource: ds },
      };
    }
    return null;
  }

  // ─── bulk id / label helpers ──────────────────────────────────

  /**
   * Deduped id list across ACTIVE.  When includeCuries is true, each
   * URI-shaped id is additionally contracted and the resulting
   * CURIEs appended (URIs first, then CURIEs, de-duplicated).
   */
  allActiveIds(
    active: readonly ActiveSource[],
    includeCuries = false,
  ): string[] {
    if (active.length === 0) return [];
    const seen = new Set<string>();
    const out: string[] = [];
    for (const entry of active) {
      const rows = this.select(
        `SELECT DISTINCT id FROM entities
          WHERE source = ? AND data_source = ?`,
        [entry.source, normDs(entry.dataSource)],
      );
      for (const r of rows) {
        const id = r[0] as string;
        if (!seen.has(id)) {
          seen.add(id);
          out.push(id);
        }
      }
    }
    if (includeCuries) {
      const iris = [...out];
      for (const id of iris) {
        if (looksLikeUriP(id)) {
          for (const curie of this.contractUri(id, active)) {
            if (!seen.has(curie)) {
              seen.add(curie);
              out.push(curie);
            }
          }
        }
      }
    }
    return out;
  }

  /**
   * Map of label -> list of ids, first-source-wins (Step 1.15 shape).
   * Mirrors elot-db-all-active-labels, including the rdfs:label
   * attribute-row resolution via pickValueByLang.
   */
  allActiveLabels(
    active: readonly ActiveSource[],
    prefs?: readonly LangPref[] | null,
  ): Map<string, string[]> {
    const ht = new Map<string, string[]>();
    const claimed = new Set<string>();
    for (const entry of active) {
      const ds = normDs(entry.dataSource);
      const entRows = this.select(
        `SELECT DISTINCT id, label FROM entities
          WHERE source = ? AND data_source = ?
            AND label IS NOT NULL`,
        [entry.source, ds],
      );
      const lblRows = this.select(
        `SELECT id, value, lang FROM attributes
          WHERE source = ? AND data_source = ?
            AND prop = 'rdfs:label'`,
        [entry.source, ds],
      );
      const byId = new Map<string, LangRow[]>();
      for (const r of lblRows) {
        const id = r[0] as string;
        const row: LangRow = {
          value: r[1] as string,
          lang: (r[2] as string | null) ?? "",
        };
        if (!byId.has(id)) byId.set(id, []);
        byId.get(id)!.push(row);
      }
      const thisPass = new Set<string>();
      for (const r of entRows) {
        const id = r[0] as string;
        const entLabel = r[1] as string | null;
        const variants = byId.get(id);
        const label = variants
          ? pickValueByLang(variants, prefs)
          : entLabel;
        if (label != null && !claimed.has(label)) {
          const cur = ht.get(label);
          if (cur) cur.push(id);
          else ht.set(label, [id]);
          thisPass.add(label);
        }
      }
      for (const l of thisPass) claimed.add(l);
    }
    return ht;
  }

  /** Convenience: `allActiveLabels(...).get(label) ?? null`. */
  idsForLabel(
    label: string,
    active: readonly ActiveSource[],
    prefs?: readonly LangPref[] | null,
  ): string[] | null {
    const ht = this.allActiveLabels(active, prefs);
    return ht.get(label) ?? null;
  }

  /**
   * Return the list of (value, lang) rdfs:label rows for id in the
   * first active source that has any.  Mirrors elot-db-label-variants.
   */
  labelVariants(
    id: string,
    active: readonly ActiveSource[],
  ): LangRow[] | null {
    for (const entry of active) {
      const rows = this.select(
        `SELECT value, lang FROM attributes
          WHERE id = ? AND prop = 'rdfs:label'
            AND source = ? AND data_source = ?`,
        [id, entry.source, normDs(entry.dataSource)],
      );
      if (rows.length > 0) {
        return rows.map((r) => ({
          value: r[0] as string,
          lang: (r[1] as string | null) ?? "",
        }));
      }
    }
    return null;
  }
}

// Re-export the pure helpers for consumers that only need them.
export {
  pickValueByLang,
  selectByLanguage,
  effectiveLanguagePrefs,
  looksLikeUriP,
  looksLikeCurieP,
  UNTAGGED,
} from "./langPicker.js";
export type { LangPref, LangRow } from "./langPicker.js";
