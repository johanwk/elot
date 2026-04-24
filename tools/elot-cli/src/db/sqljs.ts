// src/db/sqljs.ts
//
// ElotDb: sql.js-backed port of the elot-db Elisp API.
// Step 2.2.1 added the read surface and helpers.
// Step 2.2.2 adds the writer (updateSource / removeSource /
// addPrefix / removePrefix) and the full-file save() path.
//
// Mirrors elot-package/elot-db.el function-for-function where sensible.
// Schema is loaded verbatim from elot-package/schema.sql (Step 2.1);
// on an existing DB, schema_version is asserted to equal 3.

import { readFileSync, writeFileSync, existsSync, mkdirSync } from "fs";
import { resolve, dirname } from "path";
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

/**
 * Tagged or plain attribute value.  A plain string means lang='',
 * the tagged form carries an explicit language tag (may be empty).
 */
export type AttrValue =
  | string
  | { value: string; lang?: string | null };

/** Input row for updateSource: (id, label, kind?, attrs). */
export interface EntityTriple {
  id: string;
  label: string | null;
  /** Written to entities.kind; defaults to 'unknown'. */
  kind?: string;
  /** Ordered [prop, value] pairs; values may be tagged (see AttrValue). */
  attrs?: Array<[string, AttrValue]>;
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
 * ElotDb over sql.js.  Load with ElotDb.open(path).  Step 2.2.2
 * added the writer surface; use save() to persist mutations.
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

  /**
   * Write the current in-memory DB to disk at PATH (or the path
   * passed to open()).  sql.js is in-memory only; the CLI is the
   * sole writer, so each mutating command follows with a save().
   * Creates parent directories as needed.
   */
  save(path?: string | null): string {
    const target = path ?? this.path;
    if (!target) {
      throw new Error(
        "ElotDb.save: no path supplied and instance was opened in-memory",
      );
    }
    mkdirSync(dirname(target), { recursive: true });
    const bytes = this.db.export();
    writeFileSync(target, Buffer.from(bytes));
    return target;
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
   * Two-pass attribute lookup, parallel to getLabelAny.  Tries TOKEN
   * as a literal id, then CURIE-expanded, then URI-contracted.
   * Not present in Elisp (which uses Emacs-side resolution); added
   * for CLI / VS Code convenience so `attr` works symmetrically
   * with `lookup` regardless of whether the DB stores full URIs
   * or CURIEs for a given entity.
   */
  getAttrAny(
    token: string,
    prop: string,
    active: readonly ActiveSource[],
    prefs?: readonly LangPref[] | null,
  ): string | null {
    const direct = this.getAttr(token, prop, active, prefs);
    if (direct !== null) return direct;
    if (looksLikeCurieP(token)) {
      const uri = this.expandCurie(token, active);
      if (uri) {
        const hit = this.getAttr(uri, prop, active, prefs);
        if (hit !== null) return hit;
      }
    }
    if (looksLikeUriP(token)) {
      for (const curie of this.contractUri(token, active)) {
        const hit = this.getAttr(curie, prop, active, prefs);
        if (hit !== null) return hit;
      }
    }
    return null;
  }

  /**
   * Two-pass all-attrs lookup, parallel to getAttrAny.  Returns the
   * first non-null EntityAttrs across literal/expanded/contracted
   * token forms.
   */
  getAllAttrsAny(
    token: string,
    active: readonly ActiveSource[],
    prefs?: readonly LangPref[] | null,
  ): EntityAttrs | null {
    const direct = this.getAllAttrs(token, active, prefs);
    if (direct !== null) return direct;
    if (looksLikeCurieP(token)) {
      const uri = this.expandCurie(token, active);
      if (uri) {
        const hit = this.getAllAttrs(uri, active, prefs);
        if (hit !== null) return hit;
      }
    }
    if (looksLikeUriP(token)) {
      for (const curie of this.contractUri(token, active)) {
        const hit = this.getAllAttrs(curie, active, prefs);
        if (hit !== null) return hit;
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

  // ─── writers ──────────────────────────────────────────────────

  /**
   * Upsert a prefix row.  Mirrors elot-db-add-prefix.  DATA-SOURCE
   * may be null; the empty-string sentinel is stored.  PREFIX may
   * be the empty string (default ':' prefix).
   */
  addPrefix(
    source: string,
    dataSource: string | null | undefined,
    prefix: string,
    expansion: string,
  ): void {
    this.db.run(
      `INSERT OR REPLACE INTO prefixes (source, data_source, prefix, expansion)
       VALUES (?, ?, ?, ?)`,
      [source, normDs(dataSource), prefix, expansion],
    );
  }

  /**
   * Delete a prefix row.  Returns true if a row was removed.
   * Mirrors the implicit semantics of the Emacs UI (no dedicated
   * remove-prefix entry point in Elisp; done via raw SQL or by
   * removing the owning source).
   */
  removePrefix(
    source: string,
    dataSource: string | null | undefined,
    prefix: string,
  ): boolean {
    const ds = normDs(dataSource);
    const before = this.select(
      `SELECT 1 FROM prefixes
        WHERE source = ? AND data_source = ? AND prefix = ? LIMIT 1`,
      [source, ds, prefix],
    );
    if (before.length === 0) return false;
    this.db.run(
      `DELETE FROM prefixes
        WHERE source = ? AND data_source = ? AND prefix = ?`,
      [source, ds, prefix],
    );
    return true;
  }

  /**
   * Delete the sources row for (source, dataSource) and cascade
   * (entities / attributes / prefixes via ON DELETE CASCADE).
   * Mirrors elot-db-remove-source.  Returns true if a row was
   * removed, false if the source was unknown.
   */
  removeSource(
    source: string,
    dataSource?: string | null,
  ): boolean {
    const ds = normDs(dataSource);
    if (!this.sourceExistsP(source, ds)) return false;
    this.db.run(
      `DELETE FROM sources WHERE source = ? AND data_source = ?`,
      [source, ds],
    );
    return true;
  }

  /**
   * Replace DB records for (source, dataSource) with DATA.  Mirrors
   * elot-db-update-source one-to-one:
   *  - Runs inside a single transaction (sql.js supports BEGIN/
   *    COMMIT/ROLLBACK via db.run).
   *  - Existing sources row + cascaded children are deleted first.
   *  - DATA is coalesced by id: first non-empty label wins; attrs
   *    are concatenated in first-seen order; kind is taken from
   *    the first occurrence.
   *  - rdfs:label attribute rows are language-picked to produce a
   *    deterministic denormalised entities.label (Step 1.16 fix).
   *  - Tagged values {value, lang} write to attributes.lang;
   *    bare-string values write lang=''.
   *  - Returns the number of entity rows written.
   */
  updateSource(
    source: string,
    dataSource: string | null | undefined,
    type: string,
    data: readonly EntityTriple[],
    fileMtime?: number | null,
    prefs?: readonly LangPref[] | null,
  ): number {
    const ds = normDs(dataSource);
    const mtime = fileMtime ?? 0.0;
    const now = Date.now() / 1000;

    this.db.run("BEGIN");
    try {
      this.db.run(
        `DELETE FROM sources WHERE source = ? AND data_source = ?`,
        [source, ds],
      );
      this.db.run(
        `INSERT INTO sources (source, data_source, type, last_modified, last_updated)
         VALUES (?, ?, ?, ?, ?)`,
        [source, ds, type, mtime, now],
      );

      // Coalesce by id: first label wins (unless empty), attrs concat,
      // kind from first occurrence.
      interface Merged {
        label: string | null;
        kind: string | undefined;
        attrs: Array<[string, AttrValue]>;
      }
      const merged = new Map<string, Merged>();
      const order: string[] = [];
      for (const row of data) {
        const prev = merged.get(row.id);
        if (prev) {
          const useLabel =
            prev.label == null || prev.label === ""
              ? row.label
              : prev.label;
          prev.label = useLabel;
          if (prev.kind === undefined && row.kind !== undefined) {
            prev.kind = row.kind;
          }
          if (row.attrs) prev.attrs.push(...row.attrs);
        } else {
          order.push(row.id);
          merged.set(row.id, {
            label: row.label,
            kind: row.kind,
            attrs: row.attrs ? [...row.attrs] : [],
          });
        }
      }

      let n = 0;
      for (const id of order) {
        const cell = merged.get(id)!;
        const kind = cell.kind ?? "unknown";
        // rdfs:label variants drive the denormalised entities.label.
        const labelVariants: LangRow[] = [];
        for (const [prop, val] of cell.attrs) {
          if (prop === "rdfs:label") {
            if (typeof val === "string") {
              labelVariants.push({ value: val, lang: "" });
            } else {
              labelVariants.push({
                value: val.value,
                lang: val.lang ?? "",
              });
            }
          }
        }
        const picked =
          labelVariants.length > 0
            ? pickValueByLang(labelVariants, prefs)
            : null;
        const label = picked ?? cell.label;

        this.db.run(
          `INSERT OR REPLACE INTO entities (id, label, source, data_source, kind)
           VALUES (?, ?, ?, ?, ?)`,
          [id, label, source, ds, kind],
        );

        for (const [prop, val] of cell.attrs) {
          const v = typeof val === "string" ? val : val.value;
          const lang =
            typeof val === "string" ? "" : val.lang ?? "";
          this.db.run(
            `INSERT INTO attributes (id, source, data_source, prop, value, lang)
             VALUES (?, ?, ?, ?, ?, ?)`,
            [id, source, ds, prop, v, lang],
          );
        }
        n++;
      }

      this.db.run("COMMIT");
      return n;
    } catch (e) {
      try {
        this.db.run("ROLLBACK");
      } catch {
        /* swallow */
      }
      throw e;
    }
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
