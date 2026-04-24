// src/dbCli.ts
//
// Minimal CLI skeleton for the ElotDb sub-commands (Step 2.2.2).
// Wired under `elot-cli db <subcommand>` so it coexists with the
// existing org-to-OMN entrypoint in src/cli.ts.
//
// Subcommands shipped in 2.2.2:
//   init, list, lookup, attr, remove, register (JSON triples only)
// Full parsers (CSV/TSV/JSON-nested/TTL/RQ/Org) land in 2.2.3+.

import { Command } from "commander";
import { readFileSync, statSync, existsSync } from "fs";
import { resolve } from "path";
import {
  ElotDb,
  ActiveSource,
  EntityTriple,
  AttrValue,
} from "./db/sqljs.js";
import { resolveDefaultDbPath } from "./dbPaths.js";

function parseActiveSpec(spec: string | undefined): ActiveSource[] {
  // "src1,src2|dsA,src3" -- comma-separated source entries,
  // each optionally suffixed with "|dataSource".  Empty spec = [].
  if (!spec) return [];
  return spec
    .split(",")
    .map((part) => part.trim())
    .filter(Boolean)
    .map((part) => {
      const pipe = part.indexOf("|");
      if (pipe < 0) return { source: part };
      return { source: part.slice(0, pipe), dataSource: part.slice(pipe + 1) };
    });
}

async function openDb(opts: { db?: string }): Promise<ElotDb> {
  const path = opts.db ?? resolveDefaultDbPath();
  return ElotDb.open(path);
}

/**
 * Register JSON-triples input.  Shape (minimal; parsers land later):
 *   {
 *     "source": "my-source",
 *     "dataSource": "",
 *     "type": "json",
 *     "data": [
 *       { "id": "ex:x", "label": "X",
 *         "kind": "curie",
 *         "attrs": [
 *           ["rdf:type", "owl:Class"],
 *           ["rdfs:label", {"value": "X", "lang": "en"}]
 *         ]
 *       }
 *     ],
 *     "prefixes": [ ["ex", "http://example.org/"] ]
 *   }
 */
interface TriplesJson {
  source: string;
  dataSource?: string;
  type?: string;
  data: EntityTriple[];
  prefixes?: Array<[string, string]>;
}

export function buildDbCommand(): Command {
  const db = new Command("db").description("Elot DB management");

  db.option(
    "--db <path>",
    "Database file path (overrides $ELOT_DB_PATH and default)",
  );

  db.command("init")
    .description("Create a fresh DB (or verify schema v3 on an existing file)")
    .action(async (_args, cmd) => {
      const opts = cmd.optsWithGlobals();
      const path = opts.db ?? resolveDefaultDbPath();
      const existedBefore = existsSync(path);
      const d = await ElotDb.open(path);
      if (!existedBefore) d.save(path);
      d.close();
      console.log(existedBefore ? `ok: ${path}` : `created: ${path}`);
    });

  db.command("list")
    .description("List registered sources (or prefixes with --prefixes)")
    .option("--prefixes", "List prefixes instead of sources")
    .action(async (opts, cmd) => {
      const gopts = cmd.optsWithGlobals();
      const d = await openDb(gopts);
      try {
        if (opts.prefixes) {
          for (const p of d.listPrefixes()) {
            console.log(
              `${p.source}\t${p.dataSource}\t${p.prefix}\t${p.expansion}`,
            );
          }
        } else {
          for (const s of d.listSources()) {
            console.log(
              `${s.source}\t${s.dataSource}\t${s.type ?? ""}\t${s.lastModified ?? ""}`,
            );
          }
        }
      } finally {
        d.close();
      }
    });

  db.command("lookup <label>")
    .description("Print ids for LABEL across active sources")
    .option(
      "--active <spec>",
      "Active sources, comma-separated (src|ds form supported)",
    )
    .action(async (label: string, opts, cmd) => {
      const gopts = cmd.optsWithGlobals();
      const d = await openDb(gopts);
      try {
        const active = parseActiveSpec(opts.active) ?? [];
        const ids = d.idsForLabel(
          label,
          active.length > 0
            ? active
            : d.listSources().map((s) => ({
                source: s.source,
                dataSource: s.dataSource,
              })),
        );
        if (!ids) {
          process.exit(1);
        }
        for (const id of ids) console.log(id);
      } finally {
        d.close();
      }
    });

  db.command("attr <id> [prop]")
    .description("Print attribute(s) for ID; honours language prefs")
    .option("--active <spec>", "Active sources (see `lookup`)")
    .action(async (id: string, prop: string | undefined, opts, cmd) => {
      const gopts = cmd.optsWithGlobals();
      const d = await openDb(gopts);
      try {
        const active =
          parseActiveSpec(opts.active) ??
          d.listSources().map((s) => ({
            source: s.source,
            dataSource: s.dataSource,
          }));
        const activeList =
          active.length > 0
            ? active
            : d.listSources().map((s) => ({
                source: s.source,
                dataSource: s.dataSource,
              }));
        if (prop) {
          const v = d.getAttr(id, prop, activeList);
          if (v == null) process.exit(1);
          console.log(v);
        } else {
          const a = d.getAllAttrs(id, activeList);
          if (!a) process.exit(1);
          for (const [p, v] of a.entries) console.log(`${p}\t${v}`);
        }
      } finally {
        d.close();
      }
    });

  db.command("remove <source>")
    .description("Remove SOURCE (and its data_source, if supplied)")
    .option("--data-source <ds>", "Specific data_source (default: all)")
    .action(async (source: string, opts, cmd) => {
      const gopts = cmd.optsWithGlobals();
      const path = gopts.db ?? resolveDefaultDbPath();
      const d = await ElotDb.open(path);
      try {
        const ok = d.removeSource(source, opts.dataSource ?? "");
        if (ok) d.save(path);
        console.log(ok ? `removed: ${source}` : `not-found: ${source}`);
        if (!ok) process.exit(1);
      } finally {
        d.close();
      }
    });

  db.command("register <file>")
    .description(
      "Register a source from a JSON-triples file (minimal writer path; real parsers in 2.2.3+)",
    )
    .action(async (file: string, _opts, cmd) => {
      const gopts = cmd.optsWithGlobals();
      const path = gopts.db ?? resolveDefaultDbPath();
      const abs = resolve(file);
      const doc = JSON.parse(readFileSync(abs, "utf-8")) as TriplesJson;
      let mtime: number | null = null;
      try {
        mtime = statSync(abs).mtimeMs / 1000;
      } catch {
        /* ignore */
      }
      const d = await ElotDb.open(path);
      try {
        const n = d.updateSource(
          doc.source,
          doc.dataSource ?? "",
          doc.type ?? "json",
          doc.data,
          mtime,
        );
        if (doc.prefixes) {
          for (const [p, e] of doc.prefixes) {
            d.addPrefix(doc.source, doc.dataSource ?? "", p, e);
          }
        }
        d.save(path);
        console.log(`registered: ${doc.source} (${n} entities)`);
      } finally {
        d.close();
      }
    });

  return db;
}

// Stand-alone entry so `tsx src/dbCli.ts ...` works during dev.
// The main cli.ts wires this under the `db` sub-command too.
if (require.main === module) {
  const program = new Command()
    .name("elot-cli-db")
    .description("Elot DB management (dev entrypoint)")
    .addCommand(buildDbCommand());
  program.parseAsync(process.argv);
}

// Avoid unused-import warnings on AttrValue (re-exported for users).
export type { AttrValue };
