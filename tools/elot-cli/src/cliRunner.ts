// src/cliRunner.ts
//
// Step 2.3.6: pure helpers for invoking `elot-cli db ...` from the
// extension.  No vscode import - tsx-testable.  The actual spawn
// lives in `registerSourceCommand.ts` (uses node:child_process and
// pipes output to a vscode output channel).

export interface CliInvocation {
  /** Command and args, in spawn-friendly array form. */
  args: string[];
  /** Human-readable rendering for the output channel header. */
  display: string;
}

export interface RegisterArgs {
  file: string;            // local path OR (for type=rq w/ http endpoint) the .rq file
  source: string;          // source name
  type?: string | null;    // csv | tsv | json | ttl | rq | org (omit -> auto-detect)
  dataSource?: string | null;
  dbPath?: string | null;  // when omitted, CLI uses its own default
}

export interface RefreshArgs {
  source: string;
  dataSource?: string | null;
  dbPath?: string | null;
}

function quoteForDisplay(s: string): string {
  return /[\s"'\\]/.test(s) ? `"${s.replace(/"/g, '\\"')}"` : s;
}

function renderDisplay(cli: string, args: string[]): string {
  return [cli, ...args].map(quoteForDisplay).join(" ");
}

export function buildRegisterInvocation(
  cli: string,
  a: RegisterArgs,
): CliInvocation {
  const args = ["db", "register", a.file, "--source", a.source];
  if (a.type) args.push("--type", a.type);
  if (a.dataSource) args.push("--data-source", a.dataSource);
  if (a.dbPath) args.push("--db", a.dbPath);
  return { args, display: renderDisplay(cli, args) };
}

export function buildRefreshInvocation(
  cli: string,
  a: RefreshArgs,
): CliInvocation {
  const args = ["db", "refresh", a.source];
  if (a.dataSource) args.push("--data-source", a.dataSource);
  if (a.dbPath) args.push("--db", a.dbPath);
  return { args, display: renderDisplay(cli, args) };
}

/**
 * Heuristic: derive a default source name from a file path.
 *
 *   /a/b/labels.csv -> "labels"
 *   /a/b/some.query.rq -> "some.query"
 *   foo            -> "foo"
 */
export function defaultSourceNameFromFile(file: string): string {
  const base = file.replace(/^.*[\\/]/, "");
  const dot = base.lastIndexOf(".");
  return dot > 0 ? base.slice(0, dot) : base;
}
