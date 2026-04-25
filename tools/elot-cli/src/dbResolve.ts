// src/dbResolve.ts
//
// Step 2.3.1: VS Code-side DB path resolution.
//
// Precedence (highest first):
//   1. workspace `elot.dbPath` setting
//   2. user      `elot.dbPath` setting
//   3. $ELOT_DB_PATH env var
//   4. <context.globalStorageUri.fsPath>/elot.sqlite
//
// Pure -- caller passes globalStorageDir; no vscode import so the
// helper is testable under tsx.  Differs from `dbPaths.ts` (the CLI
// equivalent) in that the extension uses VS Code's official global-
// storage API rather than reconstructing the path per-platform.

import { join } from "path";

export const ELOT_DB_FILENAME = "elot.sqlite";

export interface DbPathInputs {
  /** Workspace-level `elot.dbPath` (raw setting value). */
  workspacePath?: string | null;
  /** User-level `elot.dbPath`. */
  userPath?: string | null;
  /** `$ELOT_DB_PATH` env var. */
  envPath?: string | null;
  /** Extension's globalStorage directory (context.globalStorageUri.fsPath). */
  globalStorageDir: string;
}

/** Empty strings count as "unset". */
function nonEmpty(s: string | null | undefined): string | null {
  return typeof s === "string" && s.length > 0 ? s : null;
}

/**
 * Resolve the effective DB path the extension should open.  See
 * file header for precedence.  Returns an absolute or settings-
 * supplied path string; the caller is responsible for `existsSync`
 * checks before opening.
 */
export function resolveExtensionDbPath(inp: DbPathInputs): string {
  return (
    nonEmpty(inp.workspacePath) ??
    nonEmpty(inp.userPath) ??
    nonEmpty(inp.envPath) ??
    join(inp.globalStorageDir, ELOT_DB_FILENAME)
  );
}
