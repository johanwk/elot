// src/dbPaths.ts
//
// Resolve the default ElotDb path using the same precedence order
// documented in ELOT-DB-PLAN Step 2.2:
//   1. explicit --db argument (handled by callers)
//   2. $ELOT_DB_PATH env var
//   3. VS Code global-storage directory per platform
//   4. fallback ~/.elot/elot.sqlite
//
// Pure helpers; no filesystem mutation.

import { homedir, platform } from "os";
import { join } from "path";

const VSCODE_EXT_ID = "johanwk.elot";
const DB_FILENAME = "elot.sqlite";

/** Per-platform VS Code globalStorage path for the ELOT extension. */
export function vscodeGlobalStorageDir(
  env: NodeJS.ProcessEnv = process.env,
  plat: NodeJS.Platform = platform(),
): string {
  const home = homedir();
  switch (plat) {
    case "win32": {
      const appData = env.APPDATA ?? join(home, "AppData", "Roaming");
      return join(appData, "Code", "User", "globalStorage", VSCODE_EXT_ID);
    }
    case "darwin":
      return join(
        home,
        "Library",
        "Application Support",
        "Code",
        "User",
        "globalStorage",
        VSCODE_EXT_ID,
      );
    default:
      return join(
        home,
        ".config",
        "Code",
        "User",
        "globalStorage",
        VSCODE_EXT_ID,
      );
  }
}

/**
 * Resolve the effective DB path.  ENV overrides the global-storage
 * default; falls back to ~/.elot/elot.sqlite when the platform
 * cannot be determined (defensive; in practice always returns
 * before this).
 */
export function resolveDefaultDbPath(
  env: NodeJS.ProcessEnv = process.env,
  plat: NodeJS.Platform = platform(),
): string {
  const fromEnv = env.ELOT_DB_PATH;
  if (fromEnv && fromEnv.length > 0) return fromEnv;
  try {
    return join(vscodeGlobalStorageDir(env, plat), DB_FILENAME);
  } catch {
    return join(homedir(), ".elot", DB_FILENAME);
  }
}
