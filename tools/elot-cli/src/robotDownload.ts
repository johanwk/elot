// src/robotDownload.ts
//
// Auto-download of ROBOT (https://github.com/ontodev/robot) into the
// extension's globalStorageUri, mirroring the elot-exporter.jar flow
// implemented in importOwl.ts.
//
// Resolution order at runtime (matches src/parsers/robot.ts):
//   1. $ELOT_ROBOT_JAR              (set by ensureRobotJar below)
//   2. elot.robotJarPath setting    (a path to a user-managed robot.jar)
//   3. $ROBOT  /  `robot` on PATH   (handled inside parsers/robot.ts)
//
// Public API:
//   - jarPath(context)               -> absolute path the jar lives at
//   - jarExists(context)             -> boolean
//   - ensureRobotJar(context)        -> downloads if missing (prompts user)
//   - applyRobotJarEnv(context)      -> sets process.env.ELOT_ROBOT_JAR
//   - registerDownloadRobotCommand   -> elot.downloadRobot

import * as vscode from "vscode";
import { existsSync, mkdirSync } from "fs";
import { join } from "path";
import * as https from "https";
import * as http from "http";
import * as fs from "fs";

const JAR_FILENAME = "robot.jar";

/** GitHub API endpoint for the latest ROBOT release */
const GITHUB_LATEST_RELEASE_API =
  "https://api.github.com/repos/ontodev/robot/releases/latest";

// ── Helpers ─────────────────────────────────────────────────────────

export function jarPath(context: vscode.ExtensionContext): string {
  return join(context.globalStorageUri.fsPath, JAR_FILENAME);
}

export function jarExists(context: vscode.ExtensionContext): boolean {
  return existsSync(jarPath(context));
}

function ensureStorageDir(context: vscode.ExtensionContext): void {
  const dir = context.globalStorageUri.fsPath;
  if (!existsSync(dir)) mkdirSync(dir, { recursive: true });
}

function httpsGetJson(url: string): Promise<any> {
  return new Promise((resolve, reject) => {
    const get = (u: string) => {
      https
        .get(u, { headers: { "User-Agent": "vscode-elot" } }, (res) => {
          if (
            res.statusCode &&
            res.statusCode >= 300 &&
            res.statusCode < 400 &&
            res.headers.location
          ) {
            get(res.headers.location);
            return;
          }
          if (res.statusCode !== 200) {
            reject(new Error(`HTTP ${res.statusCode} for ${u}`));
            return;
          }
          let data = "";
          res.on("data", (chunk: string) => (data += chunk));
          res.on("end", () => {
            try {
              resolve(JSON.parse(data));
            } catch (e) {
              reject(e);
            }
          });
        })
        .on("error", reject);
    };
    get(url);
  });
}

function downloadFile(
  url: string,
  dest: string,
  onProgress?: (bytes: number) => void,
): Promise<void> {
  return new Promise((resolve, reject) => {
    const get = (u: string) => {
      const mod = u.startsWith("https") ? https : http;
      (mod as typeof https)
        .get(u, { headers: { "User-Agent": "vscode-elot" } }, (res) => {
          if (
            res.statusCode &&
            res.statusCode >= 300 &&
            res.statusCode < 400 &&
            res.headers.location
          ) {
            get(res.headers.location);
            return;
          }
          if (res.statusCode !== 200) {
            reject(new Error(`HTTP ${res.statusCode} downloading ${u}`));
            return;
          }
          const file = fs.createWriteStream(dest);
          let received = 0;
          res.on("data", (chunk: Buffer) => {
            received += chunk.length;
            if (onProgress) onProgress(received);
          });
          res.pipe(file);
          file.on("finish", () => file.close(() => resolve()));
          file.on("error", (err) => {
            try {
              fs.unlinkSync(dest);
            } catch {
              /* ignore */
            }
            reject(err);
          });
        })
        .on("error", reject);
    };
    get(url);
  });
}

// ── GitHub release lookup ──────────────────────────────────────────

async function findLatestJarUrl(): Promise<{ url: string; tag: string }> {
  const release = await httpsGetJson(GITHUB_LATEST_RELEASE_API);
  const assets: any[] = release.assets ?? [];
  // ROBOT releases ship `robot.jar` as a top-level asset.
  const jarAsset = assets.find(
    (a: any) =>
      typeof a.name === "string" &&
      a.name.toLowerCase() === "robot.jar",
  );
  if (!jarAsset) {
    throw new Error(
      `No robot.jar asset found in ROBOT release ${release.tag_name}`,
    );
  }
  return { url: jarAsset.browser_download_url, tag: release.tag_name };
}

async function downloadJar(context: vscode.ExtensionContext): Promise<void> {
  const { url, tag } = await findLatestJarUrl();
  ensureStorageDir(context);
  const dest = jarPath(context);

  await vscode.window.withProgress(
    {
      location: vscode.ProgressLocation.Notification,
      title: `Downloading ROBOT (${tag})…`,
      cancellable: false,
    },
    async (progress) => {
      await downloadFile(url, dest + ".tmp", (bytes) => {
        progress.report({
          message: `${(bytes / (1024 * 1024)).toFixed(1)} MB`,
        });
      });
      fs.renameSync(dest + ".tmp", dest);
    },
  );
}

// ── User-facing helpers ────────────────────────────────────────────

/**
 * Ensure ROBOT is available.  Resolution order:
 *   1. elot.robotJarPath setting (if set and file exists) — used as-is.
 *   2. Bundled-download jar in globalStorage — created on demand
 *      (prompts the user before downloading).
 *
 * Returns the absolute jar path on success, or undefined if the user
 * declined the download / the download failed.  When this function
 * returns a path, $ELOT_ROBOT_JAR is also set so any child process
 * (CLI, tests) inherits it.
 */
export async function ensureRobotJar(
  context: vscode.ExtensionContext,
): Promise<string | undefined> {
  // 1. Honour an explicit user setting first.
  const cfgPath = (
    vscode.workspace.getConfiguration("elot").get<string>("robotJarPath") ?? ""
  ).trim();
  if (cfgPath.length > 0) {
    if (existsSync(cfgPath)) {
      process.env.ELOT_ROBOT_JAR = cfgPath;
      return cfgPath;
    }
    vscode.window.showWarningMessage(
      `elot.robotJarPath points at '${cfgPath}' which does not exist; ` +
        `falling back to auto-download.`,
    );
  }

  // 2. Use existing globalStorage jar.
  if (jarExists(context)) {
    process.env.ELOT_ROBOT_JAR = jarPath(context);
    return jarPath(context);
  }

  // 3. Prompt to download.
  const choice = await vscode.window.showInformationMessage(
    "ROBOT (robot.jar) is required to read .ttl / .rq label sources but " +
      "is not yet downloaded. Download the latest release from GitHub?",
    "Download",
    "Cancel",
  );
  if (choice !== "Download") return undefined;

  try {
    await downloadJar(context);
    const p = jarPath(context);
    process.env.ELOT_ROBOT_JAR = p;
    vscode.window.showInformationMessage(`ROBOT downloaded to ${p}`);
    return p;
  } catch (err: any) {
    vscode.window.showErrorMessage(
      `Failed to download robot.jar: ${err.message}`,
    );
    return undefined;
  }
}

/**
 * If ROBOT is already available locally (downloaded jar OR
 * elot.robotJarPath setting), set $ELOT_ROBOT_JAR for in-process
 * helpers and child CLIs.  Never prompts and never downloads.
 *
 * Call this from `activate()` so that the bundled CLI subprocess
 * inherits the right env on its very first invocation.
 */
export function applyRobotJarEnv(context: vscode.ExtensionContext): void {
  const cfgPath = (
    vscode.workspace.getConfiguration("elot").get<string>("robotJarPath") ?? ""
  ).trim();
  if (cfgPath.length > 0 && existsSync(cfgPath)) {
    process.env.ELOT_ROBOT_JAR = cfgPath;
    return;
  }
  if (jarExists(context)) {
    process.env.ELOT_ROBOT_JAR = jarPath(context);
  }
}

/**
 * Register `elot.downloadRobot` — explicit (re-)download of robot.jar.
 */
export function registerDownloadRobotCommand(
  context: vscode.ExtensionContext,
): vscode.Disposable {
  return vscode.commands.registerCommand("elot.downloadRobot", async () => {
    try {
      await downloadJar(context);
      const p = jarPath(context);
      process.env.ELOT_ROBOT_JAR = p;
      vscode.window.showInformationMessage(`ROBOT downloaded to ${p}`);
    } catch (err: any) {
      vscode.window.showErrorMessage(
        `Failed to download robot.jar: ${err.message}`,
      );
    }
  });
}
