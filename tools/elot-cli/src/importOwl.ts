// src/importOwl.ts
//
// Import an OWL ontology by running elot-exporter.jar, mirroring the
// Emacs `elot-open-owl` functionality.
//
// - Downloads the latest elot-exporter.jar from GitHub Releases if not
//   already present in VS Code's globalStorageUri.
// - Prompts the user for a local file or URL, runs the jar, prompts
//   for a save location (.org), and opens the saved file.

import * as vscode from "vscode";
import { execFile } from "child_process";
import { existsSync, mkdirSync } from "fs";
import { join, basename } from "path";
import * as https from "https";
import * as http from "http";
import * as fs from "fs";
import * as os from "os";

/** The filename we store inside globalStorageUri */
const JAR_FILENAME = "elot-exporter.jar";

/** GitHub API endpoint for the latest release */
const GITHUB_LATEST_RELEASE_API =
  "https://api.github.com/repos/johanwk/elot/releases/latest";

// ── Helpers ─────────────────────────────────────────────────────────

/** Return the path where the jar is (or will be) stored. */
function jarPath(context: vscode.ExtensionContext): string {
  return join(context.globalStorageUri.fsPath, JAR_FILENAME);
}

/** True when the jar file exists on disk. */
function jarExists(context: vscode.ExtensionContext): boolean {
  return existsSync(jarPath(context));
}

/** Ensure the globalStorage directory exists. */
function ensureStorageDir(context: vscode.ExtensionContext): void {
  const dir = context.globalStorageUri.fsPath;
  if (!existsSync(dir)) {
    mkdirSync(dir, { recursive: true });
  }
}

/**
 * Perform a simple HTTPS GET that follows redirects and returns the
 * response body as a string.  Used for the GitHub API call.
 */
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

/**
 * Download a binary file from `url` to `dest`, following redirects.
 * Reports progress via `onProgress(bytesReceived)`.
 */
function downloadFile(
  url: string,
  dest: string,
  onProgress?: (bytes: number) => void
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
            fs.unlinkSync(dest);
            reject(err);
          });
        })
        .on("error", reject);
    };
    get(url);
  });
}

// ── Download logic ──────────────────────────────────────────────────

/**
 * Query the GitHub Releases API for the latest release and find the
 * elot-exporter.jar asset download URL.
 */
async function findLatestJarUrl(): Promise<{ url: string; tag: string }> {
  const release = await httpsGetJson(GITHUB_LATEST_RELEASE_API);
  const assets: any[] = release.assets ?? [];
  const jarAsset = assets.find(
    (a: any) =>
      typeof a.name === "string" &&
      a.name.startsWith("elot-exporter") &&
      a.name.endsWith(".jar")
  );
  if (!jarAsset) {
    throw new Error(
      `No elot-exporter*.jar asset found in release ${release.tag_name}`
    );
  }
  return { url: jarAsset.browser_download_url, tag: release.tag_name };
}

/**
 * Download elot-exporter.jar with a progress notification.
 */
async function downloadJar(context: vscode.ExtensionContext): Promise<void> {
  const { url, tag } = await findLatestJarUrl();

  ensureStorageDir(context);
  const dest = jarPath(context);

  await vscode.window.withProgress(
    {
      location: vscode.ProgressLocation.Notification,
      title: `Downloading elot-exporter.jar (${tag})…`,
      cancellable: false,
    },
    async (progress) => {
      await downloadFile(url, dest + ".tmp", (bytes) => {
        progress.report({
          message: `${(bytes / (1024 * 1024)).toFixed(1)} MB`,
        });
      });
      // Rename atomically
      fs.renameSync(dest + ".tmp", dest);
    }
  );
}

// ── Ensure jar is available (prompt if missing) ─────────────────────

async function ensureJar(
  context: vscode.ExtensionContext
): Promise<string | undefined> {
  if (jarExists(context)) {
    return jarPath(context);
  }

  const choice = await vscode.window.showInformationMessage(
    "elot-exporter.jar is required to import OWL ontologies but is not yet downloaded. Download the latest release from GitHub?",
    "Download",
    "Cancel"
  );

  if (choice !== "Download") {
    return undefined;
  }

  try {
    await downloadJar(context);
    vscode.window.showInformationMessage(
      `elot-exporter.jar downloaded to ${jarPath(context)}`
    );
    return jarPath(context);
  } catch (err: any) {
    vscode.window.showErrorMessage(
      `Failed to download elot-exporter.jar: ${err.message}`
    );
    return undefined;
  }
}

// ── Run elot-exporter ───────────────────────────────────────────────

/**
 * Run `java -jar elot-exporter.jar <source>` and return stdout.
 */
function runExporter(jar: string, source: string): Promise<string> {
  const javaPath =
    vscode.workspace.getConfiguration("elot").get<string>("javaPath") || "java";
  return new Promise((resolve, reject) => {
    execFile(
      javaPath,
      ["-jar", jar, source],
      { maxBuffer: 50 * 1024 * 1024, encoding: "utf-8" },
      (err, stdout, stderr) => {
        if (err) {
          reject(
            new Error(
              `elot-exporter failed (exit ${err.code}):\n${stderr || err.message}`
            )
          );
          return;
        }
        if (stderr) {
          // elot-exporter writes statistics to stderr; not an error
          console.log("[elot-exporter stderr]", stderr);
        }
        resolve(stdout);
      }
    );
  });
}

// ── Public command ──────────────────────────────────────────────────

/**
 * Register the `elot.importOwl` command.
 *
 * Flow:
 *   1. Ensure elot-exporter.jar is present (download if needed).
 *   2. Ask the user for an OWL source (file path or URL).
 *   3. Run the exporter, prompt for a save location (.org), and open.
 */
export function registerImportOwlCommand(
  context: vscode.ExtensionContext
): vscode.Disposable {
  return vscode.commands.registerCommand("elot.importOwl", async () => {
    // 1. Ensure jar
    const jar = await ensureJar(context);
    if (!jar) return;

    // 2. Ask for source — offer file picker or manual URL entry
    const method = await vscode.window.showQuickPick(
      [
        { label: "$(globe) Enter a URL", value: "url" },
        { label: "$(file) Browse for a local file", value: "file" },
      ],
      { placeHolder: "Import OWL ontology — choose source" }
    );
    if (!method) return;

    let owlSource: string | undefined;

    if (method.value === "url") {
      owlSource = await vscode.window.showInputBox({
        prompt: "Enter the URL of the OWL ontology",
        placeHolder: "https://example.org/ontology.owl",
        validateInput: (v) =>
          v && /^https?:\/\/.+/.test(v) ? null : "Please enter a valid URL",
      });
    } else {
      const uris = await vscode.window.showOpenDialog({
        canSelectMany: false,
        filters: {
          "OWL Ontology": [
            "owl",
            "rdf",
            "xml",
            "ttl",
            "omn",
            "ofn",
            "owx",
          ],
          "All files": ["*"],
        },
        openLabel: "Import OWL",
      });
      if (uris && uris.length > 0) {
        owlSource = uris[0].fsPath;
      }
    }

    if (!owlSource) return;

    // 3. Run elot-exporter with progress
    try {
      const orgContent = await vscode.window.withProgress(
        {
          location: vscode.ProgressLocation.Notification,
          title: `Importing OWL ontology…`,
          cancellable: false,
        },
        async () => {
          return await runExporter(jar, owlSource!);
        }
      );

      // Derive a default filename from the :ID: property of the
      // top-level heading.  We look for a PROPERTIES drawer line like
      //   :ID:       core
      // and use that value.  Fall back to the first top-level heading
      // text, then to the source filename.
      let defaultBase: string | undefined;

      const idPropMatch = orgContent.match(
        /^:ID:\s+(\S+)\s*$/m
      );
      if (idPropMatch) {
        defaultBase = idPropMatch[1];
      } else {
        const headingMatch = orgContent.match(/^\*\s+(\S+)/m);
        if (headingMatch) {
          defaultBase = headingMatch[1];
        }
      }

      if (!defaultBase) {
        // Fall back to the source filename without extension
        const sourceName = owlSource.startsWith("http")
          ? decodeURIComponent(
              basename(new URL(owlSource).pathname) || "ontology"
            )
          : basename(owlSource);
        defaultBase = sourceName.replace(/\.[^.]+$/, "");
      }
      // Strip any leftover extension from the base name
      defaultBase = defaultBase.replace(/\.[^.]+$/, "");
      const defaultName = defaultBase + ".org";

      // Prompt the user for a save location so the file gets a
      // proper .org extension and ELOT features activate immediately.
      const defaultDir =
        vscode.workspace.workspaceFolders?.[0]?.uri.fsPath ??
        os.homedir();
      const defaultUri = vscode.Uri.file(join(defaultDir, defaultName));

      const saveUri = await vscode.window.showSaveDialog({
        defaultUri,
        filters: { "Org file": ["org"], "All files": ["*"] },
        title: "Save imported ontology as .org",
      });

      if (saveUri) {
        // Write to disk and open the saved file — the .org extension
        // ensures that all ELOT features activate automatically.
        await vscode.workspace.fs.writeFile(
          saveUri,
          Buffer.from(orgContent, "utf-8")
        );
        const doc = await vscode.workspace.openTextDocument(saveUri);
        await vscode.window.showTextDocument(doc);
        vscode.window.showInformationMessage(
          `Imported OWL ontology → ${basename(saveUri.fsPath)}`
        );
      } else {
        // User cancelled the save dialog — open as untitled so work
        // is not lost, but warn that ELOT features need .org extension.
        const doc = await vscode.workspace.openTextDocument({
          content: orgContent,
          language: "org",
        });
        await vscode.window.showTextDocument(doc);
        vscode.window.showWarningMessage(
          `Ontology opened as unsaved buffer. Save as "${defaultName}" to enable all ELOT features.`
        );
      }
    } catch (err: any) {
      vscode.window.showErrorMessage(`Import failed: ${err.message}`);
    }
  });
}

/**
 * Register the `elot.downloadExporter` command — allows downloading
 * (or re-downloading) elot-exporter.jar explicitly.
 */
export function registerDownloadExporterCommand(
  context: vscode.ExtensionContext
): vscode.Disposable {
  return vscode.commands.registerCommand(
    "elot.downloadExporter",
    async () => {
      try {
        await downloadJar(context);
        vscode.window.showInformationMessage(
          `elot-exporter.jar downloaded to ${jarPath(context)}`
        );
      } catch (err: any) {
        vscode.window.showErrorMessage(
          `Failed to download elot-exporter.jar: ${err.message}`
        );
      }
    }
  );
}
