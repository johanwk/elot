import * as vscode from "vscode";
import { writeFileSync } from "fs";
import { resolve, dirname, basename } from "path";
import { parseOrg } from "./parseOrgWasm.js";
import { generateFullOmn } from "./generateOmn.js";
import { registerHoverProvider, clearSlurpCache } from "./hoverProvider.js";
import { registerLabelDecorations } from "./labelDecorations.js";
import { registerFoldingProvider } from "./foldingProvider.js";
import { registerOrgIndent } from "./orgIndent.js";
import { registerDescriptionListDecorations } from "./descriptionListDecorations.js";
import { registerHeadlineBoldDecorations } from "./headlineBoldDecorations.js";
import { registerDefinitionProvider } from "./definitionProvider.js";
import { registerCompletionProvider } from "./completionProvider.js";
import { registerDiagnosticsProvider } from "./diagnosticsProvider.js";
import { registerImportOwlCommand, registerDownloadExporterCommand } from "./importOwl.js";
import { ensurePandoc, exportOrgToHtml } from "./exportHtml.js";
import { ElotDbBridge } from "./db/bridge.js";
import { resolveExtensionDbPath } from "./dbResolve.js";
import { registerDbHoverProvider } from "./dbHoverProvider.js";
import { registerDbDecorations } from "./dbDecorations.js";
import { registerDbInfoCommand } from "./dbInfo.js";
import { registerSourceCommands } from "./sourceCommands.js";
import { registerRegisterSourceCommands } from "./registerSourceCommand.js";

export function activate(context: vscode.ExtensionContext) {
  const tangle = async (doc: vscode.TextDocument, manual = false) => {
    if (doc.languageId !== "org" && !doc.fileName.endsWith(".org")) {
      if (manual) vscode.window.showErrorMessage("Not an Org file");
      return;
    }

    try {
      const orgText = doc.getText();
      const inputPath = doc.fileName;

      // Parse Org via orgize WASM → ElotNode tree (synchronous, no worker needed)
      const root = parseOrg(orgText);

      // Generate OMN
      const omn = generateFullOmn(root);

      // Determine output
      let outputPath: string | undefined;
      const firstOntology = (root.children ?? [])[0];
      const tangleTarget = firstOntology?.tangleTargetOmn;

      if (tangleTarget) {
        // Resolve relative to the input file's directory
        outputPath = resolve(dirname(inputPath), tangleTarget);
      } else if (manual) {
        // No tangle target — ask user where to save (only on manual trigger)
        const uri = await vscode.window.showSaveDialog({
          defaultUri: vscode.Uri.file(inputPath.replace(/\.org$/, ".omn")),
          filters: { "Manchester Syntax": ["omn"] },
        });
        if (uri) {
          outputPath = uri.fsPath;
        }
      }

      if (outputPath) {
        writeFileSync(outputPath, omn, "utf-8");
        if (manual) vscode.window.showInformationMessage(`Elot: Written to ${outputPath}`);
      }
    } catch (err: any) {
      if (manual) vscode.window.showErrorMessage(`Elot error: ${err.message}`);
      console.error(err);
    }
  };

  let disposable = vscode.commands.registerCommand("elot.tangle", () => {
    const editor = vscode.window.activeTextEditor;
    if (editor) {
      tangle(editor.document, true);
    } else {
      vscode.window.showErrorMessage("No active editor");
    }
  });

  // Auto-tangle on save
  const onSave = vscode.workspace.onDidSaveTextDocument((doc) => {
    tangle(doc, false);
  });

  // Hover provider: shows label, rdf:type, and annotations for CURIEs
  const hover = registerHoverProvider();

  // Label decoration toggle (elot.toggleLabels / F5)
  registerLabelDecorations(context);

  // Headline-based folding for Org files
  const folding = registerFoldingProvider();

  // Org-indent-mode: visual indentation based on headline level
  registerOrgIndent(context);

  // Description list tag fontification (always-on for Org files)
  registerDescriptionListDecorations(context);

  // Bold headline fontification (always-on for Org files)
  registerHeadlineBoldDecorations(context);

  // Go-to-definition: Ctrl+Click / F12 jumps to the heading declaring a CURIE
  const definition = registerDefinitionProvider();

  // Completion provider: insert existing resource by CURIE or label
  const completion = registerCompletionProvider();

  // OMN syntax + ELOT lint diagnostics: squiggly underlines on invalid axiom values and ontology issues
  registerDiagnosticsProvider(context);

  // Import OWL ontology via elot-exporter.jar
  const importOwl = registerImportOwlCommand(context);

  // Explicit download/update of elot-exporter.jar
  const downloadExporter = registerDownloadExporterCommand(context);

  // Export to HTML via Pandoc
  const exportHtml = vscode.commands.registerCommand("elot.exportHtml", async () => {
    const editor = vscode.window.activeTextEditor;
    if (!editor || !editor.document.fileName.endsWith(".org")) {
      vscode.window.showErrorMessage("Not an Org file");
      return;
    }

    const storageDir = context.globalStorageUri.fsPath;
    const pandocSetting = vscode.workspace.getConfiguration("elot").get<string>("pandocPath") || "pandoc";

    try {
      const pandocPath = await ensurePandoc({
        pandocPath: pandocSetting,
        storageDir,
        onDownloadPrompt: async () => {
          const choice = await vscode.window.showInformationMessage(
            "Pandoc is required for HTML export but was not found. Download it now?",
            "Download", "Cancel"
          );
          return choice === "Download";
        },
        onProgress: (msg) => vscode.window.setStatusBarMessage(msg, 5000),
      });

      if (!pandocPath) return;

      const inputPath = editor.document.fileName;
      const defaultOutput = inputPath.replace(/\.org$/, ".html");

      const saveUri = await vscode.window.showSaveDialog({
        defaultUri: vscode.Uri.file(defaultOutput),
        filters: { "HTML": ["html"], "All files": ["*"] },
        title: "Export HTML to…",
      });
      if (!saveUri) return;

      const outPath = await vscode.window.withProgress(
        { location: vscode.ProgressLocation.Notification, title: "Exporting to HTML…" },
        () => exportOrgToHtml(inputPath, pandocPath, saveUri.fsPath)
      );

      const openChoice = await vscode.window.showInformationMessage(
        `Exported to ${basename(outPath)}`, "Open in Browser", "OK"
      );
      if (openChoice === "Open in Browser") {
        vscode.env.openExternal(vscode.Uri.file(outPath));
      }
    } catch (err: any) {
      vscode.window.showErrorMessage(`HTML export failed: ${err.message}`);
    }
  });

  // ── DB-backed (global) label display ────────────────────────
  // The CLI is the sole writer; the bridge opens the DB read-only,
  // watches for changes, and serves hovers for non-Org files.
  const dbBridge = new ElotDbBridge({
    resolvePath: () => {
      const cfg = vscode.workspace.getConfiguration("elot");
      const wsCfg = cfg.inspect<string>("dbPath");
      return resolveExtensionDbPath({
        workspacePath: wsCfg?.workspaceValue ?? null,
        userPath: wsCfg?.globalValue ?? null,
        envPath: process.env.ELOT_DB_PATH ?? null,
        globalStorageDir: context.globalStorageUri.fsPath,
      });
    },
    onError: (err) => console.error("[elot] DB bridge error:", err),
  });
  // Reload bridge when path setting changes.
  const onDbCfg = vscode.workspace.onDidChangeConfiguration((e) => {
    if (e.affectsConfiguration("elot.dbPath")) {
      dbBridge.reload().catch((err) =>
        console.error("[elot] DB reload failed:", err),
      );
    }
  });
  const dbHover = registerDbHoverProvider(context, dbBridge);
  const dbDecor = registerDbDecorations(context, dbBridge);
  const dbInfo = registerDbInfoCommand(dbBridge);
  const sourceCmds = registerSourceCommands(dbBridge);
  const registerCmds = registerRegisterSourceCommands(dbBridge);
  context.subscriptions.push(dbHover, dbDecor, dbInfo, sourceCmds, registerCmds, onDbCfg, {
    dispose: () => dbBridge.dispose(),
  });

  // Clean up cached slurp maps when documents are closed
  const onClose = vscode.workspace.onDidCloseTextDocument((doc) => {
    clearSlurpCache(doc.uri.toString());
  });

  context.subscriptions.push(disposable, onSave, hover, folding, definition, completion, importOwl, downloadExporter, exportHtml, onClose);
}

export function deactivate() {}
