import * as vscode from "vscode";
import { writeFileSync } from "fs";
import { resolve, dirname } from "path";
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

  // Clean up cached slurp maps when documents are closed
  const onClose = vscode.workspace.onDidCloseTextDocument((doc) => {
    clearSlurpCache(doc.uri.toString());
  });

  context.subscriptions.push(disposable, onSave, hover, folding, definition, completion, importOwl, downloadExporter, onClose);
}

export function deactivate() {}
