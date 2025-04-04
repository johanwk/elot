# Using ELOT Batch Processing with Visual Studio Code

This guide explains how to set up your environment to run ELOT's batch processing commands (like tangling Org files) and use helpful snippets directly from within Visual Studio Code on **your own projects**.

**Goal:** You will be able to open an `.org` file in your project within VS Code, use snippets to create new ontology structures, and run commands (a "Task") that use the separate ELOT tooling installation to generate corresponding outputs (`.omn`, `.ttl`, `.html`, etc.).

## Prerequisites

Ensure the following software is installed on your system:

1.  **[Visual Studio Code](https://code.visualstudio.com/)**
2.  **[Git](https://git-scm.com/downloads)**
3.  **[Emacs](https://www.gnu.org/software/emacs/download.html):** Install a recent stable version.
    *   **IMPORTANT:** Ensure the directory containing the `emacs` executable (e.g., `emacs.exe` on Windows, `emacs` on Linux/macOS) is added to your system's **`PATH`** environment variable. VS Code needs this to find the `emacs` command. Common installers (Homebrew, Chocolatey, Linux package managers) often handle this, but double-check if you encounter issues.
4.  **Setup Script Dependencies:**
    *   **Windows:** PowerShell is typically built-in.
    *   **Linux/macOS:** `curl` and `jq`. Install using your package manager (e.g., `sudo apt install curl jq`, `brew install curl jq`).
5.  **(Recommended) VS Code Org Mode Extension:** For the best experience editing `.org` files, install the [Org Mode extension](https://marketplace.visualstudio.com/items?itemName=vscode-org-mode.org-mode) from the VS Code Marketplace.

## Step 1: Install ELOT Command-Line Tools

First, get the ELOT CLI scripts and dependencies into a dedicated location on your system. This is separate from your individual projects.

1.  **Choose Installation Location:** Decide where you want to keep the ELOT CLI tools (e.g., `~/tools/elot-cli`, `C:\tools\elot-cli`, `~/elot`). This directory will be referred to as your `<ELOT_CLI_Installation_Directory>`.
2.  **Get the Tools:**
    *   **Option A (Git Clone - Recommended):**
        *   Open your terminal/command prompt.
        *   Navigate *outside* your project directory to where you want to install the tools (e.g., `cd ~/tools`).
        *   Clone the ELOT repository from GitHub:
            ```bash
            # This will create a directory named 'elot' by default
            git clone https://github.com/johanwk/elot.git
            ```
        *   Your `<ELOT_CLI_Installation_Directory>` is now `~/tools/elot` (in this example). You can rename the `elot` directory if you prefer, just use that name consistently.
    *   **Option B (Download Zip/Release):**
        *   Download the release archive from the [ELOT GitHub Releases page](https://github.com/johanwk/elot/releases).
        *   Extract the contents to your chosen `<ELOT_CLI_Installation_Directory>`. Ensure the extracted folder has the core files like `setup.sh`, `elot-package/` etc. at its top level.
3.  **Run the One-Time Setup Script:**
    *   **Crucially:** Navigate *into* the `<ELOT_CLI_Installation_Directory>` (e.g., the `elot` directory you just cloned) in your terminal/PowerShell:
        ```bash
        cd <ELOT_CLI_Installation_Directory> # e.g., cd ~/tools/elot
        ```
    *   Run the setup script appropriate for your OS. This downloads dependencies into the `lib/` and `bin/` subdirectories *within this installation directory*:
        *   **Windows (PowerShell):**
            ```powershell
            .\setup.ps1
            ```
            *(Note: If you encounter errors about execution policies, you might need to run PowerShell as Administrator once and execute `Set-ExecutionPolicy -Scope CurrentUser RemoteSigned`, answer 'Y', then retry `.\setup.ps1` in a regular PowerShell window.)*
        *   **Linux or macOS (Bash):**
            ```bash
            # Make executable (if needed) then run:
            chmod +x setup.sh
            ./setup.sh
            # OR just run with bash directly:
            # bash setup.sh
            ```
    *   Wait for the script to complete successfully. It will download files and may take a few minutes.

## Step 2: Configure Environment Variable

The VS Code tasks need to know where you installed the ELOT CLI tools. You **must** set an environment variable named `ELOT_CLI_HOME` pointing to your `<ELOT_CLI_Installation_Directory>`.

*   **Windows:**
    1.  Search for "Environment Variables" in the Start Menu and open "Edit the system environment variables".
    2.  Click the "Environment Variables..." button.
    3.  In the "User variables" section (recommended) or "System variables", click "New...".
    4.  Variable name: `ELOT_CLI_HOME`
    5.  Variable value: `C:\path\to\your\elot` (Use the actual **full path**, e.g., `C:\tools\elot`)
    6.  Click OK on all dialogs.
    7.  **Important:** You **must restart VS Code** (or potentially log out/log in) for it to recognize the new environment variable.
*   **macOS / Linux (Bash/Zsh):**
    1.  Edit your shell's startup configuration file (e.g., `~/.bashrc`, `~/.zshrc`, `~/.profile`, or `~/.bash_profile` depending on your shell and OS).
    2.  Add the following line at the end (replace the path with your actual `<ELOT_CLI_Installation_Directory>`):
        ```bash
        export ELOT_CLI_HOME="/path/to/your/elot"
        # Example: export ELOT_CLI_HOME="$HOME/tools/elot"
        ```
    3.  Save the file.
    4.  Apply the changes: Either run `source ~/.your_config_file` (e.g., `source ~/.zshrc`) in your current terminal or simply open a new terminal window.
    5.  **Important:** Launch VS Code *from a terminal window* where this variable is set, or **restart VS Code** to ensure it inherits the new environment variable.

## Step 3: Configure Your Project in VS Code

Now, configure the specific project where you will be working with `.org` files to use the provided VS Code helpers (Tasks and Snippets).

1.  **Open Your Project:** Launch VS Code and use **File** > **Open Folder...** to open *your* project's main directory (NOT the ELOT CLI installation directory).
2.  **Create `.vscode` Directory (if needed):** In the VS Code Explorer panel, check if your project root has a `.vscode` subfolder. If not, right-click in the empty space of the Explorer panel and select "New Folder", naming it `.vscode`. This directory is where VS Code automatically looks for project-specific settings.
3.  **Copy Helper Files:**
    *   Locate the example `tasks.json` and `elot.code-snippets` files provided within the ELOT CLI tool distribution (they should be in the `vscode-support/` directory of the ELOT repository you cloned/downloaded).
    *   Copy **both** `tasks.json` and `elot.code-snippets` from the source `vscode-support/` directory.
4.  **Paste into Project:**
    *   Paste the copied files *inside* your project's `.vscode` directory.
    *   Ensure the pasted files are named exactly `tasks.json` and `elot.code-snippets`.

## Using ELOT Features in VS Code

Once the setup (Steps 1-3) is complete, you can leverage the integrated features within VS Code for your project:

### 1. Creating New Ontologies with Snippets

The `elot.code-snippets` file provides a template for quickly scaffolding a new ELOT Ontology Org file.

**Usage:**

1.  **Create/Open Org File:** Create a new file within your project and save it with an `.org` extension (e.g., `my-ontology.org`), or open an existing one.
2.  **Type Prefix:** In the Org file, start typing the snippet prefix: `elotOntologyTemplate`
3.  **Trigger Snippet:** VS Code's IntelliSense will suggest the snippet ("Creates a new ELOT Ontology Org file structure..."). Press `Tab` or `Enter` to insert the full template.
4.  **Fill Placeholders:** The template will be inserted. Use the `Tab` key to navigate through the highlighted placeholders and fill in the required information:
    *   **Label:** The human-readable title (e.g., "My Awesome Ontology").
    *   **Localname:** The technical identifier, often lowercase and hyphenated (e.g., `my-awesome-ontology`). Used for IDs and the `.omn` filename.
    *   **Prefix:** The short namespace prefix (e.g., `awe` or `mao`). *Do not include the colon (`:`) here; the snippet adds it where needed.*
    *   **Namespace URI:** The full URI for the ontology's default namespace (e.g., `http://myorg.com/ontologies/awesome#`).
    *   **Author:** Your name or identifier.
5.  **Start Editing:** Once all placeholders are filled by pressing `Tab` sequentially, the cursor will land at the final position, ready for you to add specific ontology content (like classes or properties).

### 2. Running Batch Processing Tasks

The `tasks.json` file defines commands to run ELOT's batch processing tools.

**How to Run:**

1.  **Open the Command Palette:** Use `Ctrl+Shift+P` (Windows/Linux) or `Cmd+Shift+P` (macOS), or press `F1`.
2.  **Select "Run Task":** Type `Run Task` and select **Tasks: Run Task** from the list.
3.  **Choose ELOT Task:** Select one of the available tasks prefixed with `ELOT:`:
    *   `ELOT: Tangle Current File`: Processes the *currently active* Org file editor to generate `.omn`/`.ttl` files according to the `:tangle` headers.
    *   `ELOT: Export Current File to HTML`: Exports the *currently active* Org file editor to HTML (output file is named automatically based on the Org filename).
    *   `ELOT: Import Ontology to Org File (elot-exporter.jar)`: Reads an external ontology (URL or local file) and converts it into an ELOT-structured Org file.
        *   **Prompts:** This task will ask for two inputs sequentially via the Command Palette: the source ontology URI/path and the desired output `.org` filename.
4.  **Observe Terminal:** The integrated **Terminal** panel will open and show the output of the running task. Check here for success or error messages. Generated files (e.g., `.omn`, `.html`, the imported `.org`) will appear in your project's file explorer.

## Limitations in VS Code Integration

The VS Code tasks and snippets provide convenient ways to run **non-interactive batch processes** and **scaffold files**. However, they do **not** replicate the full interactive experience of using ELOT within GNU Emacs.

Features **not** available via the VS Code integration include:

*   **Interactive Label Display:** Switching between showing full identifiers (IRIs) and their labels directly within the editor buffer (`elot-label-display-mode`).
*   **Interactive ROBOT/SPARQL Commands:** Running arbitrary ROBOT commands or SPARQL queries on the current buffer or region using ELOT's interactive Emacs functions.
*   **Diagram Generation:** Dynamically generating diagrams (e.g., using `rdfpuml`) based on the buffer content.

While diagrams and other derived artifacts can still be *included* in your Org files (e.g., as images), the dynamic generation and interactive features require the full Emacs environment.

**For the complete set of ELOT features, using GNU Emacs directly is recommended.** The VS Code integration focuses on automating core batch processing and file creation workflows.

## Troubleshooting

*   **Task fails with "command not found" (referring to `elot-run.sh` or `elot-run.ps1`):**
    *   Verify the `ELOT_CLI_HOME` environment variable is correctly set to the full path of your ELOT CLI installation directory (e.g., the `elot` folder you cloned).
    *   Restart VS Code after setting the environment variable.
    *   Ensure the `elot-run.sh` or `elot-run.ps1` file actually exists in the directory specified by `ELOT_CLI_HOME`.
    *   Ensure the setup script (`setup.sh`/`.ps1`) was run successfully within the `ELOT_CLI_HOME` directory.
*   **Task fails with "emacs: command not found":**
    *   Ensure Emacs is installed and its `bin` directory is correctly added to your system `PATH`. Restart VS Code if you recently changed the PATH.
*   **Snippet `elotOntologyTemplate` does not appear:**
    *   Ensure the file `.vscode/elot.code-snippets` exists in your project root.
    *   Ensure you are editing a file with the `.org` extension.
    *   Try restarting VS Code.
    *   Check the `elot.code-snippets` file for JSON syntax errors (VS Code might highlight them).
*   **Task runs but fails with errors in the terminal:**
    *   Read the error messages carefully. They might come from Emacs Lisp (`cli_runner.el` or ELOT code), ROBOT (during conversion), or the shell script wrapper.
    *   Check that the `lib/` and `bin/` directories within your `ELOT_CLI_HOME` contain the expected dependencies. You may need to re-run the setup script.
    *   Ensure the input `.org` file exists and is correctly formatted.
