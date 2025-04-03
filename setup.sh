#!/bin/bash
# setup.sh - Prepares the ELOT environment by fetching dependencies. (--eval style)
# Creates ./lib/ for Elisp packages and ./bin/ for ROBOT JAR.
# Uses system 'emacs', includes fixes, and converts paths for MSYS/MINGW.

# Exit immediately if a command exits with a non-zero status.
set -e
# Treat unset variables as an error when substituting.
# set -u # Optional: uncomment if you want stricter variable checks
# Cause pipelines to fail on the first command that fails.
set -o pipefail

echo "--- Starting ELOT Environment Setup (eval style) ---"

# --- Determine Paths and OS ---

# Get the initial POSIX path where this script is located.
SCRIPT_DIR_POSIX="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

# Detect OS Type
OS_TYPE="unknown"
# Check common indicators for MSYS/Cygwin
if [[ "$OSTYPE" == "msys" ]] || [[ "$OSTYPE" == "cygwin" ]] || [[ "$(uname -o 2>/dev/null)" == "Msys" ]] || [[ "$(uname -o 2>/dev/null)" == "Cygwin" ]]; then
    OS_TYPE="msys"
# Check common indicators for Linux/WSL
elif [[ "$OSTYPE" == "linux-gnu"* ]] || [[ "$(uname -o 2>/dev/null)" == "GNU/Linux" ]]; then
    OS_TYPE="linux"
fi
echo "Detected OS type: $OS_TYPE"

# Convert script path for Windows Emacs if running in MSYS/MINGW
if [[ "$OS_TYPE" == "msys" ]] && command -v cygpath &> /dev/null; then
    # Use cygpath -m (mixed) for MSYS/MINGW to get C:/Users/... style paths
    SCRIPT_DIR="$(cygpath -m "$SCRIPT_DIR_POSIX")" # <-- CHANGE -W to -m
    echo "Converted SCRIPT_DIR for Windows Emacs: $SCRIPT_DIR"
elif [[ "$OS_TYPE" == "msys" ]]; then
    echo "Warning: Running in MSYS/MINGW but 'cygpath' not found. Using POSIX path '$SCRIPT_DIR_POSIX'. This might cause issues." >&2
    SCRIPT_DIR="$SCRIPT_DIR_POSIX" # Fallback
else
    # Use the POSIX path directly for Linux/WSL
    SCRIPT_DIR="$SCRIPT_DIR_POSIX"
    echo "Using POSIX SCRIPT_DIR: $SCRIPT_DIR"
fi

# --- Check if 'emacs' is on PATH ---
if ! command -v emacs &> /dev/null; then
    echo "Error: 'emacs' command not found. Ensure Emacs is installed and in your system PATH." >&2
    exit 1
fi
EMACS_EXE=$(command -v emacs)
echo "Using Emacs from PATH: $EMACS_EXE"
# -----------------------------------

# Define target directories based on the potentially converted SCRIPT_DIR
LIB_DIR="${SCRIPT_DIR}/lib"
BIN_DIR="${SCRIPT_DIR}/bin"
GPG_DIR="${LIB_DIR}/gnupg" # GnuPG home within lib

# Create directories
echo "Creating directories '$LIB_DIR', '$BIN_DIR', '$GPG_DIR' if they don't exist..."
mkdir -p "$LIB_DIR"
mkdir -p "$BIN_DIR"
mkdir -p "$GPG_DIR"

# --- Install Elisp Dependencies ---
echo "Installing Elisp dependencies using Emacs into '$LIB_DIR'..."

# Prepare paths for Lisp - use simple double quotes now, paths should be correct format
# Emacs Lisp generally prefers forward slashes, cygpath -W provides this.
# Simple echo is sufficient if paths are clean.
elisp_lib_dir_q="\"$LIB_DIR\""
elisp_gpg_dir_q="\"$GPG_DIR\""

# List of required packages
PACKAGES=(
    "gnu-elpa-keyring-update" # Install this first
    "htmlize"
    "ht"
    "omn-mode"
    "hydra"
    "sparql-mode"
    "dash"
    "lv"
)

# Construct the package-install --eval arguments
install_evals=()
for pkg in "${PACKAGES[@]}"; do
    install_evals+=("--eval" "(package-install '$pkg)")
done

# Run Emacs with multiple --eval arguments
echo "Running Emacs command..."
emacs -q --batch \
  --eval "(require 'package)" \
  --eval "(setq url-retrieve-timeout 60)" \
  --eval "(setq package-gnupghome-dir $elisp_gpg_dir_q)" \
  --eval "(setq package-check-signature nil)" \
  --eval "(setq gnutls-algorithm-priority \"NORMAL:-VERS-TLS1.3\")" \
  --eval "(setq package-user-dir $elisp_lib_dir_q)" \
  --eval "(setq package-archives '((\"gnu\" . \"https://elpa.gnu.org/packages/\") (\"melpa\" . \"https://melpa.org/packages/\")))" \
  --eval "(package-initialize)" \
  --eval "(message \"Attempting package refresh (TLS 1.3 disabled, Signature check off)...\")" \
  --eval "(package-refresh-contents)" \
  --eval "(unless package-archive-contents (error \"Package archive contents are still nil after refresh! Check network/TLS/permissions.\"))" \
  --eval "(message \"Package contents loaded successfully.\")" \
  --eval "(message \"Installing packages...\")" \
  "${install_evals[@]}" \
  --eval "(message \"Package installation attempted.\")"

# Check exit code
EMACS_EXIT_CODE=$?
if [ $EMACS_EXIT_CODE -ne 0 ]; then
    echo "Error: Emacs setup failed with exit code $EMACS_EXIT_CODE." >&2
    exit $EMACS_EXIT_CODE
fi
echo "Elisp package installation step completed successfully."

# --- Download ROBOT JAR ---
# (Remains the same)
echo "Downloading latest ROBOT JAR..."
API_URL="https://api.github.com/repos/ontodev/robot/releases/latest"
ROBOT_JAR_PATH="${BIN_DIR}/robot.jar"
echo "Fetching download URL from $API_URL..."
DOWNLOAD_URL=$(curl --silent --fail --location "$API_URL" | jq --raw-output '.assets[] | select(.name=="robot.jar") | .browser_download_url')
CURL_EXIT_CODE=$?
if [ $CURL_EXIT_CODE -ne 0 ] || [ -z "$DOWNLOAD_URL" ] || [ "$DOWNLOAD_URL" == "null" ]; then
    echo "Error: Could not find robot.jar download URL from GitHub API (curl exit code: $CURL_EXIT_CODE)." >&2
    exit 1
fi
echo "Found download URL: $DOWNLOAD_URL"
echo "Downloading robot.jar to '$ROBOT_JAR_PATH'..."
curl --location --output "$ROBOT_JAR_PATH" "$DOWNLOAD_URL" --fail
CURL_EXIT_CODE=$?
if [ $CURL_EXIT_CODE -ne 0 ]; then
    echo "Error: Failed to download ROBOT JAR (curl exit code: $CURL_EXIT_CODE)." >&2
    exit 1
fi
chmod +r "$ROBOT_JAR_PATH"
echo "ROBOT JAR download completed."

# --- Finish ---
echo ""
echo "--- ELOT Environment Setup Complete ---"
echo "Dependencies installed in: $LIB_DIR"
echo "ROBOT JAR downloaded to: $BIN_DIR"
