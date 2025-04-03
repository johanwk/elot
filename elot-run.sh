#!/bin/bash
# elot-run.sh - Wrapper script for the ELOT CLI tool

# Exit immediately if a command exits with a non-zero status.
set -e
# Treat unset variables as an error when substituting.
set -u
# Cause pipelines to fail on the first command that fails.
set -o pipefail

# --- Determine Paths ---
# Get the directory where this script is located (Project Root).
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
ROOT_DIR="$SCRIPT_DIR"

# Define expected dependency directories relative to the root
LIB_DIR="${ROOT_DIR}/lib"
BIN_DIR="${ROOT_DIR}/bin"
ELOT_PACKAGE_DIR="${ROOT_DIR}/elot-package"
CLI_RUNNER_EL="${ROOT_DIR}/cli_runner.el"
ROBOT_JAR="${BIN_DIR}/robot.jar"

echo "--- ELOT Runner ---"
echo "Project Root: $ROOT_DIR"

# --- Check Prerequisites ---
echo "Checking prerequisites..."
if [ ! -d "$LIB_DIR" ]; then
    echo "Error: Dependency directory '$LIB_DIR' not found." >&2
    echo "Please run the setup script (setup.sh) first." >&2
    exit 1
fi
if [ ! -d "$BIN_DIR" ]; then
    echo "Error: Binary directory '$BIN_DIR' not found." >&2
    echo "Please run the setup script (setup.sh) first." >&2
    exit 1
fi
if [ ! -f "$ROBOT_JAR" ]; then
    echo "Error: ROBOT JAR not found at '$ROBOT_JAR'." >&2
    echo "Please run the setup script (setup.sh) first." >&2
    exit 1
fi
if [ ! -d "$ELOT_PACKAGE_DIR" ]; then
    echo "Error: Core ELOT directory '$ELOT_PACKAGE_DIR' not found." >&2
    exit 1
fi
if [ ! -f "$CLI_RUNNER_EL" ]; then
    echo "Error: Emacs runner script '$CLI_RUNNER_EL' not found." >&2
    exit 1
fi
if ! command -v emacs &> /dev/null; then
    echo "Error: 'emacs' command not found in PATH." >&2
    exit 1
fi
echo "Prerequisites seem OK."

# --- Set Environment Variables for Emacs ---
# Export the variables so the Emacs child process inherits them.
export ELOT_ROOT_DIR="${ROOT_DIR}"
export ELOT_BIN_DIR="${BIN_DIR}"
echo "Exporting ELOT_ROOT_DIR=${ELOT_ROOT_DIR}"
echo "Exporting ELOT_BIN_DIR=${ELOT_BIN_DIR}"

# --- Execute Emacs ---
echo "Launching ELOT tool via Emacs..."

# Run Emacs in batch mode, loading the runner script.
# Pass all arguments given to this shell script ($@) to Emacs
# after the '--' separator.
emacs -q --batch \
  --load "$CLI_RUNNER_EL" \
  -- "$@"

# Capture Emacs exit code
EXIT_CODE=$?

# --- Report Status ---
if [ $EXIT_CODE -ne 0 ]; then
    echo "ELOT tool failed with exit code $EXIT_CODE." >&2
else
    echo "ELOT tool finished successfully."
fi

exit $EXIT_CODE
