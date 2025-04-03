# elot-run.ps1 - Wrapper script for the ELOT CLI tool on Windows

# --- Determine Paths ---
$ScriptDir = $PSScriptRoot # Project Root
$RootDir = $ScriptDir

# Define expected dependency directories relative to the root
$LibDir = Join-Path $RootDir "lib"
$BinDir = Join-Path $RootDir "bin"
$ElotPackageDir = Join-Path $RootDir "elot-package"
$CliRunnerEl = Join-Path $RootDir "cli_runner.el"
$RobotJar = Join-Path $BinDir "robot.jar"

Write-Host "--- ELOT Runner ---" -ForegroundColor Yellow
Write-Host "Project Root: $RootDir"

# --- Check Prerequisites ---
Write-Host "Checking prerequisites..."
if (-not (Test-Path $LibDir -PathType Container)) {
    Write-Error "Error: Dependency directory '$LibDir' not found. Please run setup.ps1 first."
    exit 1
}
if (-not (Test-Path $BinDir -PathType Container)) {
    Write-Error "Error: Binary directory '$BinDir' not found. Please run setup.ps1 first."
    exit 1
}
if (-not (Test-Path $RobotJar -PathType Leaf)) {
    Write-Error "Error: ROBOT JAR not found at '$RobotJar'. Please run setup.ps1 first."
    exit 1
}
if (-not (Test-Path $ElotPackageDir -PathType Container)) {
    Write-Error "Error: Core ELOT directory '$ElotPackageDir' not found."
    exit 1
}
if (-not (Test-Path $CliRunnerEl -PathType Leaf)) {
    Write-Error "Error: Emacs runner script '$CliRunnerEl' not found."
    exit 1
}
$emacsCheck = Get-Command emacs -ErrorAction SilentlyContinue
if (-not $emacsCheck) {
    Write-Error "Error: 'emacs' command not found. Ensure Emacs is installed and in your system PATH."
    exit 1
}
Write-Host "Prerequisites seem OK." -ForegroundColor Green

# --- Set Environment Variables for Emacs ---
# Note: This syntax sets it for processes launched *from this script*.
$env:ELOT_ROOT_DIR = $RootDir
$env:ELOT_BIN_DIR = $BinDir
Write-Host "Setting ELOT_ROOT_DIR=$($env:ELOT_ROOT_DIR)"
Write-Host "Setting ELOT_BIN_DIR=$($env:ELOT_BIN_DIR)"

# --- Prepare Emacs Arguments ---
# Arguments passed TO this script are in the automatic $args array.
$emacsArgs = @(
    "-q",
    "--batch",
    "--load",
    "$CliRunnerEl", # Path should be safe here, but Join-Path was correct
    "--"
) + $args # Append all arguments passed to this script

# --- Execute Emacs ---
Write-Host "Launching ELOT tool via Emacs..."
try {
    # Execute Emacs. PowerShell handles quoting arguments automatically
    & emacs $emacsArgs # Use emacs from PATH

    # Capture Emacs exit code
    $ExitCode = $LASTEXITCODE
} catch {
    # Catch errors from PowerShell failing to launch emacs itself
    Write-Error "Failed to launch Emacs process: $_"
    exit 1 # Or another distinct exit code
}

# --- Report Status ---
if ($ExitCode -ne 0) {
    Write-Host "ELOT tool failed with exit code $ExitCode." -ForegroundColor Red
} else {
    Write-Host "ELOT tool finished successfully." -ForegroundColor Green
}

exit $ExitCode
