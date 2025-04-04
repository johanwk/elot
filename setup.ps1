# setup.ps1 - Prepares the ELOT environment by fetching dependencies.
# Creates ./lib/ for Elisp packages and ./bin/ for ROBOT JAR.
# Uses system 'emacs' and includes fixes for Windows/MSYS environment.

Write-Host "--- Starting ELOT Environment Setup ---" -ForegroundColor Yellow

# Get the directory where this script is located.
$ScriptDir = $PSScriptRoot
Write-Host "Project Root detected as: $ScriptDir"

# --- Check if 'emacs' is on PATH ---
$emacsCheck = Get-Command emacs -ErrorAction SilentlyContinue
if (-not $emacsCheck) {
    Write-Error "Error: 'emacs' command not found. Ensure Emacs is installed and in your system PATH."
    exit 1
}
Write-Host "Using Emacs from PATH: $($emacsCheck.Source)" -ForegroundColor Cyan
# -----------------------------------

# Define target directories relative to the script
$LibDir = Join-Path $ScriptDir "lib"
$BinDir = Join-Path $ScriptDir "bin"
$GpgDir = Join-Path $LibDir "gnupg" # GnuPG home within lib

# Create directories
Write-Host "Creating directories '$LibDir', '$BinDir', '$GpgDir' if they don't exist..."
New-Item -ItemType Directory -Path $LibDir -Force | Out-Null
New-Item -ItemType Directory -Path $BinDir -Force | Out-Null
New-Item -ItemType Directory -Path $GpgDir -Force | Out-Null # Create GPG dir

# --- Prepare Elisp Setup Script ---
Write-Host "Preparing Elisp setup script..." -ForegroundColor Cyan

# List of required packages
$Packages = @(
    "gnu-elpa-keyring-update", # Install this first
    "htmlize",
    "ht",
    "omn-mode",
    "hydra",
    "sparql-mode",
    "dash",
    "lv"
)

# Prepare paths for Lisp (forward slashes)
$elispLibDir = $LibDir.Replace('\', '/')
$elispGpgDir = $GpgDir.Replace('\', '/')

# Generate the (package-install ...) expressions
$installExpressions = $Packages | ForEach-Object { "(package-install '$_)" }

# Create the full Lisp code using a Here-String
# Escape internal double quotes as \" for Lisp strings
$lispSetupCode = @"
;; Temporary Elisp script for ELOT setup
(require 'package)
;; Settings found to work on Windows/MSYS:
(setq url-retrieve-timeout 60)
(setq package-gnupghome-dir "$elispGpgDir")
(setq package-check-signature nil)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq package-user-dir "$elispLibDir")
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(message "Attempting package refresh (TLS 1.3 disabled, Signature check off)...")
(package-refresh-contents) ; Call refresh, ignore direct return value

;; Check AFTER refresh if contents were actually loaded
(unless package-archive-contents
  (error "Package archive contents are still nil after refresh! Check network/TLS/permissions."))
(message "Package contents loaded successfully.")

;; Install required packages
(message "Installing packages: $($Packages -join ', ')...")
$($installExpressions -join "`n")
(message "Package installation attempted.")
"@

# Define temporary file path
$tempElispFile = Join-Path $env:TEMP "elot_setup_$($PID).el"

# --- Run Emacs with --load ---
Write-Host "Installing Elisp dependencies using Emacs via --load '$tempElispFile'"
try {
    # Write the Lisp code to the temporary file (UTF8 recommended)
    Set-Content -Path $tempElispFile -Value $lispSetupCode -Encoding UTF8

    # Execute Emacs, loading the temporary file
    Write-Host "Running Emacs command: & emacs -q --batch --load '$tempElispFile'"
    & emacs -q --batch --load $tempElispFile # Use 'emacs' from PATH

    if ($LASTEXITCODE -ne 0) {
        throw "Emacs setup script failed with exit code $LASTEXITCODE."
    }
    Write-Host "Elisp package installation step completed successfully." -ForegroundColor Green
} catch {
    Write-Error "Error during Elisp setup script execution: $_"
    # Optional: Display content of temp file on error for debugging
    # if (Test-Path $tempElispFile) { Write-Warning "Content of temporary script '$tempElispFile':"; Get-Content $tempElispFile | Write-Warning }
    exit 1
} finally {
    # Clean up the temporary file
    if (Test-Path $tempElispFile) {
        Write-Host "Cleaning up temporary file '$tempElispFile'"
        Remove-Item $tempElispFile -Force -ErrorAction SilentlyContinue
    }
}

# --- Download ROBOT JAR ---
Write-Host "Downloading latest ROBOT JAR..." -ForegroundColor Cyan

# GitHub API URL for the latest release
$ApiUrl = "https://api.github.com/repos/ontodev/robot/releases/latest"
$RobotJarPath = Join-Path $BinDir "robot.jar"

# Fetch the download URL using Invoke-WebRequest and JSON parsing
Write-Host "Fetching download URL from $ApiUrl..."
try {
    # Use -UseBasicParsing for broader compatibility
    # Add User-Agent header as GitHub API might require it sometimes
    $headers = @{ "User-Agent" = "PowerShell-ELOT-Setup-Script" }
    $ApiResponse = Invoke-WebRequest -Uri $ApiUrl -UseBasicParsing -Headers $headers -ErrorAction Stop
    $ApiData = $ApiResponse.Content | ConvertFrom-Json -ErrorAction Stop
    $DownloadUrl = $ApiData.assets | Where-Object { $_.name -eq 'robot.jar' } | Select-Object -ExpandProperty browser_download_url

    if (-not $DownloadUrl) {
        throw "Could not find robot.jar download URL in API response."
    }

    Write-Host "Found download URL: $DownloadUrl"
    Write-Host "Downloading robot.jar to '$RobotJarPath'..."

    # Download the file
    Invoke-WebRequest -Uri $DownloadUrl -OutFile $RobotJarPath -UseBasicParsing -ErrorAction Stop

    Write-Host "ROBOT JAR download completed." -ForegroundColor Green

} catch {
    Write-Error "Error downloading ROBOT JAR: $_"
    exit 1
}

# --- Download elot-exporter.jar ---
Write-Host "Downloading elot-exporter.jar..." -ForegroundColor Cyan
$ExporterUrl = "https://github.com/johanwk/elot/releases/download/v1.0.5/elot-exporter-0.6-SNAPSHOT.jar"
$ExporterJarPath = Join-Path $BinDir "elot-exporter.jar" # Use a clean name

Write-Host "Downloading from $ExporterUrl to '$ExporterJarPath'..."
try {
    Invoke-WebRequest -Uri $ExporterUrl -OutFile $ExporterJarPath -UseBasicParsing -ErrorAction Stop
    Write-Host "elot-exporter.jar download completed." -ForegroundColor Green
} catch {
    Write-Error "Error downloading elot-exporter.jar: $_"
    # Consider if this failure should stop the whole script or just warn
    exit 1
}

# --- Finish ---
Write-Host ""
Write-Host "--- ELOT Environment Setup Complete ---" -ForegroundColor Yellow
Write-Host "Dependencies installed in: $LibDir"
Write-Host "ROBOT JAR downloaded to: $BinDir"
