<#
.SYNOPSIS
Fetch the newest results bundle from Raapoi and unpack it into this project.

.DESCRIPTION
Finds the most recent exports/results_*.tar.gz in the cluster scratch clone,
scp's it into the local exports/ directory, and extracts it into the project
root with Windows' built-in tar.exe. The bundle contains _targets/meta/meta and
the headline _targets/objects/* files, so after extraction tar_read() on those
targets works locally.

WARNING: extraction REPLACES the local _targets/meta/meta with the cluster's
metadata. Local-only targets will show as outdated afterwards; that is expected.

.EXAMPLE
.\slurm\fetch_results.ps1
#>
[CmdletBinding()]
param(
    [string]$HostName = "raapoi.vuw.ac.nz",
    [string]$User = "marquexa",
    [string]$Scratch = "/nfs/scratch/marquexa/Demagogues"
)

$ErrorActionPreference = "Stop"
$remote = "$User@$HostName"
$projectRoot = Split-Path -Parent $PSScriptRoot   # slurm/ -> project root

# --- Locate newest bundle on the cluster -------------------------------------
Write-Host "== Looking for the newest bundle in ${remote}:$Scratch/exports ..."
$newest = ssh -n -o BatchMode=yes -o ConnectTimeout=8 $remote "ls -1t $Scratch/exports/results_*.tar.gz 2>/dev/null | head -n 1"
if ($LASTEXITCODE -ne 0) {
    Write-Host "SSH failed. Check VPN / keys (see slurm/deploy.ps1 guidance)." -ForegroundColor Red
    exit 1
}
if (-not $newest) {
    Write-Host "No results_*.tar.gz found in $Scratch/exports on the cluster." -ForegroundColor Red
    exit 1
}
$newest = $newest.Trim()
$bundleName = [System.IO.Path]::GetFileName($newest)
Write-Host "   Found: $bundleName"

# --- Download ------------------------------------------------------------------
$localExports = Join-Path $projectRoot "exports"
if (-not (Test-Path $localExports)) { New-Item -ItemType Directory -Path $localExports | Out-Null }
$localBundle = Join-Path $localExports $bundleName

Write-Host "== Downloading to $localBundle ..."
scp "${remote}:$newest" "$localBundle"
if ($LASTEXITCODE -ne 0) {
    Write-Host "scp failed." -ForegroundColor Red
    exit 1
}

# --- Extract into the project ---------------------------------------------------
Write-Host "== Bundle contents:"
tar -tzf "$localBundle"
if ($LASTEXITCODE -ne 0) {
    Write-Host "tar listing failed - corrupt download?" -ForegroundColor Red
    exit 1
}

Write-Host "== Extracting into $projectRoot (this REPLACES _targets/meta/meta with cluster metadata)..."
tar -xzf "$localBundle" -C "$projectRoot"
if ($LASTEXITCODE -ne 0) {
    Write-Host "Extraction failed." -ForegroundColor Red
    exit 1
}

Write-Host @"

Done. Updated files are listed above (under _targets/).
NOTE: the local _targets/meta/meta now reflects the CLUSTER run. Targets that
only exist locally will appear outdated to tar_outdated(); rebuild or ignore.
"@
