<#
.SYNOPSIS
Deploy the current commit to Raapoi and submit a pipeline run.

.DESCRIPTION
Checks that the git tree is clean and pushed, verifies SSH connectivity
(printing first-time key-setup guidance on failure), checks out the local HEAD
sha in the scratch clone on the cluster, and submits slurm/launch.sh (or
slurm/smoke.sh with -Smoke) via sbatch.

.EXAMPLE
.\slurm\deploy.ps1 -Run legitimacy_glmnet_100
.\slurm\deploy.ps1 -Smoke

.NOTES
Parameter is -HostName (not -Host) because $Host is a reserved automatic
variable in PowerShell. VPN to the VUW network is required off-campus.
#>
[CmdletBinding()]
param(
    [string]$Run = "full_democracy",
    [string]$Profile = "full",
    [switch]$Smoke,
    [string]$HostName = "raapoi.vuw.ac.nz",
    [string]$User = "marquexa",
    [string]$Scratch = "/nfs/scratch/marquexa/Demagogues",
    # Optional walltime override for the coordinator job, e.g. "1-00:00:00".
    [string]$Walltime = ""
)

$ErrorActionPreference = "Stop"
$remote = "$User@$HostName"

function Fail([string]$msg) {
    Write-Host "ERROR: $msg" -ForegroundColor Red
    exit 1
}

# --- 1. Git tree clean and pushed -------------------------------------------
Write-Host "== Checking git state..."
$dirty = git status --porcelain
if ($dirty) {
    Fail ("Working tree is not clean. Commit or stash first:`n" + ($dirty -join "`n"))
}
git fetch origin master --quiet
if ($LASTEXITCODE -ne 0) { Fail "git fetch origin failed (network? remote?)" }
$headSha = (git rev-parse HEAD).Trim()
$unpushed = git log origin/master..HEAD --oneline
if ($unpushed) {
    Fail ("Local commits not pushed to origin/master:`n" + ($unpushed -join "`n") + "`nRun: git push origin master")
}
Write-Host "   HEAD $headSha is clean and pushed."

# --- 2. SSH connectivity ------------------------------------------------------
Write-Host "== Testing SSH to $remote..."
ssh -o BatchMode=yes -o ConnectTimeout=8 $remote true
if ($LASTEXITCODE -ne 0) {
    Write-Host @"
SSH to $remote failed. First-time setup checklist:

  1. Off-campus? Connect to the VUW VPN first.
  2. No key yet? Generate one:
       ssh-keygen -t ed25519
  3. Copy it to the cluster (Windows equivalent of ssh-copy-id; run in
     PowerShell, not WSL -- this script uses the Windows OpenSSH client):
       type `$env:USERPROFILE\.ssh\id_ed25519.pub | ssh $remote "mkdir -p ~/.ssh && chmod 700 ~/.ssh && cat >> ~/.ssh/authorized_keys && chmod 600 ~/.ssh/authorized_keys"
     (You will be asked for your Raapoi password once. The chmods matter:
     sshd ignores authorized_keys with loose permissions.)
  4. Re-run this script.
"@ -ForegroundColor Yellow
    exit 1
}
Write-Host "   SSH OK."

# --- 3. Sync cluster clone to this commit -------------------------------------
Write-Host "== Checking out $headSha on the cluster ($Scratch)..."
$checkoutCmd = "cd $Scratch && git fetch --all --quiet && git checkout --detach $headSha && git rev-parse HEAD"
$remoteSha = ssh $remote $checkoutCmd
if ($LASTEXITCODE -ne 0) {
    Fail "Remote checkout failed. Has slurm/setup_raapoi.sh been run on the cluster yet?"
}
Write-Host "   Cluster clone at: $remoteSha (detached HEAD)"

# --- 4. Submit ------------------------------------------------------------------
$script = "slurm/launch.sh"
if ($Smoke) {
    $script = "slurm/smoke.sh"
    # Don't let this script's full-run defaults override smoke.sh's own
    # small-run defaults unless the caller passed -Run/-Profile explicitly.
    if (-not $PSBoundParameters.ContainsKey('Run')) { $Run = "auth_glmnet_40" }
    if (-not $PSBoundParameters.ContainsKey('Profile')) { $Profile = "explore" }
}

$exports = "ALL,TARGET_RUN=$Run,TARGET_PROFILE=$Profile"
$sbatchArgs = "--export=$exports"
if ($Walltime) { $sbatchArgs = "$sbatchArgs --time=$Walltime" }

Write-Host "== Submitting $script (run=$Run profile=$Profile)..."
$submitCmd = "cd $Scratch && mkdir -p logs && sbatch $sbatchArgs $script"
$submitOut = ssh $remote $submitCmd
if ($LASTEXITCODE -ne 0) { Fail "sbatch submission failed:`n$submitOut" }
Write-Host "   $submitOut"

$jobId = ""
if ($submitOut -match "Submitted batch job (\d+)") { $jobId = $Matches[1] }

# --- 5. Status + monitoring hints -------------------------------------------
Write-Host "== Current queue for ${User}:"
ssh $remote "squeue -u $User"

Write-Host @"

Monitor with:
  ssh $remote "squeue -u $User"
  ssh $remote "tail -f $Scratch/logs/coordinator-$jobId.log"
Cancel with:
  ssh $remote "scancel $jobId"
Fetch results when done:
  .\slurm\fetch_results.ps1 -HostName $HostName -User $User
"@
