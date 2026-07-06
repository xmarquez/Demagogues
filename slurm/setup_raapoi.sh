#!/bin/bash
#
# One-time (idempotent) cluster-side setup for the Demagogues pipeline.
# Run ON Raapoi (login node):
#
#   bash slurm/setup_raapoi.sh
#
# or, before the repo exists on the cluster, paste the raw file:
#
#   curl -fsSL https://raw.githubusercontent.com/xmarquez/Demagogues/master/slurm/setup_raapoi.sh | bash
#
# Steps: scratch directories, repo clone/pull, intra-cluster ssh key (needed by
# the sbatch/squeue shims that run inside the coordinator container), container
# pull from GHCR, shim permissions, and a checklist of the data that still
# needs rsyncing from the Windows machine.

set -euo pipefail

SCRATCH_ROOT="${SCRATCH_ROOT:-/nfs/scratch/$USER}"
DEMAGOGUES_SCRATCH="${DEMAGOGUES_SCRATCH:-$SCRATCH_ROOT/Demagogues}"
DEMAGOGUES_SIF="${DEMAGOGUES_SIF:-$SCRATCH_ROOT/containers/demagogues.sif}"
CORPORA_ROOT="${CORPORA_ROOT:-$SCRATCH_ROOT/corpora}"
REPO_URL="${REPO_URL:-https://github.com/xmarquez/Demagogues.git}"

done_list=()

step() { echo; echo "== $*"; }

# --- 1. Directories ----------------------------------------------------------
step "Creating scratch directories"
mkdir -p "$SCRATCH_ROOT/containers"
mkdir -p "$CORPORA_ROOT/hathi/raw-hathifiles" "$CORPORA_ROOT/hathi/hathi-ef"
mkdir -p "$(dirname "$DEMAGOGUES_SCRATCH")"
done_list+=("Directories under $SCRATCH_ROOT")

# --- 2. Repo clone / pull ----------------------------------------------------
if [ -d "$DEMAGOGUES_SCRATCH/.git" ]; then
  step "Repo exists: pulling latest master in $DEMAGOGUES_SCRATCH"
  git -C "$DEMAGOGUES_SCRATCH" fetch --all --quiet
  # Stay on whatever commit deploy.ps1 checked out unless we're on a branch.
  if git -C "$DEMAGOGUES_SCRATCH" symbolic-ref -q HEAD >/dev/null; then
    git -C "$DEMAGOGUES_SCRATCH" pull --ff-only
  else
    echo "   (detached HEAD - leaving checkout as-is; deploy.ps1 manages it)"
  fi
  done_list+=("Repo updated at $DEMAGOGUES_SCRATCH")
else
  step "Cloning $REPO_URL into $DEMAGOGUES_SCRATCH"
  git clone "$REPO_URL" "$DEMAGOGUES_SCRATCH"
  done_list+=("Repo cloned to $DEMAGOGUES_SCRATCH")
fi

mkdir -p "$DEMAGOGUES_SCRATCH/logs/crew" "$DEMAGOGUES_SCRATCH/exports"

# The pull above may have updated THIS script while bash was executing the old
# buffered copy. Re-exec the fresh repo copy once so later steps run current code.
if [ -z "${DEMAGOGUES_SETUP_REEXEC:-}" ] && [ -f "$DEMAGOGUES_SCRATCH/slurm/setup_raapoi.sh" ]; then
  export DEMAGOGUES_SETUP_REEXEC=1
  echo "   Re-executing the updated setup script..."
  exec bash "$DEMAGOGUES_SCRATCH/slurm/setup_raapoi.sh"
fi

# --- 3. Intra-cluster passwordless ssh ----------------------------------------
# The coordinator container submits/monitors worker jobs by ssh-ing back to the
# login node (slurm/shims/*). That needs a key held on the cluster itself,
# authorized for the cluster itself.
step "Checking intra-cluster ssh key"
KEY="$HOME/.ssh/id_ed25519_raapoi_internal"
mkdir -p "$HOME/.ssh"
chmod 700 "$HOME/.ssh"
if [ ! -f "$KEY" ]; then
  ssh-keygen -t ed25519 -N "" -f "$KEY" -C "demagogues-intra-cluster"
  done_list+=("Generated intra-cluster key $KEY")
else
  echo "   Key already exists: $KEY"
fi
PUB="$(cat "$KEY.pub")"
touch "$HOME/.ssh/authorized_keys"
chmod 600 "$HOME/.ssh/authorized_keys"
if ! grep -qF "$PUB" "$HOME/.ssh/authorized_keys"; then
  echo "$PUB" >> "$HOME/.ssh/authorized_keys"
  done_list+=("Authorized intra-cluster key")
else
  echo "   Key already authorized."
fi
# Make ssh from compute nodes/containers pick the key up automatically.
if ! grep -qs "id_ed25519_raapoi_internal" "$HOME/.ssh/config" 2>/dev/null; then
  {
    echo ""
    echo "# Added by Demagogues slurm/setup_raapoi.sh (intra-cluster job submission)"
    echo "Host raapoi.vuw.ac.nz raapoi* login*"
    echo "  IdentityFile $KEY"
    echo "  StrictHostKeyChecking accept-new"
  } >> "$HOME/.ssh/config"
  chmod 600 "$HOME/.ssh/config"
  done_list+=("Added intra-cluster Host block to ~/.ssh/config")
else
  echo "   ~/.ssh/config already references the intra-cluster key."
fi

# --- 4. Container image --------------------------------------------------------
step "Pulling container image from GHCR"
# Non-interactive shells have a bare MODULEPATH: add the EasyBuild tree first.
module use /home/software/tools/eb_modulefiles/all/Core 2>/dev/null || true
module load GCC/10.2.0 OpenMPI/4.0.5 Singularity/3.10.2
singularity pull --force "$DEMAGOGUES_SIF" docker://ghcr.io/xmarquez/demagogues:latest
done_list+=("Pulled $DEMAGOGUES_SIF")

# --- 5. Shim permissions -------------------------------------------------------
step "Making Slurm ssh shims executable"
chmod +x "$DEMAGOGUES_SCRATCH"/slurm/shims/* "$DEMAGOGUES_SCRATCH"/slurm/*.sh
done_list+=("chmod +x slurm/shims/* and slurm/*.sh")

# --- Summary -------------------------------------------------------------------
echo
echo "================= setup_raapoi.sh complete ================="
for item in "${done_list[@]}"; do
  echo "  [done] $item"
done
cat <<EOF

Still needed (run from the WINDOWS machine or any host with the data):

  1. Hathi catalog snapshot (~several GB, one-time):
     rsync -avP "/d/ResearchData/corpora/hathi/raw-hathifiles/" \\
       $USER@raapoi.vuw.ac.nz:$CORPORA_ROOT/hathi/raw-hathifiles/

  2. Extracted-features cache (incremental; workers can also download missing
     files themselves if compute nodes have outbound network):
     rsync -avP "/d/ResearchData/corpora/hathi/hathi-ef/" \\
       $USER@raapoi.vuw.ac.nz:$CORPORA_ROOT/hathi/hathi-ef/

  3. If the GHCR pull above failed with 403/unauthorized: make the package
     public once at github.com -> profile -> Packages -> demagogues ->
     Package settings -> Change visibility.

Then submit a smoke test from $DEMAGOGUES_SCRATCH:
  sbatch slurm/smoke.sh
EOF
