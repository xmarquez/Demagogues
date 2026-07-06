#!/bin/bash
# Refresh the Singularity image from GHCR (run on Raapoi after a new
# container build, e.g. when renv.lock changes).
#
#   bash slurm/pull_image.sh [tag]     # default tag: latest
set -euo pipefail

TAG="${1:-latest}"
DEMAGOGUES_SIF="${DEMAGOGUES_SIF:-/nfs/scratch/$USER/containers/demagogues.sif}"

mkdir -p "$(dirname "$DEMAGOGUES_SIF")"
# Lmod ``module`` is a shell function that non-login shells lack: init it first.
if ! command -v module >/dev/null 2>&1; then
  source /etc/profile.d/lmod.sh 2>/dev/null || source /opt/ohpc/admin/lmod/lmod/init/bash
fi
module use /home/software/tools/eb_modulefiles/all/Core 2>/dev/null || true
module load GCC/10.2.0 OpenMPI/4.0.5 Singularity/3.10.2
singularity pull --force "$DEMAGOGUES_SIF" "docker://ghcr.io/xmarquez/demagogues:$TAG"
echo "Pulled ghcr.io/xmarquez/demagogues:$TAG -> $DEMAGOGUES_SIF"
