#!/bin/bash
# Refresh the Singularity image from GHCR (run on Raapoi after a new
# container build, e.g. when renv.lock changes).
#
#   bash slurm/pull_image.sh [tag]     # default tag: latest
set -euo pipefail

TAG="${1:-latest}"
DEMAGOGUES_SIF="${DEMAGOGUES_SIF:-/nfs/scratch/$USER/containers/demagogues.sif}"

mkdir -p "$(dirname "$DEMAGOGUES_SIF")"
module load GCC/10.2.0 OpenMPI/4.0.5 Singularity/3.10.2
singularity pull --force "$DEMAGOGUES_SIF" "docker://ghcr.io/xmarquez/demagogues:$TAG"
echo "Pulled ghcr.io/xmarquez/demagogues:$TAG -> $DEMAGOGUES_SIF"
