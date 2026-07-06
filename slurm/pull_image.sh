#!/bin/bash
# Refresh the Singularity image from GHCR (run on Raapoi after a new
# container build, e.g. when renv.lock changes).
#
#   bash slurm/pull_image.sh [tag]     # default tag: latest
set -euo pipefail

TAG="${1:-latest}"
DEMAGOGUES_SIF="${DEMAGOGUES_SIF:-/nfs/scratch/$USER/containers/demagogues.sif}"

mkdir -p "$(dirname "$DEMAGOGUES_SIF")"
# Singularity by absolute path (EasyBuild tree is NFS-shared to all nodes).
# Lmod is deliberately avoided: batch shells proved unreliable at initializing
# it (missing shell function, bare MODULEPATH, non-set-u-safe init scripts).
SINGULARITY_BIN="${DEMAGOGUES_SINGULARITY:-/home/software/EasyBuild/software/Singularity/3.10.2-gompi-2020b/bin/singularity}"
if [ ! -x "$SINGULARITY_BIN" ]; then
  echo "WARNING: $SINGULARITY_BIN not found; falling back to module-loaded singularity" >&2
  set +eu
  if ! command -v module >/dev/null 2>&1; then
    source /etc/profile.d/lmod.sh 2>/dev/null || source /opt/ohpc/admin/lmod/lmod/init/bash
  fi
  module use /home/software/tools/eb_modulefiles/all/Core 2>/dev/null || true
  module load GCC/10.2.0 OpenMPI/4.0.5 Singularity/3.10.2
  SINGULARITY_BIN=singularity
fi
"$SINGULARITY_BIN" pull --force "$DEMAGOGUES_SIF" "docker://ghcr.io/xmarquez/demagogues:$TAG"
echo "Pulled ghcr.io/xmarquez/demagogues:$TAG -> $DEMAGOGUES_SIF"
