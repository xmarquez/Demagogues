#!/bin/bash
#
# Coordinator job for a full pipeline run on Raapoi.
#
# Submit from the login node (usually via slurm/deploy.ps1 on Windows):
#   sbatch [--time=...] [--export=ALL,TARGET_RUN=...,TARGET_PROFILE=...] slurm/launch.sh
#
# The coordinator runs targets::tar_make() INSIDE the Singularity container.
# crew.cluster submits worker jobs from inside the container through the ssh
# shims in slurm/shims (sbatch/squeue/scancel/sacct proxied to the login node).
# Workers re-exec themselves inside the same container (see cluster_resources.R).
#
# Store location: the targets store is the default `_targets/` inside the
# scratch clone. custom.yaml (which points at the Windows Dropbox path) is
# gitignored and therefore absent from the cluster clone, and TAR_CONFIG is
# deliberately NOT set here, so there is no risk of the store resolving to a
# Dropbox path on the cluster.
#
#SBATCH --job-name=demagogues-coordinator
#SBATCH --partition=parallel
#SBATCH --cpus-per-task=4
# 96G: not for resident memory. Slurm sets RLIMIT_AS (soft AND hard) to the
# memory request (VSizeFactor), and Quarto's embedded deno/V8 must RESERVE
# ~66G of virtual address space (sandbox + heap cages) or it aborts with
# "Fatal process out of memory: Oilpan: CagedHeap reservation". Verified
# empirically: 64G fails, 68G works; 96G leaves headroom for R itself.
#SBATCH --mem=96G
#SBATCH --time=2-00:00:00
#SBATCH --output=logs/coordinator-%j.log

set -u

# --- Configuration (override via sbatch --export=ALL,VAR=...) ----------------
export DEMAGOGUES_SCRATCH="${DEMAGOGUES_SCRATCH:-/nfs/scratch/marquexa/Demagogues}"
export DEMAGOGUES_SIF="${DEMAGOGUES_SIF:-/nfs/scratch/marquexa/containers/demagogues.sif}"
export TARGETS_BACKEND="${TARGETS_BACKEND:-slurm}"
export TARGET_RUN="${TARGET_RUN:-full_democracy}"
export TARGET_PROFILE="${TARGET_PROFILE:-full}"
export RESEARCH_DATA_ROOT="${RESEARCH_DATA_ROOT:-/nfs/scratch/marquexa/corpora}"
export TARGET_WORKERS_STD="${TARGET_WORKERS_STD:-25}"
export TARGET_WORKERS_BIGMEM="${TARGET_WORKERS_BIGMEM:-4}"
export DEMAGOGUES_SUBMIT_HOST="${DEMAGOGUES_SUBMIT_HOST:-${SLURM_SUBMIT_HOST:-raapoi.vuw.ac.nz}}"

cd "$DEMAGOGUES_SCRATCH" || exit 1
mkdir -p logs logs/crew exports

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

# Everything the pipeline reads from the environment must survive the
# `singularity exec` boundary. Singularity 3.10 passes most host env through,
# but SINGULARITYENV_* is the guaranteed, documented mechanism - belt and braces.
export SINGULARITYENV_PREPEND_PATH="$DEMAGOGUES_SCRATCH/slurm/shims"
export SINGULARITYENV_TARGETS_BACKEND="$TARGETS_BACKEND"
export SINGULARITYENV_TARGET_RUN="$TARGET_RUN"
export SINGULARITYENV_TARGET_PROFILE="$TARGET_PROFILE"
export SINGULARITYENV_RESEARCH_DATA_ROOT="$RESEARCH_DATA_ROOT"
export SINGULARITYENV_DEMAGOGUES_SIF="$DEMAGOGUES_SIF"
export SINGULARITYENV_DEMAGOGUES_SCRATCH="$DEMAGOGUES_SCRATCH"
export DEMAGOGUES_SINGULARITY="$SINGULARITY_BIN"
export SINGULARITYENV_DEMAGOGUES_SINGULARITY="$SINGULARITY_BIN"
export SINGULARITYENV_TARGET_WORKERS_STD="$TARGET_WORKERS_STD"
export SINGULARITYENV_TARGET_WORKERS_BIGMEM="$TARGET_WORKERS_BIGMEM"
export SINGULARITYENV_DEMAGOGUES_SUBMIT_HOST="$DEMAGOGUES_SUBMIT_HOST"

echo "== demagogues coordinator: run=$TARGET_RUN profile=$TARGET_PROFILE backend=$TARGETS_BACKEND"
echo "== scratch=$DEMAGOGUES_SCRATCH sif=$DEMAGOGUES_SIF submit_host=$DEMAGOGUES_SUBMIT_HOST"
echo "== git HEAD: $(git rev-parse HEAD 2>/dev/null || echo unknown)"

"$SINGULARITY_BIN" exec --bind /nfs/scratch "$DEMAGOGUES_SIF" \
  Rscript -e 'targets::tar_make()'
tar_make_exit=$?

echo "== tar_make finished with exit code $tar_make_exit"

# Bundle the headline results for transfer back to the Windows machine,
# even after a partial run (the bundle skips missing objects with a warning).
"$SINGULARITY_BIN" exec --bind /nfs/scratch "$DEMAGOGUES_SIF" \
  Rscript -e 'source("R/export_functions.R"); cat("bundle:", make_results_bundle(), "\n")'
bundle_exit=$?

echo "== bundle step finished with exit code $bundle_exit"
echo "== fetch with: .\\slurm\\fetch_results.ps1 (from the Windows project root)"

exit $tar_make_exit
