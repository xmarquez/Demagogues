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
#SBATCH --mem=16G
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

module use /home/software/tools/eb_modulefiles/all/Core 2>/dev/null || true
module load GCC/10.2.0 OpenMPI/4.0.5 Singularity/3.10.2

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
export SINGULARITYENV_TARGET_WORKERS_STD="$TARGET_WORKERS_STD"
export SINGULARITYENV_TARGET_WORKERS_BIGMEM="$TARGET_WORKERS_BIGMEM"
export SINGULARITYENV_DEMAGOGUES_SUBMIT_HOST="$DEMAGOGUES_SUBMIT_HOST"

echo "== demagogues coordinator: run=$TARGET_RUN profile=$TARGET_PROFILE backend=$TARGETS_BACKEND"
echo "== scratch=$DEMAGOGUES_SCRATCH sif=$DEMAGOGUES_SIF submit_host=$DEMAGOGUES_SUBMIT_HOST"
echo "== git HEAD: $(git rev-parse HEAD 2>/dev/null || echo unknown)"

singularity exec --bind /nfs/scratch "$DEMAGOGUES_SIF" \
  Rscript -e 'targets::tar_make()'
tar_make_exit=$?

echo "== tar_make finished with exit code $tar_make_exit"

# Bundle the headline results for transfer back to the Windows machine,
# even after a partial run (the bundle skips missing objects with a warning).
singularity exec --bind /nfs/scratch "$DEMAGOGUES_SIF" \
  Rscript -e 'source("R/export_functions.R"); cat("bundle:", make_results_bundle(), "\n")'
bundle_exit=$?

echo "== bundle step finished with exit code $bundle_exit"
echo "== fetch with: .\\slurm\\fetch_results.ps1 (from the Windows project root)"

exit $tar_make_exit
