#!/bin/bash
#
# Smoke-test coordinator: small pilot run (auth_glmnet_40 / explore profile)
# on the quicktest partition. Validates the whole container + crew + shim
# chain in well under the 5 h quicktest limit before committing to a full run.
#
#   sbatch slurm/smoke.sh
#
#SBATCH --job-name=demagogues-smoke
#SBATCH --partition=quicktest
#SBATCH --cpus-per-task=4
#SBATCH --mem=16G
#SBATCH --time=04:00:00
#SBATCH --output=logs/coordinator-smoke-%j.log

set -u

export DEMAGOGUES_SCRATCH="${DEMAGOGUES_SCRATCH:-/nfs/scratch/marquexa/Demagogues}"
export DEMAGOGUES_SIF="${DEMAGOGUES_SIF:-/nfs/scratch/marquexa/containers/demagogues.sif}"
export TARGETS_BACKEND="${TARGETS_BACKEND:-slurm}"
export TARGET_RUN="${TARGET_RUN:-auth_glmnet_40}"
export TARGET_PROFILE="${TARGET_PROFILE:-explore}"
export RESEARCH_DATA_ROOT="${RESEARCH_DATA_ROOT:-/nfs/scratch/marquexa/corpora}"
# Small worker fan-out for the smoke test.
export TARGET_WORKERS_STD="${TARGET_WORKERS_STD:-4}"
export TARGET_WORKERS_BIGMEM="${TARGET_WORKERS_BIGMEM:-1}"
export DEMAGOGUES_SUBMIT_HOST="${DEMAGOGUES_SUBMIT_HOST:-${SLURM_SUBMIT_HOST:-raapoi.vuw.ac.nz}}"

cd "$DEMAGOGUES_SCRATCH" || exit 1
mkdir -p logs logs/crew exports

module load GCC/10.2.0 OpenMPI/4.0.5 Singularity/3.10.2

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

echo "== demagogues SMOKE: run=$TARGET_RUN profile=$TARGET_PROFILE backend=$TARGETS_BACKEND"
echo "== git HEAD: $(git rev-parse HEAD 2>/dev/null || echo unknown)"

singularity exec --bind /nfs/scratch "$DEMAGOGUES_SIF" \
  Rscript -e 'targets::tar_make()'
tar_make_exit=$?

echo "== tar_make finished with exit code $tar_make_exit"

singularity exec --bind /nfs/scratch "$DEMAGOGUES_SIF" \
  Rscript -e 'source("R/export_functions.R"); cat("bundle:", make_results_bundle(), "\n")'

exit $tar_make_exit
