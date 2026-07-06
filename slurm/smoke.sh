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
# 96G: not for resident memory. Slurm sets RLIMIT_AS (soft AND hard) to the
# memory request (VSizeFactor), and Quarto's embedded deno/V8 must RESERVE
# ~66G of virtual address space (sandbox + heap cages) or it aborts with
# "Fatal process out of memory: Oilpan: CagedHeap reservation". Verified
# empirically: 64G fails, 68G works; 96G leaves headroom for R itself.
#SBATCH --mem=96G
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

echo "== demagogues SMOKE: run=$TARGET_RUN profile=$TARGET_PROFILE backend=$TARGETS_BACKEND"
echo "== git HEAD: $(git rev-parse HEAD 2>/dev/null || echo unknown)"

"$SINGULARITY_BIN" exec --bind /nfs/scratch "$DEMAGOGUES_SIF" \
  Rscript -e 'targets::tar_make()'
tar_make_exit=$?

echo "== tar_make finished with exit code $tar_make_exit"

"$SINGULARITY_BIN" exec --bind /nfs/scratch "$DEMAGOGUES_SIF" \
  Rscript -e 'source("R/export_functions.R"); cat("bundle:", make_results_bundle(), "\n")'

exit $tar_make_exit
