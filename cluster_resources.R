# cluster_resources.R
#
# Defines the {crew} controller group used by the targets pipeline. All Slurm
# tuning for VUW's Raapoi cluster lives here so _targets.R stays backend-neutral.
#
# Two backends, selected by the TARGETS_BACKEND environment variable:
#   * "slurm" : crew.cluster::crew_controller_slurm controllers that submit
#               worker jobs to Raapoi. Each worker re-execs itself inside the
#               Singularity container before reaching the R invocation line.
#   * "local" (default, incl. Windows dev): crew_controller_local controllers.
#
# Both backends expose a controller group with controllers named "std" and
# "bigmem", so per-target routing (tar_resources_crew(controller = ...)) and the
# default resource in _targets.R resolve identically in either mode.
#
# See HPC.md for the full architecture and the container re-exec rationale.

# --- Environment-driven configuration ---------------------------------------

# Absolute path to the Singularity image on the cluster (pulled from GHCR by
# slurm/setup_raapoi.sh). Resolved once here, at coordinator startup, and baked
# literally into the worker script (see slurm_script_lines): the sbatch call is
# proxied over ssh by slurm/shims/sbatch, so we cannot rely on Slurm propagating
# the coordinator's environment to the worker jobs.
demagogues_sif <- Sys.getenv(
  "DEMAGOGUES_SIF",
  "/nfs/scratch/marquexa/containers/demagogues.sif"
)

# Scratch clone of the repo on the cluster; also holds crew worker scripts/logs.
demagogues_scratch <- Sys.getenv(
  "DEMAGOGUES_SCRATCH",
  "/nfs/scratch/marquexa/Demagogues"
)

# Worker fan-out per controller (override via env for smoke tests).
workers_std <- suppressWarnings(as.integer(Sys.getenv("TARGET_WORKERS_STD", "25")))
workers_bigmem <- suppressWarnings(as.integer(Sys.getenv("TARGET_WORKERS_BIGMEM", "4")))
if (is.na(workers_std) || workers_std < 1L) workers_std <- 25L
if (is.na(workers_bigmem) || workers_bigmem < 1L) workers_bigmem <- 4L

# crew worker .sh scripts and Slurm logs MUST live on shared storage: the
# coordinator (inside the container) writes the script, and the compute node
# running the worker job must be able to read it. tempdir() inside the container
# is node-local and invisible to compute nodes, so we point at scratch.
demagogues_crew_dir <- file.path(demagogues_scratch, "logs", "crew")

# --- Worker-side container re-exec ------------------------------------------
#
# crew.cluster's generated worker script is:
#
#     #!/bin/sh
#     #SBATCH --job-name=...
#     ... #SBATCH directives ...
#     <script_lines>              <-- injected here
#     Rscript -e '<crew_worker call>'
#
# `Rscript` is resolved from PATH (confirmed by reading
# crew.cluster:::crew_class_launcher_cluster$launch_workers, v0.4.0). On a bare
# compute node PATH has no suitable R, so before that line we re-exec the whole
# script inside the container, where /usr/local/bin/Rscript exists. On re-exec
# the guard variable short-circuits, the #SBATCH lines are inert comments, and
# execution falls through to the Rscript line inside the container.
# Singularity is invoked by ABSOLUTE PATH (EasyBuild tree, NFS-shared to all
# nodes), not via `module load`: Lmod init is unreliable in non-interactive
# batch shells (missing shell function, bare MODULEPATH, non-set-u-safe init
# scripts - all observed on Raapoi during the first shakedown).
demagogues_singularity <- Sys.getenv(
  "DEMAGOGUES_SINGULARITY",
  "/home/software/EasyBuild/software/Singularity/3.10.2-gompi-2020b/bin/singularity"
)

# A shell function named Rscript shadows PATH lookup in crew's generated
# worker script, so its final `Rscript -e '<crew_worker call>'` line execs
# straight into the container. Do NOT re-exec "$0" instead: under sbatch, $0
# is Slurm's spooled script copy on node-local /var/lib/slurm, which does not
# exist inside the container (observed failure on the first shakedown).
# The `cd` matters: the sbatch shim submits over ssh from $HOME, so Slurm
# starts the worker there, but targets' store paths (_targets/objects/...)
# are relative to the project root.
slurm_script_lines <- c(
  # Workers do not inherit the coordinator's environment (submission goes
  # through the ssh shims), so pipeline env vars are baked in literally here.
  paste0(
    "export RESEARCH_DATA_ROOT=",
    shQuote(Sys.getenv("RESEARCH_DATA_ROOT", "/nfs/scratch/marquexa/corpora"))
  ),
  paste0(
    "Rscript() { cd ",
    shQuote(demagogues_scratch),
    " && exec ",
    shQuote(demagogues_singularity),
    " exec --bind /nfs/scratch ",
    shQuote(demagogues_sif),
    ' /usr/local/bin/Rscript "$@"; }'
  )
)

# --- Slurm controller group -------------------------------------------------

build_slurm_controller_group <- function() {
  if (!requireNamespace("crew.cluster", quietly = TRUE)) {
    stop("TARGETS_BACKEND=slurm requires the crew.cluster package.", call. = FALSE)
  }

  slurm_options <- function(partition, memory_gb_per_cpu, cpus, minutes) {
    crew.cluster::crew_options_slurm(
      partition = partition,
      memory_gigabytes_per_cpu = memory_gb_per_cpu,
      cpus_per_task = cpus,
      time_minutes = minutes,
      script_lines = slurm_script_lines,
      script_directory = demagogues_crew_dir,
      log_output = file.path(demagogues_scratch, "logs", "worker-%A_%a.out"),
      log_error = file.path(demagogues_scratch, "logs", "worker-%A_%a.err")
    )
  }

  crew::crew_controller_group(
    crew.cluster::crew_controller_slurm(
      name = "std",
      workers = workers_std,
      seconds_idle = 300,
      # Declare a launch lost (and relaunch) after 30 min without the worker
      # dialing in; the default grace is so long that a failed sbatch (e.g.
      # the exec-bit incident) stalls the pipeline for hours.
      seconds_launch = 1800,
      options_cluster = slurm_options(
        partition = "parallel",
        memory_gb_per_cpu = 6,
        cpus = 2,
        minutes = 600
      )
    ),
    crew.cluster::crew_controller_slurm(
      name = "bigmem",
      workers = workers_bigmem,
      seconds_idle = 300,
      # Declare a launch lost (and relaunch) after 30 min without the worker
      # dialing in; the default grace is so long that a failed sbatch (e.g.
      # the exec-bit incident) stalls the pipeline for hours.
      seconds_launch = 1800,
      options_cluster = slurm_options(
        partition = "bigmem",
        memory_gb_per_cpu = 40,
        cpus = 2,
        minutes = 600
      )
    )
    # Example third controller for CPU-parallel model fits (blas/xgboost). Add
    # `controller = "cpu10"` to the relevant targets to use it.
    # ,
    # crew.cluster::crew_controller_slurm(
    #   name = "cpu10",
    #   workers = 10,
    #   seconds_idle = 300,
    #   options_cluster = slurm_options(
    #     partition = "parallel",
    #     memory_gb_per_cpu = 1,
    #     cpus = 10,
    #     minutes = 540
    #   )
    # )
  )
}

# --- Local controller group (default / Windows dev) -------------------------

build_local_controller_group <- function() {
  local_workers <- suppressWarnings(as.integer(Sys.getenv("TARGET_WORKERS", "6")))
  if (is.na(local_workers) || local_workers < 1L) local_workers <- 6L

  crew::crew_controller_group(
    crew::crew_controller_local(
      name = "std",
      workers = local_workers,
      seconds_idle = 120,
      crashes_max = 10
    ),
    crew::crew_controller_local(
      name = "bigmem",
      workers = max(1L, min(2L, local_workers)),
      seconds_idle = 120,
      crashes_max = 10
    )
  )
}

#' Build the pipeline's crew controller group.
#'
#' Returns a `crew_controller_group` with controllers named "std" and "bigmem".
#' The Slurm backend is used when `TARGETS_BACKEND=slurm`; otherwise a local
#' group is returned. Constructing the Slurm controllers only happens on the
#' slurm branch, so validation on Windows (backend unset) never touches
#' crew.cluster.
#'
#' @return A `crew_controller_group` object.
#' @keywords internal
build_demagogues_controller <- function() {
  if (identical(Sys.getenv("TARGETS_BACKEND", "local"), "slurm")) {
    build_slurm_controller_group()
  } else {
    build_local_controller_group()
  }
}
