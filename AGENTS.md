# Agent Notes (Codex)

- Shell is PowerShell: don't chain commands with `&&` (use separate commands or `;`).
- Pipeline basics:
  - Targets pipeline is configured via `config/pipeline.yml` and expanded into parameter tables by `object_parameters.R` (profile overrides via `TARGET_PROFILE`).
  - When reading from the targets store in scripts/reports, call `targets::tar_config_set(store = here::here("_targets"), config = "custom.yaml")` first.
  - Verify target names with `targets::tar_manifest()`; avoid hard-coding legacy targets that may no longer exist.
  - Execution backend is selected by `TARGETS_BACKEND` (unset/`local` = local crew workers; `slurm` = crew.cluster workers on Raapoi, used only inside `slurm/launch.sh`). All Slurm tuning lives in `cluster_resources.R`; the old future.batchtools/`batchtools.slurm.tmpl` path is retired to `attic/`. See `HPC.md` for the cluster runbook (deploy with `slurm/deploy.ps1`, fetch results with `slurm/fetch_results.ps1`).
- Reporting:
  - `Paper/Appendix.qmd` is Quarto and is rendered by the pipeline via `tarchetypes::tar_quarto()` in `_targets.R` (output `Paper/Appendix.md`).
  - Do not hand-edit `Paper/graph_appendix.rmd` (it is generated; regenerate from the pipeline instead).
- Avoid running full renders/pipeline steps unless requested; they can regenerate many `Paper/figure/*` artifacts.
- When committing only specific files: stage explicitly, verify with `git diff --cached --name-only`, and unstage accidental additions with `git restore --staged <path>`.
- Documentation convention for `R/`: add roxygen2 blocks for each function (and for S3 generics/methods) including `@param`/`@return` and any important assumptions (expected columns, required docvars, etc.).
- Quick sanity checks after edits:
  - R files: `Rscript -e "source('R/<file>.R')"`
  - YAML config: `Rscript -e "yaml::read_yaml('config/pipeline.yml')"`
