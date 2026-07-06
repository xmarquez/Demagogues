# Results-bundle helpers for cluster runs.
#
# After a run on Raapoi, slurm/launch.sh calls make_results_bundle() inside the
# container to tar the targets metadata plus the small "headline" result
# objects. slurm/fetch_results.ps1 then scp's the bundle back to the Windows
# machine and extracts it into the local project, so tar_read() works locally
# without syncing the multi-GB store.

#' Headline targets worth syncing back from a cluster run
#'
#' Returns the names of the small, paper-facing objects in the targets store:
#' combined weights/performance/information-theory tables, their info/metadata
#' companions, and the corpus-stats summaries referenced by the paper. All
#' names are static targets in `_targets.R` except the per-feature workset
#' volume counts, which are generated as `workset_volume_count_<feature>` /
#' `workset_meta_volume_count_<feature>` (see `object_parameters.R`).
#'
#' @param features Character vector of feature names whose workset volume-count
#'   targets should be included. Defaults to `"democracy"` (the main paper's
#'   feature). Missing objects are skipped with a warning by
#'   [make_results_bundle()], so extra names are harmless.
#'
#' @return Character vector of target (object) names.
#' @seealso [make_results_bundle()]
headline_target_names <- function(features = "democracy") {
  static <- c(
    # Combined model outputs
    "combined_performance",
    "all_model_weights",
    "info_all_model_weights",
    "predictive_model_weights",
    "svd_model_weights",
    "ppmi_model_weights",
    "weight_correlations",
    "rank_agreements",
    "jsd_distances",
    "combined_kl",
    "combined_entropy",
    "sample_sizes",
    "info_performance",
    "info_graph",
    # Corpus stats used by the paper
    "democracy_text_percent",
    "democracy_words_per_million",
    "democracy_trans",
    "num_ht_bib_keys",
    "num_author_title",
    "num_htids_per_author",
    "num_libraries",
    "date_info",
    "total_texts"
  )
  feature_counts <- c(
    paste0("workset_volume_count_", features),
    paste0("workset_meta_volume_count_", features)
  )
  unique(c(static, feature_counts))
}

#' Bundle headline results from the targets store into a tar.gz archive
#'
#' Creates `exports/results_<run>_<timestamp>.tar.gz` containing
#' `_targets/meta/meta` plus every existing `_targets/objects/<name>` file for
#' the requested targets. Paths inside the archive are relative to the project
#' root (`_targets/meta/meta`, `_targets/objects/...`), so extracting the
#' archive at another project root drops the files straight into that
#' project's store.
#'
#' @param dir Output directory for the bundle (created if missing). Relative
#'   paths are resolved against `root`.
#' @param targets Character vector of object names to include. Defaults to
#'   [headline_target_names()].
#' @param store Path to the targets store, relative to `root`. Default
#'   `"_targets"`.
#' @param root Project root containing the store. Default `getwd()` (the
#'   coordinator runs from the scratch clone root).
#' @param run Run identifier used in the bundle filename. Defaults to the
#'   `TARGET_RUN` environment variable, falling back to `"run"`.
#'
#' @return Invisibly, the absolute path to the created bundle. Warns for each
#'   requested object missing from the store, and errors if the store metadata
#'   file is absent (nothing worth bundling without it).
make_results_bundle <- function(dir = "exports",
                                targets = headline_target_names(),
                                store = "_targets",
                                root = getwd(),
                                run = Sys.getenv("TARGET_RUN", "run")) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  meta_rel <- file.path(store, "meta", "meta")
  if (!file.exists(file.path(root, meta_rel))) {
    stop("No targets metadata at ", file.path(root, meta_rel),
         "; has the pipeline run in this store?", call. = FALSE)
  }

  object_rel <- file.path(store, "objects", targets)
  exists_mask <- file.exists(file.path(root, object_rel))
  missing <- targets[!exists_mask]
  if (length(missing)) {
    warning(
      "Skipping ", length(missing), " missing object(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  files <- c(meta_rel, object_rel[exists_mask])

  if (!dir.exists(file.path(root, dir)) && !grepl("^([A-Za-z]:)?[/\\\\]", dir)) {
    dir.create(file.path(root, dir), recursive = TRUE)
  }
  out_dir <- if (grepl("^([A-Za-z]:)?[/\\\\]", dir)) dir else file.path(root, dir)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  run_slug <- gsub("[^A-Za-z0-9]+", "_", run)
  bundle <- file.path(out_dir, sprintf("results_%s_%s.tar.gz", run_slug, timestamp))

  # tar() resolves `files` against the working directory; run from the project
  # root so archive members carry the relative _targets/... paths.
  old_wd <- setwd(root)
  on.exit(setwd(old_wd), add = TRUE)
  status <- utils::tar(
    tarfile = bundle,
    files = files,
    compression = "gzip",
    tar = "internal"
  )
  if (!identical(status, 0L) && !is.null(status) && !identical(status, 0)) {
    stop("utils::tar() returned non-zero status: ", status, call. = FALSE)
  }

  invisible(normalizePath(bundle, winslash = "/", mustWork = TRUE))
}
