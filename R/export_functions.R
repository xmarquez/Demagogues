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
#' companions, and the corpus-stats summaries referenced by the paper. The
#' feature-independent statics are constant target names in `_targets.R`; the
#' per-feature stats (workset volume counts and the bookworm corpus-stats
#' summaries) are generated per feature as `workset_volume_count_<feature>`,
#' `workset_meta_volume_count_<feature>`, `<feature>_words_per_million`,
#' `<feature>_text_percent`, and `<feature>_trans` (see `object_parameters.R`).
#'
#' @param features Character vector of feature names whose per-feature stats
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
    # Feature-independent corpus stats used by the paper
    "num_ht_bib_keys",
    "num_author_title",
    "num_htids_per_author",
    "num_libraries",
    "date_info",
    "total_texts"
  )
  feature_stats <- c(
    paste0("workset_volume_count_", features),
    paste0("workset_meta_volume_count_", features),
    paste0(features, "_words_per_million"),
    paste0(features, "_text_percent"),
    paste0(features, "_trans")
  )
  unique(c(static, feature_stats))
}

#' Rendered document outputs to ship home from a cluster run
#'
#' Collects the rendered paper/appendix artifacts under `<paper_dir>/`:
#' `*.md`, `*.html`, `*.pdf`, and the contents of any `*_files/` companion
#' directories (recursively, e.g. the figures/libs quarto emits alongside an
#' HTML render). Returns paths relative to `root` so they can be added to the
#' results bundle with the same project-relative layout as the store files.
#' Missing files simply do not appear in the result (no error, no warning).
#'
#' @param root Project root containing the paper directory.
#' @param paper_dir Sub-directory holding rendered documents, relative to
#'   `root`. Default `"Paper"`.
#'
#' @return Character vector of existing document paths relative to `root`
#'   (forward-slash separated), or `character(0)` when none are present.
#' @seealso [make_results_bundle()]
rendered_document_files <- function(root, paper_dir = "Paper") {
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  base <- file.path(root, paper_dir)
  if (!dir.exists(base)) {
    return(character(0))
  }

  doc_globs <- c("*.md", "*.html", "*.pdf")
  docs <- unlist(
    lapply(doc_globs, function(p) Sys.glob(file.path(base, p))),
    use.names = FALSE
  )
  docs <- docs[!dir.exists(docs)]

  companion_dirs <- Sys.glob(file.path(base, "*_files"))
  companion_dirs <- companion_dirs[dir.exists(companion_dirs)]
  companion_files <- unlist(
    lapply(companion_dirs, function(d) {
      list.files(d, recursive = TRUE, full.names = TRUE, all.files = TRUE, include.dirs = FALSE)
    }),
    use.names = FALSE
  )

  all_paths <- c(docs, companion_files)
  if (!length(all_paths)) {
    return(character(0))
  }
  all_paths <- normalizePath(all_paths, winslash = "/", mustWork = FALSE)
  rel <- sub(paste0("^", root, "/"), "", all_paths)
  unique(rel)
}

#' Bundle headline results from the targets store into a tar.gz archive
#'
#' Creates `exports/results_<run>_<timestamp>.tar.gz` containing
#' `_targets/meta/meta`, every existing `_targets/objects/<name>` file for
#' the requested targets, and any rendered document outputs under `Paper/`
#' (`*.md`, `*.html`, `*.pdf`, and their `*_files/` companion directories; see
#' [rendered_document_files()]). Paths inside the archive are relative to the
#' project root (`_targets/meta/meta`, `_targets/objects/...`, `Paper/...`), so
#' extracting the archive at another project root drops the files straight into
#' that project.
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
#'   file is absent (nothing worth bundling without it). Rendered documents are
#'   optional: absent ones are silently omitted.
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
  doc_files <- rendered_document_files(root)
  files <- c(meta_rel, object_rel[exists_mask], doc_files)

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
