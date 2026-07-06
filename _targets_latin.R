# Perseus/Latin prototype targets.

library(targets)
library(tarchetypes)
library(crew)
library(dplyr)
library(here)

target_workers <- as.integer(Sys.getenv("LATIN_TARGET_WORKERS", Sys.getenv("TARGET_WORKERS", "4")))
if (is.na(target_workers) || target_workers < 1) {
  target_workers <- 4
}

tar_option_set(
  packages = c(
    "dplyr", "ggplot2", "here", "knitr", "purrr", "readr", "stringr",
    "Matrix", "irlba", "quanteda", "tibble", "tidyr", "tidytext",
    "xml2", "yaml", "quarto"
  ),
  format = "rds",
  controller = crew::crew_controller_local(
    workers = target_workers,
    seconds_idle = 120,
    crashes_max = 5
  )
)

tar_source()

latin_run <- Sys.getenv("LATIN_RUN", "auctoritas_perseus")
latin_cfg <- read_latin_pipeline_config(run = latin_run)

latin_targets <- list(
  latin_ingest = list(
    tar_target(
      name = latin_config,
      command = latin_cfg,
      deployment = "main",
      description = "Latin: merged Perseus run configuration."
    ),
    tar_target(
      name = latin_perseus_repo,
      command = ensure_perseus_repository(
        repo_url = latin_config$source$repo_url,
        local_dir = latin_config$source$local_dir,
        ref = latin_config$source$ref %||% NULL,
        update = latin_config$source$update %||% FALSE
      ),
      format = "file",
      deployment = "main",
      description = "Latin ingest: local Perseus canonical Latin repository."
    ),
    tar_target(
      name = latin_perseus_manifest,
      command = list_perseus_latin_files(
        latin_perseus_repo,
        edition_language = latin_config$source$edition_language %||% "lat",
        max_files = latin_config$source$max_files %||% NULL
      ),
      deployment = "main",
      description = "Latin ingest: manifest of Perseus Latin TEI edition files."
    ),
    tar_target(
      name = latin_perseus_texts,
      command = read_perseus_latin_texts(latin_perseus_manifest, latin_config),
      deployment = "main",
      description = "Latin ingest: Perseus TEI texts with basic metadata."
    )
  ),
  latin_windows = list(
    tar_target(
      name = latin_tokens,
      command = tokenize_latin_texts(
        latin_perseus_texts,
        token_pattern = latin_config$preprocessing$token_pattern %||% "\\p{L}+"
      ),
      memory = "transient",
      garbage_collection = TRUE,
      description = "Latin windows: tokenized Perseus Latin texts."
    ),
    tar_target(
      name = latin_windows,
      command = extract_latin_windows(
        latin_tokens,
        feature_forms = latin_config$feature$forms,
        window_size = latin_config$windows$size %||% 50
      ),
      memory = "transient",
      garbage_collection = TRUE,
      description = "Latin windows: occurrence-centered context windows."
    ),
    tar_target(
      name = latin_window_summary,
      command = summarize_latin_windows(latin_windows),
      deployment = "main",
      description = "Latin windows: occurrence counts by period, author, and title."
    )
  ),
  latin_weights = list(
    tar_target(
      name = latin_context_term_counts,
      command = make_latin_context_term_counts(
        latin_windows,
        feature_forms = latin_config$feature$forms,
        stopwords = latin_config$preprocessing$stopwords %||% character(0),
        min_token_length = latin_config$preprocessing$min_token_length %||% 3
      ),
      deployment = "main",
      description = "Latin weights: cleaned context term counts by occurrence window."
    ),
    tar_target(
      name = latin_context_dfm,
      command = latin_context_counts_to_dfm(
        latin_context_term_counts,
        min_term_count = latin_config$svd$min_term_count %||% 2
      ),
      packages = c("quanteda"),
      deployment = "main",
      description = "Latin weights: window-term DFM for context SVD."
    ),
    tar_target(
      name = latin_global_corpus_term_counts,
      command = latin_corpus_term_counts(
        latin_tokens,
        group_vars = character(0),
        feature_forms = latin_config$feature$forms,
        stopwords = latin_config$preprocessing$stopwords %||% character(0),
        min_token_length = latin_config$preprocessing$min_token_length %||% 3
      ),
      deployment = "main",
      description = "Latin weights: full-corpus term counts for global PPMI baselines."
    ),
    tar_target(
      name = latin_period_corpus_term_counts,
      command = latin_corpus_term_counts(
        latin_tokens,
        group_vars = c("period", "period_label"),
        feature_forms = latin_config$feature$forms,
        stopwords = latin_config$preprocessing$stopwords %||% character(0),
        min_token_length = latin_config$preprocessing$min_token_length %||% 3
      ),
      deployment = "main",
      description = "Latin weights: full-corpus term counts for period PPMI baselines."
    ),
    tar_target(
      name = latin_global_ppmi_weights,
      command = latin_context_ppmi(
        latin_context_term_counts,
        latin_global_corpus_term_counts,
        group_vars = character(0),
        min_count = latin_config$ppmi$min_count %||% 2,
        min_corpus_count = latin_config$ppmi$min_corpus_count %||% 1,
        base = latin_config$ppmi$base %||% 2
      ),
      deployment = "main",
      description = "Latin weights: global PPMI associations for the target context."
    ),
    tar_target(
      name = latin_period_ppmi_weights,
      command = latin_context_ppmi(
        latin_context_term_counts,
        latin_period_corpus_term_counts,
        group_vars = c("period", "period_label"),
        min_count = latin_config$ppmi$min_count %||% 2,
        min_corpus_count = latin_config$ppmi$min_corpus_count %||% 1,
        base = latin_config$ppmi$base %||% 2
      ),
      deployment = "main",
      description = "Latin weights: period-specific PPMI associations for the target context."
    ),
    tar_target(
      name = latin_context_svd,
      command = compute_latin_context_svd(
        latin_context_dfm,
        dims = latin_config$svd$dims %||% 5,
        weight = latin_config$svd$weight %||% "ppmi",
        base = latin_config$svd$base %||% 10
      ),
      packages = c("Matrix", "irlba", "quanteda"),
      deployment = "main",
      description = "Latin weights: truncated SVD over the weighted context-window DFM."
    ),
    tar_target(
      name = latin_svd_terms,
      command = latin_svd_term_loadings(
        latin_context_svd,
        top_n = latin_config$svd$top_n %||% 12
      ),
      deployment = "main",
      description = "Latin weights: top positive and negative term loadings by SVD dimension."
    ),
    tar_target(
      name = latin_svd_windows,
      command = latin_svd_window_scores(latin_context_svd, latin_windows),
      deployment = "main",
      description = "Latin weights: context-window scores on retained SVD dimensions."
    ),
    tar_target(
      name = latin_period_term_weights,
      command = latin_window_term_weights(
        latin_windows,
        group_vars = c("period", "period_label"),
        feature_forms = latin_config$feature$forms,
        stopwords = latin_config$preprocessing$stopwords %||% character(0),
        min_token_length = latin_config$preprocessing$min_token_length %||% 3,
        min_count = latin_config$weights$min_count %||% 2
      ),
      deployment = "main",
      description = "Latin weights: context-term weights by broad period."
    ),
    tar_target(
      name = latin_author_term_weights,
      command = latin_window_term_weights(
        latin_windows,
        group_vars = c("author"),
        feature_forms = latin_config$feature$forms,
        stopwords = latin_config$preprocessing$stopwords %||% character(0),
        min_token_length = latin_config$preprocessing$min_token_length %||% 3,
        min_count = latin_config$weights$min_count %||% 2
      ),
      deployment = "main",
      description = "Latin weights: context-term weights by author."
    ),
    tar_target(
      name = latin_tracked_terms_period,
      command = latin_tracked_term_weights(
        latin_period_term_weights,
        tracked_terms = latin_config$feature$tracked_terms %||%
          latin_config$graphs$tracked_terms %||%
          character(0)
      ),
      deployment = "main",
      description = "Latin weights: configured tracked context terms by period."
    )
  ),
  latin_graph = list(
    tar_target(
      name = latin_report_qmd,
      command = write_latin_perseus_qmd(latin_config),
      format = "file",
      deployment = "main",
      description = "Latin graph: generated Quarto report for the Perseus run."
    ),
    tar_target(
      name = latin_report_html,
      command = {
        report_dependencies <- list(
          latin_window_summary,
          latin_global_ppmi_weights,
          latin_period_ppmi_weights,
          latin_svd_terms,
          latin_svd_windows,
          latin_period_term_weights,
          latin_author_term_weights,
          latin_tracked_terms_period,
          latin_windows
        )
        render_quarto_file(latin_report_qmd)
      },
      format = "file",
      packages = c("quarto"),
      deployment = "main",
      description = "Latin graph: rendered HTML report for the Perseus run."
    )
  )
)

enabled_outputs <- latin_cfg$outputs %||% names(latin_targets)
unknown_outputs <- setdiff(enabled_outputs, names(latin_targets))
if (length(unknown_outputs)) {
  stop("Unknown Latin output group(s): ", paste(unknown_outputs, collapse = ", "), call. = FALSE)
}

latin_targets[enabled_outputs]
