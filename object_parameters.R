library(dplyr)
library(purrr)
library(tidyr)
library(tibble)
library(stringr)
library(yaml)
library(digest)

# Precomputed fiction IDs to avoid unbound symbols in corpus filters.
fiction_ids <- tryCatch(
  hathiTools::fiction %>%
    dplyr::filter(fiction_prop > 0.9) %>%
    dplyr::pull("htid"),
  error = function(e) character(0)
)

#' Null-coalescing operator
#'
#' Convenience infix operator used throughout this script to provide defaults
#' when a configuration field is `NULL`.
#'
#' @param x Value to test for `NULL`.
#' @param y Fallback value returned when `x` is `NULL`.
#'
#' @return `x` if it is not `NULL`, otherwise `y`.
#' @examples
#' NULL %||% 1
#' 2 %||% 1
#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x

# Helpers to create readable target names ------------------------------------

#' Slugify parameter values for target names
#'
#' Drops `NULL`/`NA`/empty values, concatenates the remainder with underscores,
#' and replaces non-alphanumeric characters with underscores. This yields stable,
#' human-readable fragments for target names.
#'
#' @param ... Values to include in the slug.
#'
#' @return A single character string.
#' @examples
#' slugify("dfm", "restricted", 500)
#' @keywords internal
slugify <- function(...) {
  vals <- c(...)
  vals <- vals[!map_lgl(vals, ~ is.null(.x) ||
                         (length(.x) == 1 && is.na(.x)) ||
                         (is.character(.x) && .x == ""))]
  vals <- map_chr(vals, as.character)
  paste(vals, collapse = "_") %>%
    str_replace_all("[^A-Za-z0-9]+", "_") %>%
    str_replace_all("_+", "_") %>%
    str_remove("^_+") %>%
    str_remove("_+$")
}

#' Add targets identifiers and symbols to a parameter table
#'
#' For a parameter grid, generate a stable `*_id` string and corresponding
#' `*_object` symbol used as dynamic target names in the targets pipeline.
#' If the generated name is longer than 180 characters, it is replaced by an
#' `xxhash32` digest to avoid Windows path length issues.
#'
#' @param df A data frame/tibble of parameters.
#' @param prefix Prefix for the generated columns (e.g., `"dfm"`).
#' @param name_cols Character vector of column names that define the identity.
#'
#' @return `df` with `<prefix>_id` and `<prefix>_object` columns added.
#' @examples
#' tibble(x = 1:2) %>% add_target("example", "x")
#' @keywords internal
add_target <- function(df, prefix, name_cols) {
  stopifnot(all(name_cols %in% names(df)))
  df %>%
    dplyr::ungroup() %>%
    mutate(
      target_name = pmap_chr(select(., all_of(name_cols)), ~ slugify(prefix, ...)),
      target_name = map_chr(target_name, ~ {
        nm <- .x
        if (nchar(nm) > 180) {
          paste0(prefix, "_", digest::digest(nm, algo = "xxhash32"))
        } else {
          nm
        }
      }),
      target_sym = map(target_name, rlang::sym),
      !!paste(prefix, "id", sep = "_") := target_name,
      !!paste(prefix, "object", sep = "_") := target_sym
    ) %>%
    select(-target_name, -target_sym)
}

#' Create human-readable titles for graph runs
#'
#' Adds a `graph_title` column combining feature, corpus/workset, sampling, DFM,
#' and model metadata. Used for labeling plots and reports.
#'
#' @param df A data frame containing the expected graph/model metadata columns.
#'
#' @return `df` with an added `graph_title` character column.
#' @keywords internal
create_titles <- function(df) {
  feature <- paste("Predicted feature:", df$feature_name)
  workset <- paste("workset:", df$workset_meta_id)
  max_vols <- paste("max vols per slice:", df$sample_max_vols)
  lowercase <- paste("lowercased:", df$dfm_to_lower)
  graph_pos_patterns <- paste("POS pattern:", df$graph_pos_patterns)
  sample_type <- paste("sample type:", df$sample_type)
  weight_type <- paste(
    "model type:",
    case_when(
      !is.na(df$predictive_model_engine) ~ df$predictive_model_engine,
      !is.na(df$svd_dims) ~ paste("svd", df$svd_weight, df$svd_dims, "dimensions"),
      TRUE ~ paste("ppmi", df$ppmi_fun)
    )
  )
  df$graph_title <- str_c(
    feature, workset, max_vols, lowercase, graph_pos_patterns, sample_type, weight_type,
    sep = ", "
  )
  df
}

#' Hash IDs into short target-name symbols
#'
#' Convenience helper to keep target names short. Takes an ID column, hashes it
#' with `digest::digest()` (default `xxhash32`), and stores a symbol with the
#' requested prefix in an object column.
#'
#' @param df A data frame/tibble.
#' @param id_col Column in `df` containing IDs to hash (unquoted).
#' @param object_col Column name to receive the symbol (unquoted).
#' @param prefix String prefix for the generated symbol (e.g., `"weight_pred_"`).
#' @param algo Hash algorithm passed to `digest::digest()`.
#'
#' @return `df` with `object_col` replaced by a list-column of symbols.
#' @keywords internal
hash_object_sym <- function(df, id_col, object_col, prefix, algo = "xxhash32") {
  id_col <- rlang::ensym(id_col)
  object_col <- rlang::ensym(object_col)
  df %>%
    rowwise() %>%
    mutate(
      .hash_tmp = digest::digest(!!id_col, algo = algo),
      !!object_col := list(rlang::sym(paste0(prefix, .hash_tmp)))
    ) %>%
    ungroup() %>%
    select(-.hash_tmp)
}

#' Deep merge two configuration lists
#'
#' Recursively merges `override` into `base`. Non-list values in `override`
#' replace those in `base`; list entries are merged element-wise.
#'
#' @param base Base list.
#' @param override Override list.
#'
#' @return A merged list.
#' @keywords internal
deep_merge <- function(base, override) {
  if (is.null(base)) return(override)
  if (is.null(override)) return(base)
  if (!is.list(base) || !is.list(override)) return(override)
  nm <- union(names(base), names(override))
  out <- vector("list", length(nm))
  names(out) <- nm
  for (n in nm) {
    out[[n]] <- deep_merge(base[[n]], override[[n]])
  }
  out
}

#' Normalize predictive-model engine specifications
#'
#' Predictive-model engines may be configured in two forms in the YAML config:
#' - scalar form: `engines: [glmnet, naivebayes]` (no per-engine params); and
#' - mapping form: a sequence of `{ name: <engine>, params: { ... } }` entries.
#'
#' This helper collapses either form (or a mix) into one canonical
#' representation: a list of `list(name = <chr>, params = <named list>)` entries,
#' with an empty `params` list when none are supplied. Downstream code therefore
#' only ever sees the mapping form.
#'
#' @param engines Raw engines value from the merged config (a character vector,
#'   a list of scalar strings, or a list of `name`/`params` mappings).
#'
#' @return A list of `list(name, params)` entries (possibly empty).
#' @keywords internal
normalize_engines <- function(engines) {
  if (is.null(engines) || length(engines) == 0) {
    return(list())
  }
  purrr::map(engines, function(e) {
    if (is.list(e)) {
      list(
        name = as.character(e$name %||% e[[1]]),
        params = as.list(e$params %||% list())
      )
    } else {
      list(name = as.character(e), params = list())
    }
  })
}

#' Resolve engines across a base/profile merge (wholesale override)
#'
#' A profile-level `engines` override replaces the base engine list wholesale
#' (rather than being merged element-wise). This preserves that semantic
#' regardless of whether either side uses the scalar or mapping form, then
#' normalizes the winning list via [normalize_engines()].
#'
#' @param base_engines Base (pre-profile) engines value.
#' @param override_engines Profile-level engines value, or `NULL` when the
#'   profile does not override engines.
#'
#' @return A normalized list of `list(name, params)` entries.
#' @keywords internal
normalize_engines_for_merge <- function(base_engines, override_engines) {
  chosen <- if (!is.null(override_engines)) override_engines else base_engines
  normalize_engines(chosen)
}

#' Read a YAML file when it exists
#'
#' @param path Path to a YAML file.
#' @param required Logical; if `TRUE`, error when `path` does not exist.
#'
#' @return Parsed YAML list, or `NULL` when optional and absent.
#' @keywords internal
read_yaml_config <- function(path, required = TRUE) {
  if (!file.exists(path)) {
    if (required) {
      stop("Missing YAML config file: ", path, call. = FALSE)
    }
    return(NULL)
  }
  yaml::read_yaml(path)
}

#' Read legacy monolithic pipeline configuration
#'
#' Loads `config/pipeline.yml`, takes the `base` section, and applies any
#' profile-specific overrides from `profiles[[profile]]` via `deep_merge()`.
#'
#' @param profile Name of the profile to apply.
#' @param path Path to the legacy YAML configuration file.
#'
#' @return A list of configuration values ready to be expanded into parameter
#'   tables.
#' @keywords internal
read_legacy_pipeline_config <- function(profile = Sys.getenv("TARGET_PROFILE", "full"),
                                        path = "config/pipeline.yml") {
  raw <- yaml::read_yaml(path)
  base_cfg <- raw$base %||% raw
  profile_cfg <- raw$profiles[[profile]] %||% list()
  cfg <- deep_merge(base_cfg, profile_cfg)
  # Normalize engines to the canonical name/params form immediately after the
  # merge. A profile override replaces the base engine list wholesale.
  cfg$predictive_models$engines <- normalize_engines_for_merge(
    base_cfg$predictive_models$engines,
    profile_cfg$predictive_models$engines
  )
  cfg$profile <- profile
  cfg$run_id <- profile
  cfg$run_description <- cfg$description %||% raw$description %||% ""
  cfg$config_source <- path
  cfg
}

#' Read all feature definitions from split config files
#'
#' @param feature_dir Directory containing one feature YAML file per feature.
#'
#' @return A named list of feature configurations keyed by feature ID.
#' @keywords internal
read_feature_configs <- function(feature_dir = "config/features") {
  files <- Sys.glob(file.path(feature_dir, "*.yml"))
  files <- c(files, Sys.glob(file.path(feature_dir, "*.yaml")))
  if (!length(files)) {
    stop("No feature YAML files found in ", feature_dir, call. = FALSE)
  }

  features <- purrr::map(files, yaml::read_yaml)
  ids <- purrr::map2_chr(features, files, function(feature, path) {
    feature$id %||% feature$name %||% tools::file_path_sans_ext(basename(path))
  })

  features <- purrr::map2(features, ids, function(feature, id) {
    feature$id <- feature$id %||% id
    feature$name <- feature$name %||% feature$id
    feature
  })
  rlang::set_names(features, ids)
}

#' Read split corpus, feature, and run configuration
#'
#' Loads shared corpus/default settings, selects one or more feature YAML files,
#' and applies run-specific overrides. The run is selected with `TARGET_RUN`.
#'
#' @param run Run ID, corresponding to `config/runs/<run>.yml`.
#' @param profile Optional profile override, preserving the old `TARGET_PROFILE`
#'   behavior when profiles are present in the shared config.
#' @param config_dir Directory containing `corpus.yml`, `features/`, and `runs/`.
#'
#' @return A merged configuration list.
#' @keywords internal
read_split_pipeline_config <- function(run = Sys.getenv("TARGET_RUN", "full_democracy"),
                                       profile = Sys.getenv("TARGET_PROFILE", "full"),
                                       config_dir = "config") {
  corpus_cfg <- read_yaml_config(file.path(config_dir, "corpus.yml"))
  run_path <- file.path(config_dir, "runs", paste0(run, ".yml"))
  run_cfg <- read_yaml_config(run_path)

  run_feature_ids <- run_cfg$feature_ids %||% run_cfg$features
  if (is.null(run_feature_ids) || !length(run_feature_ids)) {
    stop("Run config must declare `features` or `feature_ids`: ", run_path, call. = FALSE)
  }

  feature_configs <- read_feature_configs(file.path(config_dir, "features"))
  missing_features <- setdiff(run_feature_ids, names(feature_configs))
  if (length(missing_features)) {
    stop(
      "Run `", run, "` references unknown feature(s): ",
      paste(missing_features, collapse = ", "),
      call. = FALSE
    )
  }

  metadata_fields <- c(
    "id", "description", "features", "feature_ids", "outputs", "overrides"
  )
  direct_overrides <- run_cfg[setdiff(names(run_cfg), metadata_fields)]

  cfg <- deep_merge(corpus_cfg, direct_overrides)
  cfg <- deep_merge(cfg, run_cfg$overrides %||% list())

  # Engines resolved by the corpus/run layers (before the profile is applied).
  pre_profile_engines <- cfg$predictive_models$engines

  profile_cfg <- cfg$profiles[[profile]] %||% list()
  cfg <- deep_merge(cfg, profile_cfg)
  cfg$profiles <- NULL

  # Normalize engines to the canonical name/params form immediately after the
  # merge. A profile override replaces the corpus/run engine list wholesale.
  cfg$predictive_models$engines <- normalize_engines_for_merge(
    pre_profile_engines,
    profile_cfg$predictive_models$engines
  )

  cfg$features <- feature_configs[run_feature_ids]
  cfg$outputs <- run_cfg$outputs %||% cfg$outputs %||% NULL
  cfg$profile <- profile
  cfg$run_id <- run_cfg$id %||% run
  cfg$run_description <- run_cfg$description %||% ""
  cfg$config_source <- run_path
  cfg
}

#' Read and merge pipeline configuration
#'
#' Prefer the split configuration layout (`corpus.yml`, `features/`, `runs/`)
#' when present. Set `TARGET_CONFIG=config/pipeline.yml` to force the legacy
#' monolithic YAML file.
#'
#' @param profile Profile override name.
#' @param run Split-config run ID.
#' @param config_dir Split configuration directory.
#' @param path Optional explicit legacy configuration path.
#'
#' @return A list of configuration values ready to be expanded into parameter
#'   tables.
#' @keywords internal
read_pipeline_config <- function(profile = Sys.getenv("TARGET_PROFILE", "full"),
                                 run = Sys.getenv("TARGET_RUN", "full_democracy"),
                                 config_dir = "config",
                                 path = Sys.getenv("TARGET_CONFIG", "")) {
  run <- tools::file_path_sans_ext(basename(run))

  if (nzchar(path)) {
    return(read_legacy_pipeline_config(profile = profile, path = path))
  }

  split_available <- file.exists(file.path(config_dir, "corpus.yml")) &&
    file.exists(file.path(config_dir, "runs", paste0(run, ".yml")))

  if (split_available) {
    return(read_split_pipeline_config(
      run = run,
      profile = profile,
      config_dir = config_dir
    ))
  }

  read_legacy_pipeline_config(profile = profile, path = file.path(config_dir, "pipeline.yml"))
}

#' Get named terms to track in run-level reports
#'
#' Combines optional feature-level `graphs.tracked_terms` settings with any
#' run-level/corpus-level `graphs.tracked_terms` override. These terms are
#' report metadata only: they are intentionally not included in `feature_df`, so
#' changing them does not alter worksets, DFMs, or model targets.
#'
#' @param cfg Merged pipeline configuration.
#'
#' @return Character vector of unique terms to plot in the rendered report.
#' @keywords internal
get_run_tracked_terms <- function(cfg) {
  feature_terms <- cfg$features %||% list()
  feature_terms <- purrr::map(feature_terms, function(feature) {
    graph_cfg <- feature$graphs %||% list()
    c(
      graph_cfg$tracked_terms %||% character(0),
      feature$tracked_terms %||% character(0)
    )
  }) |>
    unlist(use.names = FALSE)

  terms <- c(
    feature_terms,
    cfg$graphs$tracked_terms %||% character(0)
  )

  terms <- as.character(terms)
  unique(terms[!is.na(terms) & nzchar(terms)])
}

#' Get top-term count for run-level report trajectories
#'
#' Uses `graphs.tracked_top_n` from the merged config when available, falling
#' back to the first feature-level value and then to ten terms.
#'
#' @param cfg Merged pipeline configuration.
#'
#' @return Integer number of top terms to include per predictive model/POS group.
#' @keywords internal
get_run_tracked_top_n <- function(cfg) {
  feature_values <- cfg$features %||% list()
  feature_values <- purrr::map(feature_values, function(feature) {
    graph_cfg <- feature$graphs %||% list()
    graph_cfg$tracked_top_n %||% feature$tracked_top_n %||% NULL
  }) |>
    purrr::compact() |>
    unlist(use.names = FALSE)

  value <- cfg$graphs$tracked_top_n %||%
    if (length(feature_values)) feature_values[[1]] else 10

  as.integer(value)
}

# --- Load YAML-driven config -------------------------------------------------

cfg <- read_pipeline_config()
run_tracked_terms <- get_run_tracked_terms(cfg)
run_tracked_top_n <- get_run_tracked_top_n(cfg)

# Normalization scheme for turning signed model weights into probability-like
# vectors (see normalize_weights()). Referenced in _targets.R weight commands so
# that changing the config value invalidates the affected weight targets.
weight_normalization_method <- cfg$weights$normalization %||% "positive"

# Features --------------------------------------------------------------------

feature_params <- map_dfr(cfg$features, function(f) {
  feature_name <- f$name %||% f$id
  search_terms <- f$search_terms %||% f$tokens %||% feature_name
  tibble(
    feature_name = feature_name,
    feature_list = list(f$dictionary),
    feature_search_terms = list(as.character(search_terms)),
    feature_search_join = f$search_join %||% f$token_join %||%
      if (length(search_terms) > 1) "OR" else "AND",
    feature_pages_contain = f$pages_contain %||% feature_name,
    feature_description = f$description %||% ""
  )
})

feature_df <- feature_params %>%
  add_target("feature", "feature_name")

# Periods ---------------------------------------------------------------------

period_params <- tibble(
  sample_min_year = cfg$periods$sample_min_year,
  sample_max_year = cfg$periods$sample_max_year,
  sample_slice_size = cfg$periods$sample_slice_size,
  period_name = cfg$periods$period_name
) %>%
  add_target("period", c("period_name", "sample_min_year", "sample_max_year", "sample_slice_size"))

# Corpora ---------------------------------------------------------------------

corpus_df <- map_dfr(cfg$corpora, function(corp) {
  tibble(
    corpus_id = corp$id,
    corpus_filter = list(rlang::parse_expr(corp$filter %||% "TRUE")),
    corpus_description = corp$description %||% ""
  )
}) %>%
  add_target("corpus", "corpus_id")

# Worksets --------------------------------------------------------------------

workset_settings <- tibble(
  workset_method = cfg$worksets$method %||% "auto",
  workset_method_id = dplyr::if_else(workset_method == "auto", "", workset_method)
)

workset_df <- expand_grid(
  feature_df,
  period_params,
  corpus_df,
  workset_settings
) %>%
  add_target("workset", c("feature_id", "corpus_id", "period_id", "workset_method_id"))

workset_meta_df <- workset_df %>%
  add_target("workset_meta", c("workset_id", "corpus_id"))

# Stable, readable summary target names for volume counts referenced in the paper
# (replacing hashed workset/workset_meta target names). One target per
# feature x corpus; slugs use the feature name explicitly.
workset_count_df <- workset_df %>%
  mutate(workset_volume_count_object = purrr::map(
    feature_name, ~ rlang::sym(paste0("workset_volume_count_", .x))
  ))

workset_meta_count_df <- workset_meta_df %>%
  mutate(workset_meta_volume_count_object = purrr::map(
    feature_name, ~ rlang::sym(paste0("workset_meta_volume_count_", .x))
  ))

# Corpus stats ----------------------------------------------------------------

# Time bounds for the bookworm corpus-stats queries. Configurable via
# `corpus_stats.lims` (default 1700-2020, preserving the historical query). Held
# as a `c(<min>, <max>)` call so tar_eval() splices it verbatim into the
# query_bookworm() commands.
corpus_stats_lims <- as.numeric(cfg$corpus_stats$lims %||% c(1700, 2020))
corpus_stats_lims_call <- rlang::call2("c", !!!as.list(corpus_stats_lims))

# One row per run feature carrying the bookworm search-term call and the four
# stable summary target-name symbols (`<feature>_word_counts`,
# `<feature>_words_per_million`, `<feature>_text_counts`, `<feature>_text_percent`).
# Search terms come from an explicit `bookworm_terms` key, else the feature's
# search terms (matching feature_params), else the feature name. They are wrapped
# in a `c(...)` call so the emitted commands read query_bookworm(c("democracy"), ...)
# exactly as the historical hard-coded targets did.
corpus_stats_df <- purrr::map_dfr(cfg$features, function(f) {
  feature_name <- f$name %||% f$id
  search_terms <- f$search_terms %||% f$tokens %||% feature_name
  terms <- as.character(f$bookworm_terms %||% search_terms)
  tibble(
    feature_name = feature_name,
    bookworm_terms = list(rlang::call2("c", !!!as.list(terms))),
    corpus_stats_lims = list(corpus_stats_lims_call),
    word_counts_object = list(rlang::sym(paste0(feature_name, "_word_counts"))),
    words_per_million_object = list(rlang::sym(paste0(feature_name, "_words_per_million"))),
    text_counts_object = list(rlang::sym(paste0(feature_name, "_text_counts"))),
    text_percent_object = list(rlang::sym(paste0(feature_name, "_text_percent")))
  )
})

# Optional per-feature translations sources. Only features whose config declares a
# `translations_file` produce `<feature>_translations` (a "file" target) and
# `<feature>_trans` (a frequency table) targets. Empty (0x0) tibble when no
# feature defines one; _targets.R guards on nrow() before emitting the tar_eval.
translations_features <- purrr::keep(cfg$features, ~ !is.null(.x$translations_file))
corpus_stats_translations_df <- purrr::map_dfr(translations_features, function(f) {
  feature_name <- f$name %||% f$id
  tibble(
    feature_name = feature_name,
    translations_file = as.character(f$translations_file),
    translations_object = list(rlang::sym(paste0(feature_name, "_translations"))),
    trans_object = list(rlang::sym(paste0(feature_name, "_trans")))
  )
})

# Sample parameters -----------------------------------------------------------

# Volume deduplication strategy (default: none). Kept multiplicities are a
# popularity signal; dedupe is a robustness flag. `sample_dedupe_label` is "" for
# the default so it is dropped by slugify() and default sample target names are
# unchanged.
sample_dedupe <- cfg$samples$dedupe %||% "none"

# Expand max vols x sample types, then assign multipliers by type order.
sample_params <- expand_grid(
  sample_max_vols = as.numeric(cfg$samples$sample_max_vols),
  sample_type = cfg$samples$sample_type
) %>%
  mutate(
    sample_multiplier = {
      mult <- as.numeric(cfg$samples$sample_multiplier)
      types <- cfg$samples$sample_type
      if (length(mult) == 1) {
        rep(mult, length(types))
      } else if (length(mult) != length(types)) {
        rep(mult[1], length(types))
      } else {
        mult
      }
    }[match(sample_type, cfg$samples$sample_type)],
    sample_dedupe = sample_dedupe,
    sample_dedupe_label = dplyr::if_else(sample_dedupe == "none", "", sample_dedupe)
  )

# Independent volume-sample replicates for uncertainty quantification (P3). No
# explicit seed plumbing: targets derives each target's RNG seed from its name,
# and the rep label changes reps 2..n names, so each replicate samples volumes
# independently. `samples_repeats_scope` gates which rows are replicated:
# "headline" (default) replicates only the model-bearing restricted chain at the
# maximum volume cap (rep 1 exists for every row as before); "all" replicates
# every sample row.
samples_n_repeats <- max(1L, as.integer(cfg$samples$n_repeats %||% 1L))
samples_repeats_scope <- cfg$samples$repeats_scope %||% "headline"
sample_max_cap <- max(sample_params$sample_max_vols, na.rm = TRUE)

sample_params <- sample_params %>%
  mutate(
    .rep_eligible = if (identical(samples_repeats_scope, "all")) {
      TRUE
    } else {
      sample_type == "restricted" & sample_max_vols == sample_max_cap
    },
    sample_rep = purrr::map(.rep_eligible, ~ if (isTRUE(.x)) seq_len(samples_n_repeats) else 1L)
  ) %>%
  tidyr::unnest(sample_rep) %>%
  mutate(
    sample_rep = as.integer(sample_rep),
    # Empty label for rep 1 (dropped by slugify) keeps rep-1 target names
    # byte-identical; reps 2..n get a `repN` fragment, following the
    # sample_dedupe_label empty-when-default convention.
    sample_rep_label = dplyr::if_else(sample_rep == 1L, "", paste0("rep", sample_rep))
  ) %>%
  select(-.rep_eligible)

sample_df <- expand_grid(workset_meta_df, sample_params) %>%
  add_target(
    "sample",
    c("workset_meta_id", "sample_type", "sample_max_vols", "sample_dedupe_label", "sample_rep_label")
  )

files_df <- sample_df %>%
  add_target("files", "sample_id")

# DFM parameters --------------------------------------------------------------

resolve_feature_placeholder <- function(value,
                                        feature_name,
                                        feature_pages_contain,
                                        feature_search_terms) {
  value <- as.character(value)
  value <- stringr::str_replace_all(
    value,
    stringr::fixed("{feature_name}"),
    feature_name
  )
  value <- stringr::str_replace_all(
    value,
    stringr::fixed("{feature_pages_contain}"),
    feature_pages_contain
  )
  value <- stringr::str_replace_all(
    value,
    stringr::fixed("{feature_search_terms}"),
    paste(feature_search_terms, collapse = "|")
  )
  value
}

expand_dfm_plan <- function(plan, dfm_defaults, sample_defaults) {
  plan <- deep_merge(dfm_defaults, plan)
  tibble(dfm_plan_id = plan$id %||% "default") %>%
    expand_grid(
      dfm_sample_max_vols = as.numeric(plan$sample_max_vols %||% sample_defaults$sample_max_vols),
      dfm_sample_type = plan$sample_type %||% sample_defaults$sample_type,
      dfm_vocab_size = plan$vocab_size,
      dfm_feature_pos_pattern = plan$pos_pattern,
      dfm_feature_include_pattern = plan$include_pattern,
      dfm_pages_contain = plan$pages_contain,
      dfm_feature_min_length = plan$min_length,
      dfm_page_language = plan$page_language,
      dfm_page_min_sentence_count = plan$min_sentence_count,
      dfm_to_lower = plan$to_lower
    )
}

if (!is.null(cfg$dfm$plans)) {
  dfm_params_base <- purrr::map_dfr(
    cfg$dfm$plans,
    expand_dfm_plan,
    dfm_defaults = cfg$dfm,
    sample_defaults = cfg$samples
  )

  dfm_df <- expand_grid(files_df, dfm_params_base) %>%
    filter(
      sample_max_vols == dfm_sample_max_vols,
      sample_type == dfm_sample_type
    ) %>%
    rowwise() %>%
    mutate(
      dfm_pages_contain = resolve_feature_placeholder(
        dfm_pages_contain,
        feature_name,
        feature_pages_contain,
        feature_search_terms
      )
    ) %>%
    ungroup() %>%
    select(-dfm_sample_max_vols, -dfm_sample_type) %>%
    distinct()
} else {
  dfm_params_base <- expand_grid(
    dfm_vocab_size = cfg$dfm$vocab_size,
    dfm_feature_pos_pattern = cfg$dfm$pos_pattern,
    dfm_feature_include_pattern = cfg$dfm$include_pattern,
    dfm_pages_contain = cfg$dfm$pages_contain,
    dfm_feature_min_length = cfg$dfm$min_length,
    dfm_page_language = cfg$dfm$page_language,
    dfm_page_min_sentence_count = cfg$dfm$min_sentence_count,
    dfm_to_lower = cfg$dfm$to_lower
  )

  min_sample_cap <- min(sample_params$sample_max_vols, na.rm = TRUE)

  files_small_df <- files_df %>% filter(sample_max_vols == min_sample_cap)
  files_large_df <- files_df %>% filter(sample_max_vols != min_sample_cap)

  dfm_small_df <- expand_grid(files_small_df, dfm_params_base) %>%
    rowwise() %>%
    mutate(
      dfm_pages_contain = resolve_feature_placeholder(
        dfm_pages_contain,
        feature_name,
        feature_pages_contain,
        feature_search_terms
      )
    ) %>%
    ungroup() %>%
    filter(dfm_pages_contain == ".")

  dfm_large_df <- expand_grid(files_large_df, dfm_params_base) %>%
    rowwise() %>%
    mutate(
      dfm_pages_contain = resolve_feature_placeholder(
        dfm_pages_contain,
        feature_name,
        feature_pages_contain,
        feature_search_terms
      )
    ) %>%
    ungroup() %>%
    filter(dfm_to_lower, dfm_pages_contain == feature_pages_contain)

  dfm_df <- bind_rows(dfm_small_df, dfm_large_df)
}

if (!exists("min_sample_cap")) {
  min_sample_cap <- min(sample_params$sample_max_vols, na.rm = TRUE)
}

dfm_df <- dfm_df %>%
  add_target("dfm", c("sample_id", "dfm_to_lower", "dfm_vocab_size", "dfm_pages_contain"))

# Splits params ---------------------------------------------------------------

split_df <- dfm_df %>%
  group_by(dfm_id) %>%
  mutate(split_params = tibble(
    split_downsample = FALSE,
    split_downsample_type = NA_character_
  ) %>%
    list()) %>%
  unnest(split_params) %>%
  add_target("split", c("dfm_id", "split_downsample"))

# Predictive models -----------------------------------------------------------

# Stable 8-char hash of a (canonicalized) engine params list. Empty params hash
# to "" so slugify() drops the fragment and default/scalar-form configs keep the
# same target names as before this parameterization was introduced. Names are
# sorted before hashing so YAML key order does not affect the id.
predictive_model_params_id <- function(params) {
  if (length(params) == 0) {
    return("")
  }
  canonical <- params[order(names(params))]
  substr(digest::digest(canonical, algo = "xxhash32"), 1, 8)
}

# Engine tibble carries each engine name alongside its (possibly empty) params
# list, keeping name and params paired through the grid expansion.
engines_tbl <- tibble(
  predictive_model_engine = purrr::map_chr(cfg$predictive_models$engines, "name"),
  predictive_model_params = purrr::map(cfg$predictive_models$engines, "params")
)

predictive_model_params <- expand_grid(
  predictive_model_task = cfg$predictive_models$task,
  predictive_model_dfm_weight = cfg$predictive_models$weights,
  engines_tbl
)

predictive_model_df <- expand_grid(split_df, predictive_model_params) %>%
  mutate(
    predictive_model_params_hash = purrr::map_chr(predictive_model_params, predictive_model_params_id)
  ) %>%
  add_target(
    "predictive_model",
    c("split_id", "predictive_model_engine", "predictive_model_dfm_weight", "predictive_model_params_hash")
  ) %>%
  mutate(
    predictive_model_object = predictive_model_object %>%
      purrr::map(~ {
        original <- rlang::as_name(.x)
        hashed <- digest::digest(original, algo = "xxhash32")
        rlang::sym(paste0("predictive_model_", hashed))
      })
  )

# Shared sample cap for restricted model/graph outputs ------------------------

max_sample_cap <- max(sample_params$sample_max_vols, na.rm = TRUE)

# SVD -------------------------------------------------------------------------

svd_params <- expand_grid(
  svd_weight = cfg$svd$weights,
  svd_dims = cfg$svd$dims
)

svd_df <- expand_grid(split_df, svd_params) %>%
  add_target("svd", c("split_id", "svd_weight", "svd_dims")) %>%
  filter(sample_type == "restricted", sample_max_vols == max_sample_cap,
         svd_weight == "ppmi")

# Weights ---------------------------------------------------------------------

predictive_model_weights_df <- predictive_model_df %>%
  filter(sample_type == "restricted", sample_max_vols == max_sample_cap,
         predictive_model_dfm_weight == "ppmi") %>%
  add_target("weight_pred", "predictive_model_id") %>%
  hash_object_sym(weight_pred_id, weight_pred_object, prefix = "weight_pred_")

# Reduced DFMs for topic modeling --------------------------------------------

reduced_dfm_df <- predictive_model_weights_df %>%
  expand_grid(reduced_dfm_vocab_per_period = cfg$topic_models$reduced_vocab %||% 50) %>%
  add_target("reduced_dfm", c("weight_pred_id", "reduced_dfm_vocab_per_period")) %>%
  filter(sample_max_vols == max_sample_cap)  %>%
  hash_object_sym(reduced_dfm_id, reduced_dfm_object, prefix = "reduced_dfm_")

# Topic models ----------------------------------------------------------------

topic_model_df <- reduced_dfm_df %>%
  expand_grid(topic_model_K = cfg$topic_models$K) %>%
  add_target("topic_model", c("reduced_dfm_id", "topic_model_K"))

# Predictive model performance ------------------------------------------------

performance_splits <- cfg$performance$splits %||% c("testing", "training")

# Standard (in-sample) performance rows: evaluated on the model's own DFM,
# selecting the training/testing partition via the split. These get their own
# tar_eval in _targets.R.
performance_standard_df <- predictive_model_df %>%
  expand_grid(performance_split = performance_splits) %>%
  add_target("performance", c("predictive_model_id", "performance_split")) %>%
  hash_object_sym(performance_id, performance_object, prefix = "performance_predictive_model_")

performance_wild_sample_df <- predictive_model_df %>%
  filter(sample_type == "restricted")

testing_dfms <- dfm_df %>%
  filter(
    sample_type == "unrestricted",
    sample_max_vols == min_sample_cap,
    # Pair every model replicate with the rep-1 unrestricted testing DFM. Under
    # "headline" scope the unrestricted chain only has rep 1, so reps 2..n would
    # otherwise lose their OOD match; filtering the testing side to rep 1 (and
    # dropping sample_rep in the transmute below) neutralizes the rep column so
    # the join on workset_meta_id + dfm_to_lower matches all reps to one DFM.
    sample_rep == 1L,
    dfm_to_lower %in% performance_wild_sample_df$dfm_to_lower
  ) %>%
  transmute(
    workset_meta_id,
    dfm_to_lower,
    testing_dfm_id = dfm_id,
    testing_dfm_object = dfm_object
  ) %>%
  mutate(performance_split = "testing - OOD")

# OOD evaluation requires a matching unrestricted DFM. Pilot runs may omit
# that sample plan, so only create OOD targets when the DFM exists.
performance_wild_sample_df <- performance_wild_sample_df %>%
  inner_join(testing_dfms, by = c("workset_meta_id", "dfm_to_lower")) %>%
  add_target("performance", c("predictive_model_id", "performance_split")) %>%
  hash_object_sym(performance_id, performance_object, prefix = "performance_predictive_model_")

# Bound table retained for downstream info/combine targets (combined_performance,
# info_performance) which iterate over performance_predictive_model_df$performance_object.
# The standard and OOD rows are generated by separate tar_eval calls in _targets.R
# (they evaluate on different DFMs), but their performance_object symbols are the
# union of the two source tables' symbols, so combining here is correct.
performance_predictive_model_df <- performance_standard_df %>%
  bind_rows(performance_wild_sample_df)

performance_predictive_model_per_volume_df <- predictive_model_df %>%
  expand_grid(performance_split = "full") %>%
  add_target("performance_per_volume", c("predictive_model_id", "performance_split"))

# SVD word vector cosine similarities ----------------------------------------

sims_svd_df <- svd_df %>%
  filter(sample_type == "restricted", sample_max_vols == max_sample_cap) %>%
  add_target("weight_svd", "svd_id") %>%
  hash_object_sym(weight_svd_id, weight_svd_object, prefix = "weight_pred_")

# PPMI word vectors -----------------------------------------------------------

ppmi_wv_df <- dfm_df %>%
  filter(sample_type == "restricted", sample_max_vols == max_sample_cap) %>%
  expand_grid(ppmi_fun = cfg$ppmi$funs) %>%
  add_target("weight_ppmi", c("dfm_id", "ppmi_fun")) %>%
  hash_object_sym(weight_ppmi_id, weight_ppmi_object, prefix = "weight_pred_")

# Graphs ----------------------------------------------------------------------

graph_df <- bind_rows(sims_svd_df,
                      predictive_model_weights_df,
                      ppmi_wv_df) %>%
  nesting() %>%
  expand_grid(nesting(tibble(
    graph_pos_patterns = cfg$graphs$patterns,
    graph_collapse_cased = cfg$graphs$collapse_cased,
    graph_max_per_slice = cfg$graphs$max_per_slice,
    graph_max_terms = cfg$graphs$max_terms))) %>%
  filter(!(dfm_to_lower & graph_pos_patterns == "^[A-Z].+NN")) %>%
  mutate(weight_id = coalesce(weight_pred_id, weight_ppmi_id, weight_svd_id),
         weight_object = coalesce(weight_pred_object, weight_ppmi_object, weight_svd_object)) %>%
  {
    if (isTRUE(cfg$graphs$exclude_fiction)) {
      filter(., !str_detect(workset_meta_id, "fiction"))
    } else {
      .
    }
  } %>%
  filter(!is.na(weight_id)) %>%
  add_target("graph", c("weight_id", "workset_meta_id", "graph_pos_patterns", "dfm_to_lower")) %>%
  create_titles() %>%
  hash_object_sym(graph_id, graph_object, prefix = "graph_weight_")

# KL divergences --------------------------------------------------------------

divergence_base <- bind_rows(predictive_model_weights_df,
                             ppmi_wv_df,
                             sims_svd_df) %>%
  # Engine label carries the params hash (when non-empty) so multiple variants
  # of the same engine (e.g. a tuning run's xgboost grid) get distinct
  # kl/entropy target names and distinguishable weight_type labels downstream.
  # Default (empty-hash) configs keep the bare engine name, so historical
  # target names are unchanged.
  mutate(predictive_model_engine_label = dplyr::case_when(
    is.na(predictive_model_engine) ~ NA_character_,
    is.na(predictive_model_params_hash) | predictive_model_params_hash == "" ~ predictive_model_engine,
    TRUE ~ paste0(predictive_model_engine, "_", predictive_model_params_hash)
  )) %>%
  mutate(weight_type = case_when(!is.na(predictive_model_engine) ~ paste(predictive_model_engine_label, sample_max_vols,
                                                                         dfm_to_lower),
                                 !is.na(svd_dims) ~ paste("svd", sample_max_vols, svd_weight,
                                                          dfm_to_lower),
                                 TRUE ~ paste("ppmi", sample_max_vols, ppmi_fun,
                                              dfm_to_lower)),
         weight_object = coalesce(weight_pred_object, weight_ppmi_object, weight_svd_object)) %>%
  dplyr::ungroup()

# Optional common-vocabulary robustness variant for KL (default: OFF). When the
# flag is FALSE only the union-vocabulary (headline) KL is computed;
# `kl_common_vocab_label` is "" for that case so slugify() drops it and the
# default kl target names are unchanged.
kl_common_vocab_values <- if (isTRUE(cfg$kl$common_vocab_robustness %||% FALSE)) {
  c(FALSE, TRUE)
} else {
  FALSE
}

# kl and entropy target names are built from semantic labels (weight_type etc.)
# that do NOT encode the sample id, so two replicates of the same engine would
# collide (exactly like the duplicate-engine bug fixed via
# predictive_model_engine_label above). `sample_rep_label` (empty for rep 1)
# disambiguates reps 2..n while keeping default target names unchanged.
kl_df <- divergence_base %>%
  expand_grid(kl_common_vocab = kl_common_vocab_values) %>%
  mutate(kl_common_vocab_label = dplyr::if_else(kl_common_vocab, "commonvocab", "")) %>%
  add_target(
    "kl",
    c("weight_type", "workset_meta_id", "feature_id", "dfm_to_lower", "kl_common_vocab_label", "sample_rep_label")
  )

entropy_df <- divergence_base %>%
  add_target(
    "entropy",
    c("weight_type", "workset_meta_id", "feature_id", "dfm_to_lower", "sample_rep_label")
  )
