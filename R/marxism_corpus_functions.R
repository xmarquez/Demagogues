if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}

#' Deep merge configuration lists for Marxism runs
#'
#' Recursively merges `override` into `base`, replacing scalar values and
#' merging named list values.
#'
#' @param base Base configuration list.
#' @param override Override configuration list.
#'
#' @return A merged list.
#' @keywords internal
marxism_deep_merge <- function(base, override) {
  if (is.null(base)) {
    return(override)
  }
  if (is.null(override)) {
    return(base)
  }
  if (!is.list(base) || !is.list(override)) {
    return(override)
  }

  keys <- union(names(base), names(override))
  out <- vector("list", length(keys))
  names(out) <- keys
  for (key in keys) {
    out[[key]] <- marxism_deep_merge(base[[key]], override[[key]])
  }
  out
}

#' Read a Marxism corpus pipeline configuration
#'
#' Loads shared Marxism corpus defaults, one feature definition from
#' `config/features`, and one run definition from `config/marxism_runs`.
#'
#' @param run Run ID, matching `config/marxism_runs/<run>.yml`.
#' @param config_dir Configuration directory.
#'
#' @return A merged configuration list with `source`, `feature`, and run fields.
read_marxism_pipeline_config <- function(run = {
                                           marxism_run <- Sys.getenv("MARXISM_RUN", "")
                                           if (nzchar(marxism_run)) {
                                             marxism_run
                                           } else {
                                             Sys.getenv("TARGET_RUN", "authority_marxism_glmnet_100")
                                           }
                                         },
                                         config_dir = "config") {
  corpus_path <- file.path(config_dir, "marxism_corpus.yml")
  run_path <- file.path(config_dir, "marxism_runs", paste0(run, ".yml"))

  if (!file.exists(corpus_path)) {
    stop("Missing Marxism corpus config: ", corpus_path, call. = FALSE)
  }
  if (!file.exists(run_path)) {
    stop("Missing Marxism run config: ", run_path, call. = FALSE)
  }

  corpus_cfg <- yaml::read_yaml(corpus_path)
  run_cfg <- yaml::read_yaml(run_path)

  source_id <- run_cfg$source %||% "marx_engels"
  source_cfg <- corpus_cfg$sources[[source_id]]
  if (is.null(source_cfg)) {
    stop("Unknown Marxism source `", source_id, "` in ", run_path, call. = FALSE)
  }

  feature_id <- run_cfg$feature %||% run_cfg$feature_id
  if (is.null(feature_id) || !nzchar(feature_id)) {
    stop("Marxism run must declare `feature`: ", run_path, call. = FALSE)
  }
  feature_path <- file.path(config_dir, "features", paste0(feature_id, ".yml"))
  if (!file.exists(feature_path)) {
    stop("Missing feature config: ", feature_path, call. = FALSE)
  }
  feature_cfg <- yaml::read_yaml(feature_path)
  feature_cfg$id <- feature_cfg$id %||% feature_id
  feature_cfg$name <- feature_cfg$name %||% feature_cfg$id

  direct_overrides <- run_cfg[setdiff(
    names(run_cfg),
    c("id", "description", "feature", "feature_id", "source", "source_id", "outputs", "overrides")
  )]
  cfg <- marxism_deep_merge(corpus_cfg, direct_overrides)
  cfg <- marxism_deep_merge(cfg, run_cfg$overrides %||% list())
  cfg$source <- source_cfg
  cfg$source$id <- source_id
  cfg$feature <- feature_cfg
  cfg$run_id <- run_cfg$id %||% run
  cfg$run_description <- run_cfg$description %||% ""
  cfg$outputs <- run_cfg$outputs %||% cfg$outputs
  cfg$config_source <- run_path
  cfg
}

#' Resolve the Marxism corpus root
#'
#' @param cfg Marxism pipeline configuration.
#'
#' @return Normalized path to the migrated Marxism corpus directory.
marxism_data_root <- function(cfg) {
  configured_root <- cfg$root %||% ""
  if (!nzchar(configured_root)) {
    configured_root <- file.path(
      Sys.getenv("RESEARCH_DATA_ROOT", "D:/ResearchData/corpora"),
      "marxism"
    )
  }
  normalizePath(configured_root, winslash = "/", mustWork = FALSE)
}

#' Resolve a path inside the configured Marxism corpus source
#'
#' @param cfg Marxism pipeline configuration.
#' @param field Source field containing a relative directory.
#'
#' @return Normalized source path.
marxism_source_path <- function(cfg, field) {
  relative <- cfg$source[[field]]
  if (is.null(relative) || !nzchar(relative)) {
    stop("Configured Marxism source is missing `", field, "`.", call. = FALSE)
  }
  normalizePath(file.path(marxism_data_root(cfg), relative), winslash = "/", mustWork = FALSE)
}

#' Extract a composition year from a Marx/Engels relative path
#'
#' @param path Character vector of normalized paths.
#'
#' @return Integer vector of years, or `NA_integer_`.
marxism_year_from_path <- function(path) {
  path <- gsub("\\\\", "/", path)
  match <- stringr::str_match(path, "/works/(\\d{4})(?:-pre)?/")
  as.integer(match[, 2])
}

#' Canonicalize Marx/Engels author strings
#'
#' @param author Raw author metadata from HTML.
#' @param text_id Relative text path.
#'
#' @return Canonical author label.
normalize_marxism_author <- function(author, text_id = NA_character_) {
  author <- stringr::str_squish(author %||% NA_character_)
  author <- dplyr::case_when(
    is.na(author) | !nzchar(author) ~ "Unknown/other",
    stringr::str_detect(author, stringr::regex("Marx.*Engels|Engels.*Marx|selected works", ignore_case = TRUE)) ~
      "Marx and Engels",
    stringr::str_detect(author, stringr::regex("Frederick|F Engels|\\bEngels\\b", ignore_case = TRUE)) ~
      "Frederick Engels",
    stringr::str_detect(author, stringr::regex("Karl Marx|\\bMarx\\b", ignore_case = TRUE)) ~
      "Karl Marx",
    TRUE ~ "Unknown/other"
  )
  author
}

#' Read metadata from one Marx/Engels HTML file
#'
#' @param file HTML file path.
#' @param html_root Root directory used to create relative `text_id` values.
#'
#' @return One-row tibble with basic metadata and text counts.
read_marx_engels_html_metadata <- function(file, html_root) {
  html_root <- normalizePath(html_root, winslash = "/", mustWork = TRUE)
  file_norm <- normalizePath(file, winslash = "/", mustWork = TRUE)
  text_id <- substring(file_norm, nchar(paste0(html_root, "/")) + 1L)

  doc <- tryCatch(
    xml2::read_html(file_norm, options = c("RECOVER", "HUGE")),
    error = function(e) e
  )
  if (inherits(doc, "error")) {
    return(tibble::tibble(
      text_id = text_id,
      html_path = file_norm,
      year = marxism_year_from_path(text_id),
      title = NA_character_,
      author = NA_character_,
      author_raw = NA_character_,
      classification = NA_character_,
      information = NA_character_,
      num_paragraphs = NA_integer_,
      num_words = NA_integer_,
      parse_ok = FALSE,
      parse_error = conditionMessage(doc)
    ))
  }

  doc <- xml2::xml_ns_strip(doc)
  title <- xml2::xml_find_first(doc, ".//head/title | .//h1[1]") |>
    xml2::xml_text(trim = TRUE)
  author <- xml2::xml_find_all(doc, ".//head/meta[@name='author']") |>
    xml2::xml_attr("content")
  classification <- xml2::xml_find_all(doc, ".//head/meta[@name='classification']") |>
    xml2::xml_attr("content")
  information <- xml2::xml_find_all(doc, ".//p[@class='information'] | .//p[@class='info']") |>
    xml2::xml_text(trim = TRUE)
  paragraphs <- xml2::xml_find_all(
    doc,
    ".//p[not(@class='information' or @class='footer')] | .//blockquote | .//h1 | .//h2 | .//h3 | .//h4"
  ) |>
    xml2::xml_text(trim = TRUE)

  if (!length(title) || is.na(title[[1]]) || !nzchar(title[[1]])) {
    title <- NA_character_
  }
  if (!length(author)) {
    author <- NA_character_
  }
  if (!length(classification)) {
    classification <- NA_character_
  }
  if (!length(information)) {
    information <- NA_character_
  }

  tibble::tibble(
    text_id = text_id,
    html_path = file_norm,
    year = marxism_year_from_path(text_id),
    title = stringr::str_squish(title[[1]]),
    author = normalize_marxism_author(paste(author, collapse = ", "), text_id),
    author_raw = stringr::str_squish(paste(author, collapse = ", ")),
    classification = stringr::str_squish(paste(classification, collapse = ", ")),
    information = stringr::str_squish(paste(information, collapse = "\n")),
    num_paragraphs = length(paragraphs),
    num_words = sum(stringr::str_count(paragraphs, "\\S+")),
    parse_ok = TRUE,
    parse_error = NA_character_
  )
}

#' Build Marx/Engels metadata with annotation paths
#'
#' @param cfg Marxism pipeline configuration.
#'
#' @return Metadata tibble for annotated Marx/Engels texts.
build_marx_engels_metadata <- function(cfg) {
  html_root <- marxism_source_path(cfg, "raw_html_dir")
  annotation_root <- marxism_source_path(cfg, "annotated_rds_dir")
  if (!dir.exists(html_root)) {
    stop("Marx/Engels HTML directory not found: ", html_root, call. = FALSE)
  }
  if (!dir.exists(annotation_root)) {
    stop("Marx/Engels annotation directory not found: ", annotation_root, call. = FALSE)
  }

  html_files <- list.files(html_root, pattern = "\\.htm$", recursive = TRUE, full.names = TRUE)
  annotation_files <- list.files(annotation_root, pattern = "\\.rds$", recursive = TRUE, full.names = TRUE)
  if (!length(html_files)) {
    stop("No Marx/Engels HTML files found in ", html_root, call. = FALSE)
  }

  metadata <- purrr::map_dfr(html_files, read_marx_engels_html_metadata, html_root = html_root)

  annotation_root_norm <- normalizePath(annotation_root, winslash = "/", mustWork = TRUE)
  annotation_index <- tibble::tibble(
    annotation_path = normalizePath(annotation_files, winslash = "/", mustWork = TRUE),
    text_id = substring(normalizePath(annotation_files, winslash = "/", mustWork = TRUE),
                        nchar(paste0(annotation_root_norm, "/")) + 1L)
  ) |>
    dplyr::mutate(text_id = stringr::str_remove(text_id, "AnnotatedFull\\.rds$"))

  min_year <- as.integer(cfg$periods$sample_min_year %||% -Inf)
  max_year <- as.integer(cfg$periods$sample_max_year %||% Inf)
  slice_size <- as.integer(cfg$periods$sample_slice_size %||% 10L)

  metadata <- metadata |>
    dplyr::left_join(annotation_index, by = "text_id") |>
    dplyr::filter(
      !is.na(year),
      year >= min_year,
      year < max_year
    )

  if (isTRUE(cfg$source$require_annotations %||% TRUE)) {
    metadata <- metadata |>
      dplyr::filter(!is.na(annotation_path), file.exists(annotation_path))
  }

  metadata |>
    dplyr::mutate(
      period = floor(year / slice_size) * slice_size,
      period_label = paste0(period, "s"),
      source = "marx_engels"
    ) |>
    dplyr::arrange(year, text_id)
}

#' Return configured Marxism feature terms
#'
#' @param cfg Marxism pipeline configuration.
#'
#' @return Character vector of DFM feature names used as positive labels.
marxism_feature_terms <- function(cfg) {
  terms <- unlist(cfg$feature$dictionary, use.names = FALSE)
  terms <- as.character(terms[!is.na(terms) & nzchar(terms)])
  if (isTRUE(cfg$preprocessing$to_lower %||% TRUE)) {
    terms <- stringr::str_to_lower(terms)
  }
  unique(terms)
}

#' Read one Marx/Engels CoreNLP annotation into segment-term counts
#'
#' @param metadata_row One-row metadata tibble.
#' @param cfg Marxism pipeline configuration.
#'
#' @return Term counts by segment for one text.
read_marxism_annotation_counts <- function(metadata_row, cfg) {
  annotation_path <- metadata_row$annotation_path[[1]]
  annotation <- tryCatch(readRDS(annotation_path), error = function(e) e)
  if (inherits(annotation, "error") || is.null(annotation$token)) {
    return(tibble::tibble())
  }

  tokens <- tibble::as_tibble(annotation$token)
  required <- c("sentence", "token", "lemma", "POS")
  if (!all(required %in% names(tokens))) {
    return(tibble::tibble())
  }

  sentences_per_segment <- as.integer(cfg$preprocessing$sentences_per_segment %||% 5L)
  include_pattern <- cfg$preprocessing$token_pattern %||% "^\\p{L}+$"
  pos_pattern <- cfg$preprocessing$pos_pattern %||% "."
  min_length <- as.integer(cfg$preprocessing$min_token_length %||% 3L)
  to_lower <- isTRUE(cfg$preprocessing$to_lower %||% TRUE)
  use_lemma <- isTRUE(cfg$preprocessing$use_lemma %||% TRUE)
  exclude_tokens <- as.character(cfg$preprocessing$exclude_tokens %||% character(0))
  if (to_lower) {
    exclude_tokens <- stringr::str_to_lower(exclude_tokens)
  }

  token_col <- if (use_lemma) tokens$lemma else tokens$token
  if (is.null(token_col)) {
    token_col <- tokens$token
  }

  tokens <- tokens |>
    dplyr::mutate(
      sentence = suppressWarnings(as.integer(sentence)),
      token_base = as.character(token_col),
      pos = as.character(POS)
    ) |>
    dplyr::filter(
      !is.na(sentence),
      !is.na(token_base),
      !is.na(pos),
      stringr::str_detect(pos, pos_pattern),
      stringr::str_detect(token_base, include_pattern),
      stringr::str_length(token_base) >= min_length
    )

  if (!nrow(tokens)) {
    return(tibble::tibble())
  }

  if (to_lower) {
    tokens <- tokens |>
      dplyr::mutate(token_base = stringr::str_to_lower(token_base),
                    pos = stringr::str_to_lower(pos))
  }
  if (length(exclude_tokens)) {
    tokens <- tokens |>
      dplyr::filter(!token_base %in% exclude_tokens)
  }

  if (!nrow(tokens)) {
    return(tibble::tibble())
  }

  text_id <- metadata_row$text_id[[1]]
  tokens |>
    dplyr::mutate(
      segment_index = floor((sentence - 1L) / sentences_per_segment) + 1L,
      doc_id = paste(text_id, sprintf("%04d", segment_index), sep = "::"),
      word = paste(token_base, pos, sep = "_")
    ) |>
    dplyr::count(doc_id, word, name = "n") |>
    dplyr::mutate(
      text_id = text_id,
      source = metadata_row$source[[1]],
      author = metadata_row$author[[1]],
      title = metadata_row$title[[1]],
      year = metadata_row$year[[1]],
      period = metadata_row$period[[1]],
      period_label = metadata_row$period_label[[1]]
    )
}

#' Build Marxism segment-term counts
#'
#' @param metadata Marx/Engels metadata with annotation paths.
#' @param cfg Marxism pipeline configuration.
#'
#' @return Tibble of term counts by segment with feature-hit labels.
build_marxism_segment_counts <- function(metadata, cfg) {
  counts <- purrr::map_dfr(seq_len(nrow(metadata)), function(i) {
    read_marxism_annotation_counts(metadata[i, , drop = FALSE], cfg)
  })
  if (!nrow(counts)) {
    stop("No Marxism segment counts could be built.", call. = FALSE)
  }

  feature_terms <- marxism_feature_terms(cfg)
  feature_by_doc <- counts |>
    dplyr::group_by(doc_id) |>
    dplyr::summarise(feature_hit = any(word %in% feature_terms), .groups = "drop")

  counts |>
    dplyr::left_join(feature_by_doc, by = "doc_id") |>
    dplyr::relocate(feature_hit, .after = period_label)
}

#' Summarize Marxism feature support before modeling
#'
#' @param segment_counts Segment-term counts from `build_marxism_segment_counts`.
#' @param metadata Marx/Engels metadata.
#' @param cfg Marxism pipeline configuration.
#'
#' @return A list of audit tables.
audit_marxism_feature_support <- function(segment_counts, metadata, cfg) {
  segment_meta <- segment_counts |>
    dplyr::distinct(doc_id, text_id, source, author, title, year, period, period_label, feature_hit)

  segment_support <- segment_meta |>
    dplyr::group_by(period, period_label) |>
    dplyr::summarise(
      n_segments = dplyr::n(),
      positive_segments = sum(feature_hit),
      negative_segments = sum(!feature_hit),
      n_texts = dplyr::n_distinct(text_id),
      positive_texts = dplyr::n_distinct(text_id[feature_hit]),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      enough_positive = positive_segments >= as.integer(cfg$samples$min_positive_segments_per_period %||% 1L),
      enough_negative = negative_segments >= as.integer(cfg$samples$min_negative_segments_per_period %||% 1L),
      trainable = enough_positive & enough_negative
    )

  document_support <- metadata |>
    dplyr::mutate(feature_text = text_id %in% segment_meta$text_id[segment_meta$feature_hit]) |>
    dplyr::group_by(period, period_label) |>
    dplyr::summarise(
      n_texts = dplyr::n(),
      annotated_texts = sum(!is.na(annotation_path)),
      feature_texts = sum(feature_text),
      authors = paste(sort(unique(author)), collapse = "; "),
      .groups = "drop"
    )

  top_feature_segments <- segment_meta |>
    dplyr::filter(feature_hit) |>
    dplyr::count(period, author, title, sort = TRUE, name = "positive_segments") |>
    dplyr::group_by(period) |>
    dplyr::slice_max(positive_segments, n = 10, with_ties = FALSE) |>
    dplyr::ungroup()

  top_terms <- segment_counts |>
    dplyr::group_by(word) |>
    dplyr::summarise(n = sum(n), .groups = "drop") |>
    dplyr::slice_max(n, n = 50)

  list(
    segment_support = segment_support,
    document_support = document_support,
    top_feature_segments = top_feature_segments,
    top_terms = top_terms
  )
}

#' Sample Marxism segments for supervised modeling
#'
#' Keeps up to the configured number of positive segments per period and samples
#' matched negative segments according to `negative_multiplier`.
#'
#' @param segment_counts Segment-term counts with `feature_hit`.
#' @param cfg Marxism pipeline configuration.
#'
#' @return Sampled segment-term counts.
sample_marxism_segment_counts <- function(segment_counts, cfg) {
  seed <- as.integer(cfg$samples$seed %||% 20240427L)
  max_positive <- as.integer(cfg$samples$sample_max_segments %||% 100L)
  negative_multiplier <- as.numeric(cfg$samples$negative_multiplier %||% 1)
  min_positive <- as.integer(cfg$samples$min_positive_segments_per_period %||% 1L)
  min_negative <- as.integer(cfg$samples$min_negative_segments_per_period %||% 1L)

  set.seed(seed)
  segment_meta <- segment_counts |>
    dplyr::distinct(doc_id, period, feature_hit)

  sampled <- segment_meta |>
    dplyr::group_by(period) |>
    dplyr::group_modify(function(.x, .y) {
      positives <- dplyr::filter(.x, feature_hit)
      negatives <- dplyr::filter(.x, !feature_hit)
      if (nrow(positives) < min_positive || nrow(negatives) < min_negative) {
        return(tibble::tibble())
      }
      positives <- dplyr::slice_sample(positives, n = min(max_positive, nrow(positives)))
      n_negative <- min(nrow(negatives), max(min_negative, ceiling(nrow(positives) * negative_multiplier)))
      negatives <- dplyr::slice_sample(negatives, n = n_negative)
      dplyr::bind_rows(positives, negatives)
    }) |>
    dplyr::ungroup()

  if (!nrow(sampled)) {
    stop("No Marxism periods have enough positive and negative feature segments.", call. = FALSE)
  }

  segment_counts |>
    dplyr::semi_join(sampled, by = "doc_id")
}

#' Convert Marxism segment counts to a quanteda DFM
#'
#' @param segment_counts Sampled segment-term counts.
#' @param cfg Marxism pipeline configuration.
#'
#' @return A `quanteda` DFM with segment metadata in docvars.
marxism_counts_to_dfm <- function(segment_counts, cfg) {
  feature_terms <- marxism_feature_terms(cfg)
  vocab_size <- as.integer(cfg$dfm$vocab_size %||% 30000L)
  term_totals <- segment_counts |>
    dplyr::group_by(word) |>
    dplyr::summarise(total = sum(n), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(total))
  keep_terms <- utils::head(term_totals$word, vocab_size)
  keep_terms <- union(keep_terms, feature_terms)

  counts <- segment_counts |>
    dplyr::filter(word %in% keep_terms)
  if (!nrow(counts)) {
    stop("No Marxism segment counts remain after vocabulary trimming.", call. = FALSE)
  }

  docs <- sort(unique(counts$doc_id))
  features <- sort(unique(counts$word))
  sparse <- Matrix::sparseMatrix(
    i = match(counts$doc_id, docs),
    j = match(counts$word, features),
    x = counts$n,
    dimnames = list(docs, features)
  )
  dfm <- quanteda::as.dfm(sparse)

  docvars <- counts |>
    dplyr::distinct(doc_id, text_id, source, author, title, year, period, period_label, feature_hit) |>
    dplyr::arrange(match(doc_id, docs))
  quanteda::docvars(dfm) <- docvars |>
    dplyr::select(-doc_id) |>
    as.data.frame()

  quanteda::dfm_subset(dfm, quanteda::rowSums(dfm) > 0)
}

#' Split a Marxism DFM by period
#'
#' @param dfm Marxism segment DFM.
#' @param cfg Marxism pipeline configuration.
#'
#' @return Named list of period-specific DFMs with enough positive/negative rows.
split_marxism_dfm_by_period <- function(dfm, cfg) {
  min_positive <- as.integer(cfg$samples$min_positive_segments_per_period %||% 1L)
  min_negative <- as.integer(cfg$samples$min_negative_segments_per_period %||% 1L)
  docvars <- quanteda::docvars(dfm)
  periods <- sort(unique(docvars$period))

  period_dfms <- purrr::map(periods, function(period_value) {
    idx <- docvars$period == period_value
    period_dfm <- dfm[idx, ]
    period_docvars <- quanteda::docvars(period_dfm)
    positive <- sum(period_docvars$feature_hit)
    negative <- sum(!period_docvars$feature_hit)
    if (positive < min_positive || negative < min_negative) {
      return(NULL)
    }
    period_dfm
  })
  names(period_dfms) <- as.character(periods)
  purrr::compact(period_dfms)
}

#' Build train/test splits for Marxism period DFMs
#'
#' @param period_dfms Named list of period-specific DFMs.
#' @param feature_object Quanteda dictionary for the target feature.
#'
#' @return Named list of `rsample` splits.
make_marxism_splits <- function(period_dfms, feature_object) {
  purrr::imap(period_dfms, function(dfm, period) {
    train_test_splits(
      dfm = dfm,
      feat = feature_object,
      downsampled = FALSE,
      type = NA_character_
    )
  })
}

#' Fit configured predictive models for Marxism period DFMs
#'
#' @param period_dfms Named list of period-specific DFMs.
#' @param splits Named list of train/test splits.
#' @param feature_object Quanteda dictionary for the target feature.
#' @param cfg Marxism pipeline configuration.
#'
#' @return Tibble with model objects and any fit errors.
fit_marxism_predictive_models <- function(period_dfms, splits, feature_object, cfg) {
  periods <- names(period_dfms)
  grid <- tidyr::expand_grid(
    period = periods,
    predictive_model_engine = cfg$predictive_models$engines,
    predictive_model_dfm_weight = cfg$predictive_models$weights,
    predictive_model_task = cfg$predictive_models$task
  )

  grid |>
    dplyr::mutate(
      result = purrr::pmap(
        list(period, predictive_model_engine, predictive_model_dfm_weight, predictive_model_task),
        function(period, engine, weight, task) {
          tryCatch(
            list(
              model = predictive_model(
                dfm = period_dfms[[period]],
                initial_split = splits[[period]],
                feat = feature_object,
                weight = weight,
                model_type = task,
                engine = engine
              ),
              error = NA_character_
            ),
            error = function(e) list(model = NULL, error = conditionMessage(e))
          )
        }
      ),
      model = purrr::map(result, "model"),
      error = purrr::map_chr(result, "error")
    ) |>
    dplyr::select(-result)
}

#' Evaluate Marxism predictive models
#'
#' Computes testing or training performance for each fitted period/model
#' combination using the same `model_performance()` helpers as the Hathi
#' pipeline.
#'
#' @param models Tibble returned by `fit_marxism_predictive_models()`.
#' @param period_dfms Named list of period-specific DFMs.
#' @param splits Named list of `rsample` splits.
#' @param feature_object Quanteda dictionary for the target feature.
#' @param performance_split Split to evaluate, usually `"testing"`.
#'
#' @return A tibble of yardstick metrics with period/model metadata.
evaluate_marxism_predictive_models <- function(models,
                                               period_dfms,
                                               splits,
                                               feature_object,
                                               performance_split = "testing") {
  fitted <- models |>
    dplyr::filter(purrr::map_lgl(model, ~ !is.null(.x)))
  if (!nrow(fitted)) {
    return(tibble::tibble())
  }

  results <- vector("list", nrow(fitted))
  for (i in seq_len(nrow(fitted))) {
    period <- fitted$period[[i]]
    model <- fitted$model[[i]]
    weight <- fitted$predictive_model_dfm_weight[[i]]

    performance <- tryCatch(
      model_performance(
        model = model,
        dfm = period_dfms[[period]],
        initial_split = splits[[period]],
        feat = feature_object,
        weight = weight,
        use = performance_split
      ) |>
        dplyr::mutate(performance_error = NA_character_),
      error = function(e) {
        tibble::tibble(
          .metric = "error",
          .estimator = NA_character_,
          .estimate = NA_real_,
          conf_mat = list(NULL),
          model_type = NA_character_,
          performance_error = conditionMessage(e)
        )
      }
    )

    if ("conf_mat" %in% names(performance)) {
      conf_mat_indices <- which(purrr::map_lgl(performance$conf_mat, ~ !is.null(.x)))
      conf_mat_index <- if (length(conf_mat_indices)) conf_mat_indices[[1]] else NA_integer_
      if (!is.na(conf_mat_index)) {
        conf_table <- performance$conf_mat[[conf_mat_index]]$table
        sample_size <- sum(conf_table)
        negative <- sum(conf_table[, 1])
        positive <- sum(conf_table[, 2])
      } else {
        sample_size <- NA_integer_
        negative <- NA_integer_
        positive <- NA_integer_
      }
      performance <- performance |>
        dplyr::mutate(
          sample_size = sample_size,
          negative = negative,
          positive = positive
        ) |>
        dplyr::select(-conf_mat)
    } else {
      performance <- performance |>
        dplyr::mutate(
          sample_size = NA_integer_,
          negative = NA_integer_,
          positive = NA_integer_
        )
    }

    results[[i]] <- fitted[i, , drop = FALSE] |>
      dplyr::select(-model, -error) |>
      dplyr::bind_cols(performance) |>
      dplyr::mutate(
        period = as.integer(period),
        performance_split = performance_split,
        performance_id = paste(
          "marxism",
          period,
          predictive_model_engine,
          predictive_model_dfm_weight,
          performance_split,
          sep = "_"
        )
      )

    rm(model, performance)
    gc()
  }

  dplyr::bind_rows(results)
}

#' Summarize Marxism model sample sizes from confusion matrices
#'
#' @param performance Performance tibble from
#'   `evaluate_marxism_predictive_models()`.
#'
#' @return Tibble with positive/negative counts by model and period.
summarize_marxism_sample_sizes <- function(performance) {
  if (!nrow(performance)) {
    return(tibble::tibble())
  }

  performance |>
    dplyr::filter(.metric != "error") |>
    dplyr::distinct(
      performance_id,
      period,
      performance_split,
      predictive_model_engine,
      predictive_model_dfm_weight,
      sample_size,
      negative,
      positive
    )
}

#' Write Marxism predictive performance to an RDS file
#'
#' The performance tibble is persisted as a file target to avoid serializing
#' nested metric artifacts directly through the targets object store.
#'
#' @param models Tibble returned by `fit_marxism_predictive_models()`.
#' @param period_dfms Named list of period-specific DFMs.
#' @param splits Named list of `rsample` splits.
#' @param feature_object Quanteda dictionary for the target feature.
#' @param cfg Marxism pipeline configuration.
#' @param performance_split Split to evaluate.
#'
#' @return Path to the written RDS file.
write_marxism_predictive_performance <- function(models,
                                                 period_dfms,
                                                 splits,
                                                 feature_object,
                                                 cfg,
                                                 performance_split = "testing") {
  performance <- evaluate_marxism_predictive_models(
    models = models,
    period_dfms = period_dfms,
    splits = splits,
    feature_object = feature_object,
    performance_split = performance_split
  )
  out_dir <- file.path("_targets", "user")
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  out_path <- file.path(out_dir, paste0(cfg$run_id, "_marxism_combined_performance.rds"))
  saveRDS(performance, out_path)
  out_path
}

#' Extract model weights from Marxism predictive models
#'
#' @param models Tibble returned by `fit_marxism_predictive_models`.
#'
#' @return Combined model-weight tibble.
extract_marxism_model_weights <- function(models) {
  fitted <- models |>
    dplyr::filter(purrr::map_lgl(model, ~ !is.null(.x)))
  if (!nrow(fitted)) {
    return(tibble::tibble())
  }

  fitted |>
    dplyr::mutate(weights = purrr::map(model, model_weights)) |>
    tidyr::unnest(weights) |>
    dplyr::mutate(
      period = as.integer(period),
      weight_id = paste("marxism", period, predictive_model_engine, predictive_model_dfm_weight, sep = "_")
    ) |>
    dplyr::select(
      weight_id, period, predictive_model_engine, predictive_model_dfm_weight,
      predictive_model_task, dplyr::everything(), -model
    )
}

#' Build Marxism model-weight graphs
#'
#' @param weights Combined model weights.
#' @param cfg Marxism pipeline configuration.
#'
#' @return Tibble with one graph object per engine/POS pattern.
build_marxism_graphs <- function(weights, cfg) {
  if (!nrow(weights)) {
    return(tibble::tibble())
  }

  patterns <- as.character(cfg$graphs$patterns %||% ".")
  collapse_cased <- as.logical(cfg$graphs$collapse_cased %||% TRUE)
  if (length(collapse_cased) == 1L) {
    collapse_cased <- rep(collapse_cased, length(patterns))
  }
  max_per_slice <- as.integer(cfg$graphs$max_per_slice %||% 8L)
  max_terms <- as.integer(cfg$graphs$max_terms %||% 40L)

  grid <- tidyr::expand_grid(
    predictive_model_engine = unique(weights$predictive_model_engine),
    graph_pos_patterns = patterns
  ) |>
    dplyr::mutate(
      graph_collapse_cased = collapse_cased[match(graph_pos_patterns, patterns)],
      graph_max_per_slice = max_per_slice[[1]],
      graph_max_terms = max_terms[[1]]
    )

  grid |>
    dplyr::mutate(
      graph_id = paste("marxism", cfg$run_id, predictive_model_engine,
                       gsub("[^A-Za-z0-9]+", "_", graph_pos_patterns), sep = "_"),
      title = paste(
        cfg$feature$name,
        "in Marx/Engels",
        predictive_model_engine,
        graph_pos_patterns
      ),
      plot = purrr::pmap(
        list(predictive_model_engine, graph_pos_patterns, graph_collapse_cased,
             graph_max_per_slice, graph_max_terms, title),
        function(engine, pattern, collapse, top_n, max_n, title) {
          graph_data <- weights |>
            dplyr::filter(
              predictive_model_engine == engine,
              stringr::str_detect(word, pattern)
            )
          if (!nrow(graph_data)) {
            return(ggplot2::ggplot() +
                     ggplot2::theme_void() +
                     ggplot2::labs(title = paste(title, "(no matching terms)")))
          }
          graph_similarities(
            graph_data,
            top_n = top_n,
            var = value,
            max_n = max_n,
            collapse_cased = collapse
          ) +
            ggplot2::labs(title = stringr::str_wrap(title))
        }
      )
    )
}

#' Write a Quarto report for Marxism authority graphs
#'
#' @param cfg Marxism pipeline configuration.
#' @param output_dir Directory for generated reports.
#'
#' @return Path to the generated `.qmd` file.
write_marxism_graph_qmd <- function(cfg, output_dir = file.path("Paper")) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  yaml_quote <- function(x) {
    paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
  }

  chunk_quote <- function(x) {
    x <- gsub("\\", "\\\\", x, fixed = TRUE)
    x <- gsub("\"", "\\\"", x, fixed = TRUE)
    paste0("\"", x, "\"")
  }

  r_character_vector <- function(x) {
    x <- as.character(x)
    x <- x[!is.na(x) & nzchar(x)]
    if (!length(x)) {
      return("character(0)")
    }
    paste0("c(", paste(purrr::map_chr(x, chunk_quote), collapse = ", "), ")")
  }

  output_path <- file.path(output_dir, paste0("marxism_graphs_", cfg$run_id, ".qmd"))

  feature_name <- cfg$feature$name %||% cfg$feature$id
  tracked_terms <- unique(c(
    cfg$feature$graphs$tracked_terms %||% character(0),
    cfg$feature$tracked_terms %||% character(0),
    cfg$graphs$tracked_terms %||% character(0)
  ))
  tracked_top_n <- as.integer(
    cfg$graphs$tracked_top_n %||%
      cfg$feature$graphs$tracked_top_n %||%
      cfg$feature$tracked_top_n %||%
      10L
  )
  if (is.na(tracked_top_n) || tracked_top_n < 1L) {
    tracked_top_n <- 10L
  }

  tracked_pos_info <- tibble::tribble(
    ~pos_group, ~pos_slug, ~pos_term_label,
    "Adjectives", "adjectives", "adjective terms",
    "Verbs", "verbs", "verb terms",
    "Nouns", "nouns", "noun terms"
  )

  top_term_fig_height <- max(7, 2 + 0.8 * tracked_top_n)

  model_info <- tidyr::expand_grid(
    predictive_model_engine = cfg$predictive_models$engines,
    predictive_model_dfm_weight = cfg$predictive_models$weights
  ) |>
    dplyr::mutate(
      engine_order = match(
        predictive_model_engine,
        c("glmnet", "LiblineaR", "naivebayes", "xgboost")
      ),
      engine_order = dplyr::coalesce(engine_order, 999L),
      model_label = paste(
        predictive_model_engine,
        predictive_model_dfm_weight,
        "weights"
      ),
      model_slug = stringr::str_replace_all(
        tolower(model_label),
        "[^a-z0-9]+",
        "-"
      ) |>
        stringr::str_remove("^-") |>
        stringr::str_remove("-$")
    ) |>
    dplyr::arrange(engine_order, model_label)

  graph_patterns <- as.character(cfg$graphs$patterns %||% ".")
  graph_info <- tidyr::expand_grid(
    predictive_model_engine = cfg$predictive_models$engines,
    graph_pos_patterns = graph_patterns
  ) |>
    dplyr::mutate(
      graph_id = paste(
        "marxism",
        cfg$run_id,
        predictive_model_engine,
        gsub("[^A-Za-z0-9]+", "_", graph_pos_patterns),
        sep = "_"
      ),
      graph_type = paste("Predictive model weights", predictive_model_engine),
      pos_label = dplyr::case_when(
        graph_pos_patterns == "." ~ "All terms",
        stringr::str_detect(graph_pos_patterns, "_nn|_NN") ~ "Nouns",
        stringr::str_detect(graph_pos_patterns, "_vb|_VB") ~ "Verbs",
        stringr::str_detect(graph_pos_patterns, "_jj|_JJ") ~ "Adjectives",
        TRUE ~ graph_pos_patterns
      ),
      section_title = paste(graph_type, pos_label, sep = " - "),
      caption = paste0(
        graph_type, "; ", pos_label,
        "; up to ", cfg$samples$sample_max_segments,
        " positive segments per period with matched negative segments. Terms at the top are more closely associated with '",
        feature_name,
        "' in later years. The label marks the peak density of association."
      ),
      chunk_label = paste0(
        "fig-marxism-",
        stringr::str_replace_all(graph_id, "[^A-Za-z0-9]+", "-")
      )
    )

  header <- c(
    "---",
    paste0("title: ", yaml_quote(paste("Graphs for", cfg$run_id))),
    "format:",
    "  html:",
    "    toc: true",
    "    toc-depth: 3",
    "    toc-location: right",
    "    include-in-header:",
    "      text: |",
    "        <style>",
    "        #TOC ul.collapse {",
    "          display: block !important;",
    "        }",
    "        #TOC ul.collapse li {",
    "          margin-left: 0.75rem;",
    "        }",
    "        </style>",
    "execute-dir: project",
    "execute:",
    "  echo: false",
    "  message: false",
    "  warning: false",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "library(targets)",
    "library(dplyr)",
    "library(tidyr)",
    "library(ggplot2)",
    "library(knitr)",
    "library(stringr)",
    "ggplot2::theme_set(ggplot2::theme_bw())",
    "```",
    "",
    paste0("Run: `", cfg$run_id, "`."),
    paste0("Feature(s): ", feature_name, "."),
    cfg$run_description %||% "",
    "",
    "This document reads precomputed Marx/Engels graph objects from the central `_targets` store.",
    "",
    "## Feature Support Audit",
    "",
    "The audit table reports segment-level support before model fitting. A segment is a fixed group of CoreNLP sentences from the annotated Marx/Engels corpus.",
    "",
    "```{r}",
    "#| label: tbl-marxism-feature-support",
    "audit <- targets::tar_read(marxism_feature_audit, store = '_targets')",
    "audit$segment_support |>",
    "  dplyr::mutate(",
    "    positive_share = round(positive_segments / n_segments, 3)",
    "  ) |>",
    "  dplyr::select(",
    "    period, period_label, n_segments, positive_segments, negative_segments,",
    "    positive_share, n_texts, positive_texts, trainable",
    "  ) |>",
    "  knitr::kable(caption = \"Authority feature support by decade in Marx/Engels segments.\")",
    "```",
    "",
    "## Predictive Performance",
    "",
    paste(
      "F1 is the harmonic mean of precision and recall for segments containing",
      "the target feature. These diagnostics are internal to the configured",
      "sample and train/test split; they do not estimate prevalence in the",
      "full Marx/Engels corpus."
    ),
    "",
    "```{r}",
    "#| label: fig-marxism-performance-f1",
    "#| fig-cap: \"Testing F1 by model and period.\"",
    "#| fig-width: 8",
    "#| fig-height: 4",
    "performance <- readRDS(targets::tar_read(marxism_combined_performance, store = '_targets')) |>",
    "  dplyr::mutate(",
    "    model = paste(",
    "      predictive_model_engine,",
    "      predictive_model_dfm_weight,",
    "      \"weights\"",
    "    )",
    "  )",
    "",
    "sample_sizes <- targets::tar_read(marxism_sample_sizes, store = '_targets') |>",
    "  dplyr::select(",
    "    performance_id,",
    "    period,",
    "    performance_split,",
    "    sample_size,",
    "    negative,",
    "    positive",
    "  )",
    "",
    "performance_wide <- performance |>",
    "  dplyr::filter(",
    "    performance_split == \"testing\",",
    "    .metric %in% c(\"f_meas\", \"roc_auc\", \"accuracy\", \"kap\")",
    "  ) |>",
    "  dplyr::select(period, performance_id, model, .metric, .estimate) |>",
    "  tidyr::pivot_wider(names_from = .metric, values_from = .estimate) |>",
    "  dplyr::left_join(",
    "    sample_sizes |>",
    "      dplyr::filter(performance_split == \"testing\"),",
    "    by = c(\"period\", \"performance_id\")",
    "  ) |>",
    "  dplyr::arrange(model, period)",
    "",
    "ggplot2::ggplot(",
    "  performance_wide,",
    "  ggplot2::aes(x = period, y = f_meas, color = model, group = model)",
    ") +",
    "  ggplot2::geom_line(linewidth = 0.6) +",
    "  ggplot2::geom_point(size = 2) +",
    "  ggplot2::scale_y_continuous(limits = c(0, 1)) +",
    "  ggplot2::labs(x = \"Period\", y = \"Testing F1\", color = \"Model\")",
    "```",
    "",
    "```{r}",
    "#| label: tbl-marxism-performance-testing",
    "performance_wide |>",
    "  dplyr::transmute(",
    "    model,",
    "    period,",
    "    f1 = round(f_meas, 3),",
    "    roc_auc = round(roc_auc, 3),",
    "    accuracy = round(accuracy, 3),",
    "    kappa = round(kap, 3),",
    "    sample_size,",
    "    positive,",
    "    negative",
    "  ) |>",
    "  knitr::kable(",
    "    caption = \"Testing split predictive performance by model and period.\"",
    "  )",
    "```",
    "",
    "## Term Weight Trajectories",
    "",
    paste(
      "These plots use raw predictive-model weights recovered from the targets",
      "store. Xgboost values are feature-importance scores, not signed",
      "coefficients. Facets use free y scales because model outputs are not",
      "directly comparable in magnitude. The automatic top-term plots select",
      "adjectives, verbs, and nouns separately within each model using a cumulative log-scaled",
      "positive-association score: `sum(log1p(pmax(weight, 0)))` across",
      "periods. This favors terms with persistent positive association while",
      "dampening one-period spikes."
    ),
    "",
    "```{r}",
    "#| label: marxism-top-term-data",
    "#| include: false",
    paste0("tracked_top_n <- ", tracked_top_n),
    "tracked_pos_levels <- c(\"Adjectives\", \"Verbs\", \"Nouns\")",
    "",
    "model_weights <- targets::tar_read(marxism_predictive_model_weights, store = '_targets') |> ",
    "  dplyr::mutate(",
    "    model = paste(",
    "      predictive_model_engine,",
    "      predictive_model_dfm_weight,",
    "      \"weights\"",
    "    )",
    "  )",
    "",
    "top_terms <- model_weights |> ",
    "  dplyr::mutate(",
    "    pos_group = dplyr::case_when(",
    "      stringr::str_detect(",
    "        word,",
    "        stringr::regex(\"_jj[a-z]*$\", ignore_case = TRUE)",
    "      ) ~ \"Adjectives\",",
    "      stringr::str_detect(",
    "        word,",
    "        stringr::regex(\"_vb[a-z]*$\", ignore_case = TRUE)",
    "      ) ~ \"Verbs\",",
    "      stringr::str_detect(",
    "        word,",
    "        stringr::regex(\"_nn[a-z]*$\", ignore_case = TRUE)",
    "      ) ~ \"Nouns\",",
    "      TRUE ~ NA_character_",
    "    ),",
    "    pos_group = factor(pos_group, levels = tracked_pos_levels),",
    "    positive_value = pmax(value, 0),",
    "    selection_score = log1p(positive_value)",
    "  ) |> ",
    "  dplyr::filter(!is.na(pos_group)) |> ",
    "  dplyr::group_by(model, pos_group, word) |> ",
    "  dplyr::summarise(",
    "    association_score = sum(selection_score, na.rm = TRUE),",
    "    periods_positive = sum(positive_value > 0, na.rm = TRUE),",
    "    max_positive_weight = max(positive_value, na.rm = TRUE),",
    "    .groups = \"drop\"",
    "  ) |> ",
    "  dplyr::filter(association_score > 0) |> ",
    "  dplyr::group_by(model, pos_group) |> ",
    "  dplyr::arrange(",
    "    dplyr::desc(association_score),",
    "    dplyr::desc(periods_positive),",
    "    dplyr::desc(max_positive_weight),",
    "    word,",
    "    .by_group = TRUE",
    "  ) |> ",
    "  dplyr::slice_head(n = tracked_top_n) |> ",
    "  dplyr::ungroup()",
    "```"
  )

  top_term_blocks <- purrr::pmap_chr(
    model_info,
    function(predictive_model_engine,
             predictive_model_dfm_weight,
             engine_order,
             model_label,
             model_slug,
             ...) {
      purrr::pmap_chr(
        tracked_pos_info,
        function(pos_group, pos_slug, pos_term_label) {
          paste0(
            "### ", model_label, " - ", pos_group, "\n\n",
            "```{r}\n",
            "#| label: fig-marxism-top-term-weights-", model_slug, "-", pos_slug, "\n",
            "#| fig-cap: \"Top persistent positive-association ",
            pos_term_label,
            " for ",
            model_label,
            ".\"\n",
            "#| fig-width: 9\n",
            "#| fig-height: ", top_term_fig_height, "\n",
            "model_name <- ",
            chunk_quote(model_label),
            "\n",
            "pos_name <- ",
            chunk_quote(pos_group),
            "\n\n",
            "model_top_terms <- top_terms |> \n",
            "  dplyr::filter(model == model_name, pos_group == pos_name) |> \n",
            "  dplyr::arrange(\n",
            "    dplyr::desc(association_score),\n",
            "    dplyr::desc(periods_positive),\n",
            "    dplyr::desc(max_positive_weight),\n",
            "    word\n",
            "  )\n\n",
            "if (nrow(model_top_terms)) {\n",
            "  model_top_weights <- tidyr::expand_grid(\n",
            "    model = model_name,\n",
            "    period = sort(unique(model_weights$period[model_weights$model == model_name])),\n",
            "    word = model_top_terms$word\n",
            "  ) |> \n",
            "    dplyr::left_join(\n",
            "      model_weights |> \n",
            "        dplyr::filter(model == model_name) |> \n",
            "        dplyr::select(model, period, word, value),\n",
            "      by = c(\"model\", \"period\", \"word\")\n",
            "    ) |> \n",
            "    dplyr::mutate(\n",
            "      value = tidyr::replace_na(value, 0),\n",
            "      word = factor(word, levels = model_top_terms$word)\n",
            "    )\n\n",
            "  ggplot2::ggplot(\n",
            "    model_top_weights,\n",
            "    ggplot2::aes(x = period, y = value, group = word)\n",
            "  ) +\n",
            "    ggplot2::geom_hline(yintercept = 0, color = \"grey75\", linewidth = 0.3) +\n",
            "    ggplot2::geom_line(color = \"#2F5D62\", linewidth = 0.65) +\n",
            "    ggplot2::geom_point(color = \"#2F5D62\", size = 1.3) +\n",
            "    ggplot2::facet_wrap(ggplot2::vars(word), ncol = 1, scales = \"free_y\") +\n",
            "    ggplot2::labs(\n",
            "      x = \"Period\",\n",
            "      y = \"Raw weight / importance\",\n",
            "      subtitle = paste(\n",
            "        \"Terms ordered by cumulative log-scaled positive association within\",\n",
            "        tolower(pos_name)\n",
            "      )\n",
            "    ) +\n",
            "    ggplot2::theme(\n",
            "      legend.position = \"none\",\n",
            "      strip.text = ggplot2::element_text(face = \"bold\")\n",
            "    )\n",
            "} else {\n",
            "  cat(\"No positive ",
            pos_term_label,
            " available for this model.\")\n",
            "}\n",
            "```\n"
          )
        }
      ) |>
        paste(collapse = "\n")
    }
  )

  selected_terms_block <- c(
    "",
    "### Selected Configured Terms",
    "",
    "```{r}",
    "#| label: fig-marxism-selected-term-weights",
    "#| fig-cap: \"Selected term weights or importance by model over time.\"",
    "#| fig-width: 9",
    "#| fig-height: 5",
    paste0("tracked_terms <- ", r_character_vector(tracked_terms)),
    "",
    "if (length(tracked_terms)) {",
    "  selected_weights <- model_weights |> ",
    "    dplyr::distinct(model, period) |> ",
    "    tidyr::expand_grid(word = tracked_terms) |> ",
    "    dplyr::left_join(",
    "      model_weights |> ",
    "        dplyr::filter(word %in% tracked_terms) |> ",
    "        dplyr::select(model, period, word, value),",
    "      by = c(\"model\", \"period\", \"word\")",
    "    ) |> ",
    "    dplyr::mutate(",
    "      value = tidyr::replace_na(value, 0),",
    "      word = factor(word, levels = tracked_terms)",
    "    )",
    "",
    "  ggplot2::ggplot(",
    "    selected_weights,",
    "    ggplot2::aes(x = period, y = value, color = word, group = word)",
    "  ) +",
    "    ggplot2::geom_hline(yintercept = 0, color = \"grey70\", linewidth = 0.3) +",
    "    ggplot2::geom_line(linewidth = 0.6) +",
    "    ggplot2::geom_point(size = 1.5) +",
    "    ggplot2::facet_wrap(ggplot2::vars(model), scales = \"free_y\") +",
    "    ggplot2::labs(x = \"Period\", y = \"Raw weight / importance\", color = \"Term\")",
    "} else {",
    "  cat(\"No tracked terms configured.\")",
    "}",
    "```",
    "",
    "## Graphs"
  )

  graph_blocks <- purrr::pmap_chr(
    graph_info,
    function(graph_id, section_title, caption, chunk_label, ...) {
      paste0(
        "\n\n### ", section_title, "\n\n",
        "```{r}\n",
        "#| label: ", chunk_label, "\n",
        "#| fig-cap: ", chunk_quote(caption), "\n",
        "#| fig-width: 11\n",
        "#| fig-height: 9\n",
        "graphs <- targets::tar_read(marxism_graphs, store = '_targets')\n",
        "graph_row <- graphs |> dplyr::filter(graph_id == ",
        chunk_quote(graph_id),
        ")\n",
        "if (nrow(graph_row)) {\n",
        "  graph_row$plot[[1]]\n",
        "} else {\n",
        "  cat(\"Graph not available.\")\n",
        "}\n",
        "```"
      )
    }
  )

  readr::write_lines(
    c(header, top_term_blocks, selected_terms_block, graph_blocks),
    output_path
  )
  output_path
}
