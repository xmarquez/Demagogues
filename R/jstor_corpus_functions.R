if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}

#' Deep merge configuration lists for JSTOR runs
#'
#' Recursively merges `override` into `base`, replacing scalar values and
#' merging named list values.
#'
#' @param base Base configuration list.
#' @param override Override configuration list.
#'
#' @return A merged list.
#' @keywords internal
jstor_deep_merge <- function(base, override) {
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
    out[[key]] <- jstor_deep_merge(base[[key]], override[[key]])
  }
  out
}

#' Read a JSTOR corpus pipeline configuration
#'
#' Loads shared JSTOR corpus defaults, one feature definition from
#' `config/features`, and one run definition from `config/jstor_runs`.
#'
#' @param run Run ID, matching `config/jstor_runs/<run>.yml`.
#' @param config_dir Configuration directory.
#'
#' @return A merged configuration list with `source`, `feature`, and run fields.
read_jstor_pipeline_config <- function(run = {
                                         jstor_run <- Sys.getenv("JSTOR_RUN", "")
                                         if (nzchar(jstor_run)) {
                                           jstor_run
                                         } else {
                                           Sys.getenv("TARGET_RUN", "authority_jstor_dfr_audit")
                                         }
                                       },
                                       config_dir = "config") {
  corpus_path <- file.path(config_dir, "jstor_corpus.yml")
  run_path <- file.path(config_dir, "jstor_runs", paste0(run, ".yml"))

  if (!file.exists(corpus_path)) {
    stop("Missing JSTOR corpus config: ", corpus_path, call. = FALSE)
  }
  if (!file.exists(run_path)) {
    stop("Missing JSTOR run config: ", run_path, call. = FALSE)
  }

  corpus_cfg <- yaml::read_yaml(corpus_path)
  run_cfg <- yaml::read_yaml(run_path)

  source_id <- run_cfg$source %||% "dfr"
  source_cfg <- corpus_cfg$sources[[source_id]]
  if (is.null(source_cfg)) {
    stop("Unknown JSTOR source `", source_id, "` in ", run_path, call. = FALSE)
  }

  feature_id <- run_cfg$feature %||% run_cfg$feature_id
  if (is.null(feature_id) || !nzchar(feature_id)) {
    stop("JSTOR run must declare `feature`: ", run_path, call. = FALSE)
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
  cfg <- jstor_deep_merge(corpus_cfg, direct_overrides)
  cfg <- jstor_deep_merge(cfg, run_cfg$overrides %||% list())
  cfg$source <- source_cfg
  cfg$source$id <- source_id
  cfg$feature <- feature_cfg
  cfg$run_id <- run_cfg$id %||% run
  cfg$run_description <- run_cfg$description %||% ""
  cfg$outputs <- run_cfg$outputs %||% cfg$outputs
  cfg$config_source <- run_path
  cfg
}

#' Resolve the JSTOR corpus root
#'
#' @param cfg JSTOR pipeline configuration.
#'
#' @return Normalized path to the migrated JSTOR corpus directory.
jstor_data_root <- function(cfg) {
  configured_root <- cfg$root %||% ""
  if (!nzchar(configured_root)) {
    configured_root <- file.path(
      Sys.getenv("RESEARCH_DATA_ROOT", "D:/ResearchData/corpora"),
      "jstor"
    )
  }
  normalizePath(configured_root, winslash = "/", mustWork = FALSE)
}

#' Resolve a source-relative JSTOR path
#'
#' @param cfg JSTOR pipeline configuration.
#' @param field Source field containing a relative path.
#'
#' @return Normalized source path.
jstor_source_path <- function(cfg, field) {
  relative <- cfg$source[[field]]
  if (is.null(relative) || !nzchar(relative)) {
    stop("Configured JSTOR source is missing `", field, "`.", call. = FALSE)
  }
  normalizePath(file.path(jstor_data_root(cfg), relative), winslash = "/", mustWork = FALSE)
}

#' Terms to audit in a JSTOR run
#'
#' JSTOR ngrams are untagged, so POS-tagged dictionary entries are reduced to
#' their lexical forms only as a fallback.
#'
#' @param cfg JSTOR pipeline configuration.
#'
#' @return Lowercase character vector of unique terms.
jstor_feature_terms <- function(cfg) {
  terms <- cfg$feature_terms %||% cfg$terms %||% cfg$audit$terms %||% NULL

  if (is.null(terms)) {
    terms <- cfg$feature$search_terms %||% cfg$feature$tokens %||% cfg$feature$name
  }

  dictionary_terms <- unlist(cfg$feature$dictionary %||% list(), use.names = FALSE)
  dictionary_terms <- stringr::str_remove(
    dictionary_terms,
    "_(nn|nnp|nns|nnps|vb|vbd|vbg|vbn|vbp|vbz|jj|jjr|jjs)$"
  )

  terms <- unique(c(as.character(terms), dictionary_terms))
  terms <- stringr::str_to_lower(terms)
  terms[!is.na(terms) & nzchar(terms)]
}

#' Period labels for JSTOR audits
#'
#' @param year Integer publication year.
#' @param cfg JSTOR pipeline configuration.
#'
#' @return Integer period lower bound.
jstor_period_from_year <- function(year, cfg) {
  slice <- as.integer(cfg$periods$sample_slice_size %||% 10L)
  floor(as.integer(year) / slice) * slice
}

#' Human-readable JSTOR period label
#'
#' @param period Integer period lower bound.
#' @param cfg JSTOR pipeline configuration.
#'
#' @return Character period label.
jstor_period_label <- function(period, cfg) {
  if (identical(cfg$periods$period_name %||% "decade", "decade")) {
    return(paste0(period, "s"))
  }
  as.character(period)
}

#' Inventory JSTOR source files
#'
#' @param cfg JSTOR pipeline configuration.
#'
#' @return A list with source metadata and an article/file inventory tibble.
inventory_jstor_source <- function(cfg) {
  source_type <- cfg$source$type %||% "dfr_zip"
  if (identical(source_type, "dfr_zip")) {
    return(inventory_jstor_dfr_source(cfg))
  }
  if (identical(source_type, "polisci_jsonl")) {
    return(inventory_jstor_polisci_source(cfg))
  }
  stop("Unsupported JSTOR source type: ", source_type, call. = FALSE)
}

#' Inventory JSTOR DfR zip ngram files
#'
#' @param cfg JSTOR pipeline configuration.
#'
#' @return A list with zip metadata and NGRAMS file inventory.
inventory_jstor_dfr_source <- function(cfg) {
  root <- jstor_data_root(cfg)
  zip_glob <- cfg$source$zip_glob %||% "*.zip"
  ngram_type <- cfg$source$ngram_type %||% "NGRAMS1"
  zip_paths <- Sys.glob(file.path(root, zip_glob))
  zip_paths <- normalizePath(zip_paths, winslash = "/", mustWork = TRUE)
  if (!length(zip_paths)) {
    stop("No JSTOR DfR zip files matched `", zip_glob, "` under ", root, call. = FALSE)
  }

  file_inventory <- purrr::map_dfr(zip_paths, function(zip_path) {
    zip_info <- utils::unzip(zip_path, list = TRUE)
    ngram_pattern <- paste0(ngram_type, "\\.txt$")
    names <- zip_info[["Name"]][grepl(ngram_pattern, zip_info[["Name"]])]
    parts <- strsplit(names, "/", fixed = TRUE)
    year <- suppressWarnings(as.integer(vapply(parts, function(x) x[[2]], character(1))))
    document_id <- tools::file_path_sans_ext(basename(names))
    document_id <- stringr::str_remove(document_id, paste0("-", ngram_type, "$"))

    tibble::tibble(
      source = cfg$source$id,
      text_id = paste(basename(zip_path), names, sep = "::"),
      zip_path = zip_path,
      zip_file = basename(zip_path),
      filename = names,
      document_id = document_id,
      journal_code = vapply(parts, function(x) x[[1]], character(1)),
      year = year,
      period = jstor_period_from_year(year, cfg),
      period_label = jstor_period_label(period, cfg)
    )
  })

  min_year <- as.integer(cfg$periods$sample_min_year %||% min(file_inventory$year, na.rm = TRUE))
  max_year <- as.integer(cfg$periods$sample_max_year %||% max(file_inventory$year, na.rm = TRUE))
  file_inventory <- file_inventory |>
    dplyr::filter(!is.na(year), year >= min_year, year <= max_year)

  list(
    source_type = "dfr_zip",
    root = root,
    ngram_type = ngram_type,
    zip_files = tibble::tibble(
      zip_path = zip_paths,
      zip_file = basename(zip_paths),
      bytes = file.info(zip_paths)[["size"]]
    ),
    files = file_inventory
  )
}

#' Inventory the JSTOR political science JSONL sample
#'
#' @param cfg JSTOR pipeline configuration.
#'
#' @return A list with metadata and JSONL inventory.
inventory_jstor_polisci_source <- function(cfg) {
  metadata_path <- jstor_source_path(cfg, "metadata_file")
  jsonl_path <- jstor_source_path(cfg, "jsonl_file")

  metadata <- readr::read_csv(metadata_path, show_col_types = FALSE)
  metadata <- metadata |>
    dplyr::mutate(
      source = cfg$source$id,
      line_number = dplyr::row_number(),
      text_id = as.character(.data$id),
      year = as.integer(.data$publicationYear),
      period = jstor_period_from_year(year, cfg),
      period_label = jstor_period_label(period, cfg)
    )

  min_year <- as.integer(cfg$periods$sample_min_year %||% min(metadata$year, na.rm = TRUE))
  max_year <- as.integer(cfg$periods$sample_max_year %||% max(metadata$year, na.rm = TRUE))
  metadata <- metadata |>
    dplyr::filter(!is.na(year), year >= min_year, year <= max_year)

  list(
    source_type = "polisci_jsonl",
    root = jstor_data_root(cfg),
    metadata_file = metadata_path,
    jsonl_file = jsonl_path,
    files = metadata
  )
}

#' Apply the configured audit sampling cap
#'
#' @param inventory JSTOR source inventory tibble.
#' @param cfg JSTOR pipeline configuration.
#'
#' @return Inventory subset to scan.
sample_jstor_inventory_for_audit <- function(inventory, cfg) {
  cap <- cfg$audit$max_files_per_period %||% Inf
  cap <- suppressWarnings(as.integer(cap))
  if (is.na(cap) || !is.finite(cap) || cap < 1L) {
    return(inventory)
  }

  seed <- as.integer(cfg$audit$seed %||% cfg$samples$seed %||% 20240427L)
  set.seed(seed)
  inventory |>
    dplyr::group_by(period) |>
    dplyr::group_modify(~ dplyr::slice_sample(.x, n = min(cap, nrow(.x)))) |>
    dplyr::ungroup()
}

#' Count configured JSTOR terms in one local ngram file
#'
#' @param path Local NGRAMS file path.
#' @param terms Lowercase terms to count.
#'
#' @return One-row tibble of term counts plus total feature token count.
jstor_count_terms_in_ngram_file <- function(path, terms) {
  counts <- stats::setNames(rep.int(0L, length(terms)), terms)
  if (!file.exists(path)) {
    return(tibble::as_tibble_row(as.list(counts)))
  }

  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  if (!length(lines)) {
    return(tibble::as_tibble_row(as.list(counts)))
  }

  tab_pos <- regexpr("\t", lines, fixed = TRUE)
  keep <- tab_pos > 1
  if (!any(keep)) {
    return(tibble::as_tibble_row(as.list(counts)))
  }

  lines <- lines[keep]
  tab_pos <- tab_pos[keep]
  tokens <- stringr::str_to_lower(substr(lines, 1L, tab_pos - 1L))
  values <- suppressWarnings(as.integer(substr(lines, tab_pos + 1L, nchar(lines))))
  values[is.na(values)] <- 0L

  matched <- match(terms, tokens)
  found <- !is.na(matched)
  counts[found] <- values[matched[found]]
  tibble::as_tibble_row(as.list(counts))
}

#' Scan JSTOR DfR NGRAMS files for configured feature terms
#'
#' @param inventory Inventory list from `inventory_jstor_source()`.
#' @param cfg JSTOR pipeline configuration.
#'
#' @return Text-level term counts and metadata.
scan_jstor_dfr_feature_terms <- function(inventory, cfg) {
  terms <- jstor_feature_terms(cfg)
  files <- sample_jstor_inventory_for_audit(inventory$files, cfg)
  if (!nrow(files)) {
    return(files)
  }

  extract_to_temp <- isTRUE(cfg$audit$extract_to_temp %||% TRUE)

  if (!extract_to_temp) {
    counts <- purrr::pmap_dfr(
      list(files$zip_path, files$filename),
      function(zip_path, filename) {
        tmp <- tempfile("jstor_ngram_")
        on.exit(unlink(tmp), add = TRUE)
        utils::unzip(zip_path, files = filename, exdir = dirname(tmp), junkpaths = TRUE)
        local_path <- file.path(dirname(tmp), basename(filename))
        out <- jstor_count_terms_in_ngram_file(local_path, terms)
        unlink(local_path)
        out
      }
    )
    return(dplyr::bind_cols(files, counts))
  }

  files |>
    dplyr::group_split(zip_path) |>
    purrr::map_dfr(function(group) {
      zip_path <- group$zip_path[[1]]
      tmp_dir <- tempfile("jstor_ngram_extract_")
      dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
      on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

      utils::unzip(zip_path, files = group$filename, exdir = tmp_dir)
      counts <- purrr::map_dfr(
        group$filename,
        function(filename) {
          jstor_count_terms_in_ngram_file(file.path(tmp_dir, filename), terms)
        }
      )
      dplyr::bind_cols(group, counts)
    })
}

#' Count configured terms in JSTOR political-science JSONL rows
#'
#' @param inventory Inventory list from `inventory_jstor_source()`.
#' @param cfg JSTOR pipeline configuration.
#'
#' @return Text-level term counts and metadata.
scan_jstor_polisci_feature_terms <- function(inventory, cfg) {
  terms <- jstor_feature_terms(cfg)
  files <- sample_jstor_inventory_for_audit(inventory$files, cfg)
  if (!nrow(files)) {
    return(files)
  }

  con <- gzfile(inventory$jsonl_file, "rt")
  on.exit(close(con), add = TRUE)
  lines <- readLines(con, warn = FALSE)
  line_lookup <- stats::setNames(lines, seq_along(lines))

  counts <- purrr::map_dfr(files$line_number, function(line_number) {
    line <- jsonlite::fromJSON(line_lookup[[as.character(line_number)]], simplifyVector = FALSE)
    unigram_counts <- line$unigramCount %||% list()
    values <- stats::setNames(rep.int(0L, length(terms)), terms)
    for (term in terms) {
      value <- unigram_counts[[term]]
      if (!is.null(value)) {
        values[[term]] <- as.integer(value)
      }
    }
    tibble::as_tibble_row(as.list(values))
  })

  dplyr::bind_cols(files, counts)
}

#' Scan JSTOR feature support
#'
#' @param inventory Inventory list from `inventory_jstor_source()`.
#' @param cfg JSTOR pipeline configuration.
#'
#' @return Text-level term counts and metadata.
scan_jstor_feature_terms <- function(inventory, cfg) {
  if (identical(inventory$source_type, "dfr_zip")) {
    return(scan_jstor_dfr_feature_terms(inventory, cfg))
  }
  if (identical(inventory$source_type, "polisci_jsonl")) {
    return(scan_jstor_polisci_feature_terms(inventory, cfg))
  }
  stop("Unsupported JSTOR source type: ", inventory$source_type, call. = FALSE)
}

#' Summarize JSTOR feature support before modeling
#'
#' @param inventory Inventory list from `inventory_jstor_source()`.
#' @param cfg JSTOR pipeline configuration.
#'
#' @return A list of audit tables.
audit_jstor_feature_support <- function(inventory, cfg) {
  terms <- jstor_feature_terms(cfg)
  text_support <- scan_jstor_feature_terms(inventory, cfg)

  if (!nrow(text_support)) {
    return(list(
      source_summary = tibble::tibble(),
      period_support = tibble::tibble(),
      term_period_support = tibble::tibble(),
      text_support = text_support,
      top_feature_texts = tibble::tibble(),
      terms = terms
    ))
  }

  text_support <- text_support |>
    dplyr::mutate(
      feature_tokens = rowSums(dplyr::pick(dplyr::all_of(terms)), na.rm = TRUE),
      feature_hit = feature_tokens > 0
    )

  min_positive <- as.integer(cfg$audit$min_positive_texts_per_period %||% 8L)
  min_negative <- as.integer(cfg$audit$min_negative_texts_per_period %||% 8L)
  cap <- cfg$audit$max_files_per_period %||% Inf
  sampled <- is.finite(suppressWarnings(as.numeric(cap)))

  period_support <- text_support |>
    dplyr::group_by(period, period_label) |>
    dplyr::summarise(
      n_texts = dplyr::n(),
      positive_texts = sum(feature_hit),
      negative_texts = sum(!feature_hit),
      feature_tokens = sum(feature_tokens),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      enough_positive = positive_texts >= min_positive,
      enough_negative = negative_texts >= min_negative,
      trainable = enough_positive & enough_negative,
      sampled_audit = sampled,
      sample_max_texts_per_period = if (sampled) as.integer(cap) else NA_integer_
    ) |>
    dplyr::arrange(period)

  term_period_support <- text_support |>
    dplyr::select(period, period_label, dplyr::all_of(terms)) |>
    tidyr::pivot_longer(
      dplyr::all_of(terms),
      names_to = "term",
      values_to = "n"
    ) |>
    dplyr::group_by(period, period_label, term) |>
    dplyr::summarise(
      positive_texts = sum(n > 0),
      tokens = sum(n),
      .groups = "drop"
    ) |>
    dplyr::arrange(period, term)

  source_summary <- tibble::tibble(
    run_id = cfg$run_id,
    source = cfg$source$id,
    source_type = inventory$source_type,
    feature = cfg$feature$name,
    terms = paste(terms, collapse = ", "),
    n_available_texts = nrow(inventory$files),
    n_scanned_texts = nrow(text_support),
    min_year = min(text_support$year, na.rm = TRUE),
    max_year = max(text_support$year, na.rm = TRUE),
    sampled_audit = sampled,
    sample_max_texts_per_period = if (sampled) as.integer(cap) else NA_integer_
  )

  top_feature_texts <- text_support |>
    dplyr::filter(feature_hit) |>
    dplyr::arrange(dplyr::desc(feature_tokens), period, text_id) |>
    dplyr::select(
      text_id,
      dplyr::any_of(c("title", "isPartOf", "journal_code")),
      year,
      period,
      feature_tokens,
      dplyr::all_of(terms)
    ) |>
    utils::head(50)

  list(
    source_summary = source_summary,
    period_support = period_support,
    term_period_support = term_period_support,
    text_support = text_support,
    top_feature_texts = top_feature_texts,
    terms = terms
  )
}

#' Build a JSTOR feature dictionary
#'
#' @param cfg JSTOR pipeline configuration.
#'
#' @return A case-sensitive quanteda dictionary over untagged JSTOR terms.
build_jstor_feature_dictionary <- function(cfg) {
  feature_name <- cfg$feature$name %||% cfg$feature$id %||% "feature"
  terms <- jstor_feature_terms(cfg)
  quanteda::dictionary(stats::setNames(list(terms), feature_name), tolower = FALSE)
}

#' Read a cached JSTOR audit target as a sampling frame
#'
#' @param inventory JSTOR source inventory.
#' @param cfg JSTOR pipeline configuration.
#'
#' @return JSTOR audit list with `text_support`.
load_jstor_sampling_audit <- function(inventory, cfg) {
  target_name <- cfg$sampling_frame$target %||% cfg$sampling_frame_target %||% ""
  if (length(target_name) && !is.na(target_name) && nzchar(target_name)) {
    return(targets::tar_read_raw(name = target_name, store = "_targets"))
  }
  audit_jstor_feature_support(inventory, cfg)
}

#' Summarize audit support using the current model thresholds
#'
#' @param audit JSTOR feature audit list.
#' @param cfg JSTOR pipeline configuration.
#'
#' @return Period support table with model trainability columns.
jstor_model_period_support <- function(audit, cfg) {
  min_positive <- as.integer(cfg$samples$min_positive_texts_per_period %||%
                               cfg$audit$min_positive_texts_per_period %||% 1L)
  min_negative <- as.integer(cfg$samples$min_negative_texts_per_period %||%
                               cfg$audit$min_negative_texts_per_period %||% 1L)

  audit$period_support |>
    dplyr::mutate(
      model_enough_positive = positive_texts >= min_positive,
      model_enough_negative = negative_texts >= min_negative,
      model_trainable = model_enough_positive & model_enough_negative
    )
}

#' Select JSTOR texts for modeling
#'
#' Supports a top-frequency positive sample to mirror the current Hathi/Bookworm
#' behavior and a random negative sample matched to the selected positives.
#'
#' @param audit JSTOR feature audit list.
#' @param cfg JSTOR pipeline configuration.
#'
#' @return Text metadata for sampled positive and negative articles.
sample_jstor_texts <- function(audit, cfg) {
  text_support <- audit$text_support
  if (!nrow(text_support)) {
    stop("JSTOR sampling frame is empty.", call. = FALSE)
  }

  min_year <- as.integer(cfg$periods$sample_min_year %||% min(text_support$year, na.rm = TRUE))
  max_year <- as.integer(cfg$periods$sample_max_year %||% max(text_support$year, na.rm = TRUE))
  max_positive <- as.integer(cfg$samples$sample_max_texts %||% cfg$samples$sample_max_segments %||% 100L)
  negative_multiplier <- as.numeric(cfg$samples$negative_multiplier %||% 1)
  min_positive <- as.integer(cfg$samples$min_positive_texts_per_period %||% 1L)
  min_negative <- as.integer(cfg$samples$min_negative_texts_per_period %||% 1L)
  positive_strategy <- cfg$samples$positive_strategy %||% cfg$samples$sampling_strategy %||% "top_frequency"
  negative_strategy <- cfg$samples$negative_strategy %||% "random"
  seed <- as.integer(cfg$samples$seed %||% 20240427L)

  text_support <- text_support |>
    dplyr::filter(!is.na(year), year >= min_year, year <= max_year)

  set.seed(seed)
  sampled <- text_support |>
    dplyr::group_by(period) |>
    dplyr::group_modify(function(.x, .y) {
      positives <- dplyr::filter(.x, feature_hit)
      negatives <- dplyr::filter(.x, !feature_hit)
      if (nrow(positives) < min_positive || nrow(negatives) < min_negative) {
        return(tibble::tibble())
      }

      if (identical(positive_strategy, "top_frequency")) {
        positives <- positives |>
          dplyr::arrange(dplyr::desc(feature_tokens), text_id) |>
          utils::head(max_positive)
      } else if (identical(positive_strategy, "random")) {
        positives <- dplyr::slice_sample(positives, n = min(max_positive, nrow(positives)))
      } else {
        stop("Unsupported JSTOR positive sampling strategy: ", positive_strategy, call. = FALSE)
      }

      n_negative <- min(
        nrow(negatives),
        max(min_negative, ceiling(nrow(positives) * negative_multiplier))
      )
      if (identical(negative_strategy, "random")) {
        negatives <- dplyr::slice_sample(negatives, n = n_negative)
      } else if (identical(negative_strategy, "top_frequency")) {
        negatives <- negatives |>
          dplyr::arrange(dplyr::desc(feature_tokens), text_id) |>
          utils::head(n_negative)
      } else {
        stop("Unsupported JSTOR negative sampling strategy: ", negative_strategy, call. = FALSE)
      }

      dplyr::bind_rows(
        dplyr::mutate(positives, sample_role = "positive"),
        dplyr::mutate(negatives, sample_role = "negative")
      )
    }) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      doc_id = paste0("jstor_", period, "_", dplyr::row_number())
    )

  if (!nrow(sampled)) {
    stop("No JSTOR periods have enough positive and negative texts for modeling.", call. = FALSE)
  }
  sampled
}

#' Read all unigram counts from one extracted JSTOR NGRAMS1 file
#'
#' @param path Local path to an extracted NGRAMS file.
#' @param doc_id Stable document identifier.
#' @param cfg JSTOR pipeline configuration.
#'
#' @return Tibble with `doc_id`, `word`, and `n`.
read_jstor_ngram_counts_file <- function(path, doc_id, cfg) {
  if (!file.exists(path)) {
    return(tibble::tibble(doc_id = character(), word = character(), n = integer()))
  }

  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  if (!length(lines)) {
    return(tibble::tibble(doc_id = character(), word = character(), n = integer()))
  }

  tab_pos <- regexpr("\t", lines, fixed = TRUE)
  keep <- tab_pos > 1
  if (!any(keep)) {
    return(tibble::tibble(doc_id = character(), word = character(), n = integer()))
  }

  lines <- lines[keep]
  tab_pos <- tab_pos[keep]
  words <- substr(lines, 1L, tab_pos - 1L)
  counts <- suppressWarnings(as.integer(substr(lines, tab_pos + 1L, nchar(lines))))
  counts[is.na(counts)] <- 0L

  to_lower <- isTRUE(cfg$preprocessing$to_lower %||% TRUE)
  if (to_lower) {
    words <- stringr::str_to_lower(words)
  }

  token_pattern <- cfg$preprocessing$token_pattern %||% "^\\p{L}+$"
  min_length <- as.integer(cfg$preprocessing$min_token_length %||% 3L)
  feature_terms <- jstor_feature_terms(cfg)
  remove_stopwords <- isTRUE(cfg$preprocessing$remove_stopwords %||% TRUE)
  stop_words <- if (remove_stopwords) quanteda::stopwords("en") else character(0)
  exclude_tokens <- as.character(cfg$preprocessing$exclude_tokens %||% character(0))

  keep <- stringr::str_detect(words, token_pattern) &
    stringr::str_length(words) >= min_length &
    !(words %in% exclude_tokens) &
    (!(words %in% stop_words) | words %in% feature_terms)

  tibble::tibble(doc_id = doc_id, word = words[keep], n = counts[keep]) |>
    dplyr::group_by(doc_id, word) |>
    dplyr::summarise(n = sum(n), .groups = "drop")
}

#' Build JSTOR document-term counts for selected articles
#'
#' @param sampled_texts Texts selected by `sample_jstor_texts()`.
#' @param cfg JSTOR pipeline configuration.
#'
#' @return Tidy document-term counts.
build_jstor_document_counts <- function(sampled_texts, cfg) {
  if (!nrow(sampled_texts)) {
    stop("No sampled JSTOR texts to read.", call. = FALSE)
  }

  sampled_texts |>
    dplyr::group_split(zip_path) |>
    purrr::map_dfr(function(group) {
      zip_path <- group$zip_path[[1]]
      tmp_dir <- tempfile("jstor_model_extract_")
      dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
      on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

      utils::unzip(zip_path, files = group$filename, exdir = tmp_dir)
      purrr::map2_dfr(
        group$filename,
        group$doc_id,
        function(filename, doc_id) {
          read_jstor_ngram_counts_file(file.path(tmp_dir, filename), doc_id, cfg)
        }
      )
    })
}

#' Convert JSTOR document counts to a quanteda DFM
#'
#' @param counts Tidy document-term counts.
#' @param sampled_texts Text metadata for sampled articles.
#' @param cfg JSTOR pipeline configuration.
#'
#' @return A quanteda DFM with JSTOR metadata in docvars.
jstor_counts_to_dfm <- function(counts, sampled_texts, cfg) {
  feature_terms <- jstor_feature_terms(cfg)
  vocab_size <- as.integer(cfg$dfm$vocab_size %||% 30000L)
  term_totals <- counts |>
    dplyr::group_by(word) |>
    dplyr::summarise(total = sum(n), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(total))
  keep_terms <- union(utils::head(term_totals$word, vocab_size), feature_terms)

  counts <- counts |>
    dplyr::filter(word %in% keep_terms)
  if (!nrow(counts)) {
    stop("No JSTOR counts remain after vocabulary trimming.", call. = FALSE)
  }

  docs <- sampled_texts$doc_id
  features <- sort(unique(counts$word))
  sparse <- Matrix::sparseMatrix(
    i = match(counts$doc_id, docs),
    j = match(counts$word, features),
    x = counts$n,
    dims = c(length(docs), length(features)),
    dimnames = list(docs, features)
  )
  dfm <- quanteda::as.dfm(sparse)

  docvars <- sampled_texts |>
    dplyr::arrange(match(doc_id, docs)) |>
    dplyr::select(
      -doc_id,
      dplyr::any_of(c("authority", "authorities", "authoritarian", "authoritarianism", "authoritarians"))
    )
  quanteda::docvars(dfm) <- as.data.frame(docvars)
  quanteda::dfm_subset(dfm, quanteda::rowSums(dfm) > 0)
}

#' Split a JSTOR DFM by period
#'
#' @param dfm JSTOR document DFM.
#' @param cfg JSTOR pipeline configuration.
#'
#' @return Named list of period-specific DFMs.
split_jstor_dfm_by_period <- function(dfm, cfg) {
  min_positive <- as.integer(cfg$samples$min_positive_texts_per_period %||% 1L)
  min_negative <- as.integer(cfg$samples$min_negative_texts_per_period %||% 1L)
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

#' Build train/test splits for JSTOR period DFMs
#'
#' @param period_dfms Named list of period-specific DFMs.
#' @param feature_object Quanteda dictionary for the target feature.
#'
#' @return Named list of rsample splits.
make_jstor_splits <- function(period_dfms, feature_object) {
  purrr::imap(period_dfms, function(dfm, period) {
    train_test_splits(
      dfm = dfm,
      feat = feature_object,
      downsampled = FALSE,
      type = NA_character_
    )
  })
}

#' Dense LiblineaR performance path for JSTOR DFMs
#'
#' The shared sparse LiblineaR prediction path can crash on the JSTOR
#' 30k-feature article matrices in this environment. This helper keeps the
#' Hathi metric semantics but sends LiblineaR a small dense test matrix.
#'
#' @param model Fitted LiblineaR model.
#' @param dfm Period DFM.
#' @param initial_split Train/test split.
#' @param feat Target feature dictionary.
#' @param weight Feature weighting scheme.
#' @param use Split to evaluate.
#'
#' @return Yardstick metric tibble.
model_performance_jstor_liblinear_dense <- function(model,
                                                    dfm,
                                                    initial_split,
                                                    feat,
                                                    weight = c("ppmi", "tfidf", "none"),
                                                    use = "testing") {
  weight <- match.arg(weight, c("ppmi", "tfidf", "none"))
  use <- match.arg(use, c("testing", "training", "testing - OOD"))
  if (use == "testing") {
    testing_dfm <- get_test_sample(dfm, initial_split)
  } else if (use == "training") {
    testing_dfm <- get_training_sample(dfm, initial_split)
  } else {
    testing_dfm <- dfm
  }

  model_type <- ifelse(model$Type %in% c(0:7), "classification", "regression")
  model_features <- colnames(model$W)[colnames(model$W) != "Bias"]
  x_test <- get_x(testing_dfm, feat = feat, weight = weight) |>
    quanteda::dfm_match(model_features) |>
    as.matrix()
  y_test <- get_y(testing_dfm, feat, model_type = model_type)

  predictions <- LiblineaR:::predict.LiblineaR(model, newx = x_test)

  if (identical(model_type, "classification")) {
    preds <- tibble::tibble(
      truth = factor(y_test, levels = c(FALSE, TRUE)),
      class = factor(predictions$predictions, levels = c(FALSE, TRUE))
    )
    conf_mat <- preds |>
      yardstick::conf_mat(truth = truth, estimate = class)

    res <- preds |>
      yardstick::metrics(truth = truth, estimate = class) |>
      dplyr::bind_rows(yardstick::f_meas(
        preds,
        truth = truth,
        estimate = class,
        event_level = "second"
      )) |>
      dplyr::mutate(conf_mat = list(conf_mat))
  } else {
    preds <- tibble::tibble(truth = y_test, estimate = predictions$predictions)
    res <- preds |>
      yardstick::metrics(truth = truth, estimate = estimate)
  }

  res |>
    dplyr::mutate(model_type = model$TypeDetail)
}

#' Fit configured predictive models for JSTOR period DFMs
#'
#' @param period_dfms Named list of period-specific DFMs.
#' @param splits Named list of rsample splits.
#' @param feature_object Quanteda dictionary for the target feature.
#' @param cfg JSTOR pipeline configuration.
#'
#' @return Tibble with model objects and fit errors.
fit_jstor_predictive_models <- function(period_dfms, splits, feature_object, cfg) {
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

#' Evaluate JSTOR predictive models
#'
#' @param models Tibble returned by `fit_jstor_predictive_models()`.
#' @param period_dfms Named list of period-specific DFMs.
#' @param splits Named list of rsample splits.
#' @param feature_object Quanteda dictionary for the target feature.
#' @param performance_split Split to evaluate.
#'
#' @return Performance tibble.
evaluate_jstor_predictive_models <- function(models,
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
      {
        if (inherits(model, "LiblineaR")) {
          model_performance_jstor_liblinear_dense(
            model = model,
            dfm = period_dfms[[period]],
            initial_split = splits[[period]],
            feat = feature_object,
            weight = weight,
            use = performance_split
          )
        } else {
          model_performance(
            model = model,
            dfm = period_dfms[[period]],
            initial_split = splits[[period]],
            feat = feature_object,
            weight = weight,
            use = performance_split
          )
        }
      } |>
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
          "jstor",
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

#' Write JSTOR predictive performance to an RDS file
#'
#' @param models Tibble returned by `fit_jstor_predictive_models()`.
#' @param period_dfms Named list of period-specific DFMs.
#' @param splits Named list of rsample splits.
#' @param feature_object Quanteda dictionary for the target feature.
#' @param cfg JSTOR pipeline configuration.
#' @param performance_split Split to evaluate.
#'
#' @return Path to written RDS.
write_jstor_predictive_performance <- function(models,
                                               period_dfms,
                                               splits,
                                               feature_object,
                                               cfg,
                                               performance_split = "testing") {
  performance <- evaluate_jstor_predictive_models(
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
  out_path <- file.path(out_dir, paste0(cfg$run_id, "_jstor_combined_performance.rds"))
  saveRDS(performance, out_path)
  out_path
}

#' Summarize JSTOR sample sizes from performance output
#'
#' @param performance Performance tibble.
#'
#' @return Sample-size tibble.
summarize_jstor_sample_sizes <- function(performance) {
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

#' Extract JSTOR model weights
#'
#' @param models Tibble returned by `fit_jstor_predictive_models()`.
#'
#' @return Combined model-weight tibble.
extract_jstor_model_weights <- function(models) {
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
      weight_id = paste("jstor", period, predictive_model_engine, predictive_model_dfm_weight, sep = "_")
    ) |>
    dplyr::select(
      weight_id, period, predictive_model_engine, predictive_model_dfm_weight,
      predictive_model_task, dplyr::everything(), -model
    )
}

#' Build JSTOR model-weight graphs
#'
#' @param weights Combined model weights.
#' @param cfg JSTOR pipeline configuration.
#'
#' @return Tibble with one graph object per engine/pattern.
build_jstor_graphs <- function(weights, cfg) {
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
      graph_id = paste("jstor", cfg$run_id, predictive_model_engine,
                       gsub("[^A-Za-z0-9]+", "_", graph_pos_patterns), sep = "_"),
      title = paste(
        cfg$feature$name,
        "in JSTOR DfR NGRAMS1",
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

#' Slugify a JSTOR target name
#'
#' @param prefix Target prefix.
#' @param run_id Run identifier.
#'
#' @return Target name as character.
jstor_target_name <- function(prefix, run_id) {
  paste(prefix, run_id, sep = "_") |>
    stringr::str_replace_all("[^A-Za-z0-9]+", "_") |>
    stringr::str_replace_all("_+", "_") |>
    stringr::str_remove("^_+") |>
    stringr::str_remove("_+$")
}

#' Write a Quarto report for JSTOR feature graphs
#'
#' @param cfg JSTOR pipeline configuration.
#' @param output_dir Directory for generated reports.
#'
#' @return Path to generated `.qmd`.
write_jstor_graph_qmd <- function(cfg, output_dir = file.path("Paper")) {
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

  report_slug <- cfg$report$slug %||% cfg$report_slug %||% cfg$run_id
  output_path <- file.path(output_dir, paste0("jstor_graphs_", report_slug, ".qmd"))

  feature_name <- cfg$feature$name %||% cfg$feature$id
  tracked_terms <- unique(c(
    cfg$graphs$tracked_terms %||% character(0),
    cfg$feature$graphs$tracked_terms %||% character(0),
    cfg$feature$tracked_terms %||% character(0)
  ))
  tracked_terms <- stringr::str_remove(
    tracked_terms,
    "_(nn|nnp|nns|nnps|vb|vbd|vbg|vbn|vbp|vbz|jj|jjr|jjs)$"
  ) |>
    unique()
  tracked_top_n <- as.integer(
    cfg$graphs$tracked_top_n %||%
      cfg$feature$graphs$tracked_top_n %||%
      cfg$feature$tracked_top_n %||%
      10L
  )
  if (is.na(tracked_top_n) || tracked_top_n < 1L) {
    tracked_top_n <- 10L
  }

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
        "jstor",
        cfg$run_id,
        predictive_model_engine,
        gsub("[^A-Za-z0-9]+", "_", graph_pos_patterns),
        sep = "_"
      ),
      graph_type = paste("Predictive model weights", predictive_model_engine),
      pos_label = dplyr::case_when(
        graph_pos_patterns == "." ~ "All terms",
        TRUE ~ graph_pos_patterns
      ),
      section_title = paste(graph_type, pos_label, sep = " - "),
      caption = paste0(
        graph_type, "; ", pos_label,
        "; top ", cfg$samples$sample_max_texts,
        " feature-positive JSTOR articles per period with matched negative articles. Terms at the top are more closely associated with '",
        feature_name,
        "' in later years. The label marks the peak density of association."
      ),
      chunk_label = paste0(
        "fig-jstor-",
        stringr::str_replace_all(graph_id, "[^A-Za-z0-9]+", "-")
      )
    )

  target <- function(prefix) jstor_target_name(prefix, cfg$run_id)

  header <- c(
    "---",
    paste0("title: ", yaml_quote(paste("JSTOR graphs:", report_slug))),
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
    "library(here)",
    "library(knitr)",
    "library(stringr)",
    "source(here::here(\"R\", \"jstor_corpus_functions.R\"))",
    "targets::tar_config_set(store = here::here(\"_targets\"), config = \"custom.yaml\")",
    "ggplot2::theme_set(ggplot2::theme_bw())",
    "```",
    "",
    paste0("Run: `", cfg$run_id, "`."),
    paste0("Feature(s): ", paste(jstor_feature_terms(cfg), collapse = ", "), "."),
    cfg$run_description %||% "",
    "",
    "This document reads precomputed JSTOR graph objects from the central `_targets` store.",
    "JSTOR DfR NGRAMS1 files are untagged unigram counts, so POS-specific Hathi panels are not generated here.",
    "",
    "## Feature Support Audit",
    "",
    "```{r}",
    "#| label: tbl-jstor-feature-support",
    paste0("audit <- targets::tar_read(", target("jstor_sampling_audit"), ", store = here::here('_targets'))"),
    "support <- jstor_model_period_support(audit, targets::tar_read(",
    paste0("  ", target("jstor_config"), ", store = here::here('_targets'))"),
    ")",
    "support |>",
    "  dplyr::mutate(",
    "    positive_share = round(positive_texts / n_texts, 3)",
    "  ) |>",
    "  dplyr::select(",
    "    period, period_label, n_texts, positive_texts, negative_texts,",
    "    positive_share, feature_tokens, model_trainable",
    "  ) |>",
    paste0("  knitr::kable(caption = \"", feature_name, " feature support by decade in JSTOR DfR NGRAMS1 articles.\")"),
    "```",
    "",
    "## Predictive Performance",
    "",
    paste(
      "F1 is the harmonic mean of precision and recall for JSTOR articles",
      "containing the target feature. These diagnostics are internal to the",
      "configured sample and train/test split; they do not estimate prevalence",
      "in JSTOR."
    ),
    "",
    "```{r}",
    "#| label: fig-jstor-performance-f1",
    "#| fig-cap: \"Testing F1 by model and period.\"",
    "#| fig-width: 8",
    "#| fig-height: 4",
    paste0("performance_path <- targets::tar_read(", target("jstor_combined_performance"), ", store = here::here('_targets'))"),
    "if (!grepl(\"^(?:[A-Za-z]:|/|\\\\\\\\)\", performance_path)) {",
    "  performance_path <- here::here(performance_path)",
    "}",
    "performance <- readRDS(performance_path) |>",
    "  dplyr::mutate(",
    "    model = paste(",
    "      predictive_model_engine,",
    "      predictive_model_dfm_weight,",
    "      \"weights\"",
    "    )",
    "  )",
    "",
    paste0("sample_sizes <- targets::tar_read(", target("jstor_sample_sizes"), ", store = here::here('_targets')) |>"),
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
    "#| label: tbl-jstor-performance-testing",
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
      "terms using a cumulative log-scaled positive-association score:",
      "`sum(log1p(pmax(weight, 0)))` across periods."
    ),
    "",
    "```{r}",
    "#| label: jstor-top-term-data",
    "#| include: false",
    paste0("tracked_top_n <- ", tracked_top_n),
    "",
    paste0("model_weights <- targets::tar_read(", target("jstor_predictive_model_weights"), ", store = here::here('_targets')) |> "),
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
    "    positive_value = pmax(value, 0),",
    "    selection_score = log1p(positive_value)",
    "  ) |> ",
    "  dplyr::group_by(model, word) |> ",
    "  dplyr::summarise(",
    "    association_score = sum(selection_score, na.rm = TRUE),",
    "    periods_positive = sum(positive_value > 0, na.rm = TRUE),",
    "    max_positive_weight = max(positive_value, na.rm = TRUE),",
    "    .groups = \"drop\"",
    "  ) |> ",
    "  dplyr::filter(association_score > 0) |> ",
    "  dplyr::group_by(model) |> ",
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
      paste0(
        "### ", model_label, " - All Terms\n\n",
        "```{r}\n",
        "#| label: fig-jstor-top-term-weights-", model_slug, "-all-terms\n",
        "#| fig-cap: \"Top persistent positive-association terms for ",
        model_label,
        ".\"\n",
        "#| fig-width: 9\n",
        "#| fig-height: ", max(7, 2 + 0.8 * tracked_top_n), "\n",
        "model_name <- ",
        chunk_quote(model_label),
        "\n\n",
        "model_top_terms <- top_terms |> \n",
        "  dplyr::filter(model == model_name) |> \n",
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
        "      subtitle = \"Terms ordered by cumulative log-scaled positive association\"\n",
        "    ) +\n",
        "    ggplot2::theme(\n",
        "      legend.position = \"none\",\n",
        "      strip.text = ggplot2::element_text(face = \"bold\")\n",
        "    )\n",
        "} else {\n",
        "  cat(\"No positive terms available for this model.\")\n",
        "}\n",
        "```\n"
      )
    }
  )

  selected_terms_block <- c(
    "",
    "### Selected Configured Terms",
    "",
    "```{r}",
    "#| label: fig-jstor-selected-term-weights",
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
        paste0("graphs <- targets::tar_read(", target("jstor_graphs"), ", store = here::here('_targets'))\n"),
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
