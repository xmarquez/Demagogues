#' Null-coalescing operator
#'
#' Returns the left-hand value unless it is `NULL`, otherwise returns the
#' right-hand value. Defined here so the Latin prototype can run independently
#' of `object_parameters.R`.
#'
#' @param x Value to test.
#' @param y Fallback value.
#'
#' @return `x` when non-`NULL`, otherwise `y`.
#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Deep merge configuration lists
#'
#' Recursively merges `override` into `base`, replacing scalar values and
#' merging named list values. Used for Latin corpus/run configuration.
#'
#' @param base Base configuration list.
#' @param override Override configuration list.
#'
#' @return A merged list.
#' @keywords internal
latin_deep_merge <- function(base, override) {
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
    out[[key]] <- latin_deep_merge(base[[key]], override[[key]])
  }
  out
}

#' Read a Latin Perseus pipeline configuration
#'
#' Loads shared Latin corpus defaults, one Latin feature definition, and one run
#' definition. The selected run defaults to `LATIN_RUN` and then `TARGET_RUN`.
#'
#' @param run Run ID, matching `config/latin_runs/<run>.yml`.
#' @param config_dir Configuration directory.
#'
#' @return A merged configuration list containing `source`, `feature`, and run
#'   metadata.
read_latin_pipeline_config <- function(run = {
                                         latin_run <- Sys.getenv("LATIN_RUN", "")
                                         if (nzchar(latin_run)) {
                                           latin_run
                                         } else {
                                           Sys.getenv("TARGET_RUN", "auctoritas_perseus")
                                         }
                                       },
                                       config_dir = "config") {
  corpus_path <- file.path(config_dir, "latin_corpus.yml")
  run_path <- file.path(config_dir, "latin_runs", paste0(run, ".yml"))

  if (!file.exists(corpus_path)) {
    stop("Missing Latin corpus config: ", corpus_path, call. = FALSE)
  }
  if (!file.exists(run_path)) {
    stop("Missing Latin run config: ", run_path, call. = FALSE)
  }

  corpus_cfg <- yaml::read_yaml(corpus_path)
  run_cfg <- yaml::read_yaml(run_path)

  feature_id <- run_cfg$feature %||% run_cfg$feature_id
  if (is.null(feature_id) || !nzchar(feature_id)) {
    stop("Latin run must declare `feature`: ", run_path, call. = FALSE)
  }

  feature_path <- file.path(config_dir, "latin_features", paste0(feature_id, ".yml"))
  if (!file.exists(feature_path)) {
    stop("Missing Latin feature config: ", feature_path, call. = FALSE)
  }
  feature_cfg <- yaml::read_yaml(feature_path)
  feature_cfg$id <- feature_cfg$id %||% feature_id
  feature_cfg$name <- feature_cfg$name %||% feature_cfg$id

  source_id <- run_cfg$source %||% run_cfg$source_id %||% "perseus"
  source_cfg <- corpus_cfg$sources[[source_id]]
  if (is.null(source_cfg)) {
    stop("Unknown Latin source `", source_id, "` in ", run_path, call. = FALSE)
  }

  override_cfg <- run_cfg$overrides %||% list()
  direct_overrides <- run_cfg[setdiff(
    names(run_cfg),
    c("id", "description", "feature", "feature_id", "source", "source_id", "outputs", "overrides")
  )]

  cfg <- corpus_cfg
  cfg$source <- source_cfg
  cfg$source$id <- source_id
  cfg <- latin_deep_merge(cfg, direct_overrides)
  cfg <- latin_deep_merge(cfg, override_cfg)

  cfg$feature <- feature_cfg
  cfg$run_id <- run_cfg$id %||% run
  cfg$run_description <- run_cfg$description %||% ""
  cfg$outputs <- run_cfg$outputs %||% cfg$outputs %||% c(
    "latin_ingest", "latin_windows", "latin_weights", "latin_graph"
  )
  cfg$config_source <- run_path
  cfg
}

#' Ensure a local Perseus repository exists
#'
#' Clones the configured Perseus repository when absent. Existing repositories
#' are reused by default to avoid surprising local changes.
#'
#' @param repo_url Git URL for the Perseus repository.
#' @param local_dir Local directory for the clone.
#' @param ref Optional branch, tag, or commit to check out after cloning.
#' @param update Logical; if `TRUE`, fetches updates for an existing clone before
#'   checking out `ref`.
#'
#' @return Normalized path to the local repository directory.
ensure_perseus_repository <- function(repo_url,
                                      local_dir,
                                      ref = NULL,
                                      update = FALSE) {
  local_dir <- normalizePath(local_dir, winslash = "/", mustWork = FALSE)
  git_dir <- file.path(local_dir, ".git")

  run_git <- function(args) {
    status <- system2("git", args = args, stdout = TRUE, stderr = TRUE)
    code <- attr(status, "status") %||% 0L
    if (!identical(code, 0L)) {
      stop("git command failed: git ", paste(args, collapse = " "), "\n",
           paste(status, collapse = "\n"), call. = FALSE)
    }
    invisible(status)
  }

  if (dir.exists(local_dir)) {
    if (!dir.exists(git_dir)) {
      stop("Configured Perseus path exists but is not a git repository: ", local_dir,
           call. = FALSE)
    }
    if (isTRUE(update)) {
      run_git(c("-C", local_dir, "fetch", "--all", "--tags"))
      if (!is.null(ref) && nzchar(ref)) {
        run_git(c("-C", local_dir, "checkout", ref))
      }
    }
    return(normalizePath(local_dir, winslash = "/", mustWork = TRUE))
  }

  parent <- dirname(local_dir)
  if (!dir.exists(parent)) {
    dir.create(parent, recursive = TRUE)
  }

  run_git(c("clone", "--depth", "1", repo_url, local_dir))
  if (!is.null(ref) && nzchar(ref)) {
    run_git(c("-C", local_dir, "checkout", ref))
  }

  normalizePath(local_dir, winslash = "/", mustWork = TRUE)
}

#' List Latin TEI edition files in a Perseus repository
#'
#' Finds XML files under `data/`, excludes CTS metadata files, and keeps Latin
#' editions by filename convention.
#'
#' @param repo_dir Local Perseus repository directory.
#' @param edition_language Edition language marker, usually `"lat"`.
#' @param max_files Optional cap for pilot runs.
#'
#' @return A tibble with one row per TEI file.
list_perseus_latin_files <- function(repo_dir,
                                     edition_language = "lat",
                                     max_files = NULL) {
  data_dir <- file.path(repo_dir, "data")
  if (!dir.exists(data_dir)) {
    stop("Perseus data directory not found: ", data_dir, call. = FALSE)
  }

  files <- list.files(data_dir, pattern = "\\.xml$", recursive = TRUE, full.names = TRUE)
  files <- files[basename(files) != "__cts__.xml"]

  language_pattern <- paste0("-", edition_language, "[0-9]*\\.xml$")
  files <- files[stringr::str_detect(basename(files), language_pattern)]
  files <- sort(files)

  if (!is.null(max_files) && !is.na(max_files) && is.finite(max_files)) {
    files <- utils::head(files, as.integer(max_files))
  }

  repo_norm <- normalizePath(repo_dir, winslash = "/", mustWork = TRUE)
  file_norm <- normalizePath(files, winslash = "/", mustWork = TRUE)
  repo_prefix <- paste0(repo_norm, "/")

  tibble::tibble(
    file = file_norm,
    relative_path = substring(file_norm, nchar(repo_prefix) + 1L),
    text_id = tools::file_path_sans_ext(basename(files)),
    group_id = basename(dirname(dirname(files))),
    work_id = basename(dirname(files))
  )
}

#' Read the first matching XML text value
#'
#' @param doc An `xml2` document.
#' @param xpath XPath expression.
#'
#' @return A single character string, or `NA_character_` when absent.
#' @keywords internal
latin_xml_first_text <- function(doc, xpath) {
  node <- xml2::xml_find_first(doc, xpath)
  if (inherits(node, "xml_missing")) {
    return(NA_character_)
  }
  value <- xml2::xml_text(node, trim = TRUE)
  if (!nzchar(value)) {
    return(NA_character_)
  }
  value
}

#' Read one Perseus TEI text
#'
#' Extracts basic header metadata and body text from a Latin TEI edition. Dates
#' in the TEI header usually describe the modern edition, not ancient
#' composition, so periodization is handled separately.
#'
#' @param file Path to a TEI XML file.
#'
#' @return A one-row tibble with metadata and plain body text.
read_perseus_latin_text <- function(file) {
  doc <- tryCatch(
    xml2::read_xml(file, options = c("RECOVER", "HUGE")),
    error = function(e) e
  )
  if (inherits(doc, "error")) {
    return(tibble::tibble(
      file = normalizePath(file, winslash = "/", mustWork = TRUE),
      text_id = tools::file_path_sans_ext(basename(file)),
      cts_urn = paste0("urn:cts:latinLit:", tools::file_path_sans_ext(basename(file))),
      author = NA_character_,
      title = NA_character_,
      source_title = NA_character_,
      edition_date = NA_character_,
      text = "",
      n_chars = 0L,
      parse_ok = FALSE,
      parse_error = conditionMessage(doc)
    ))
  }
  doc <- xml2::xml_ns_strip(doc)

  editorial_nodes <- xml2::xml_find_all(
    doc,
    ".//text/body//note | .//text/body//bibl | .//text/body//app"
  )
  if (length(editorial_nodes)) {
    xml2::xml_remove(editorial_nodes)
  }

  body_nodes <- xml2::xml_find_all(doc, ".//text/body//text()[normalize-space()]")
  text <- paste(xml2::xml_text(body_nodes, trim = TRUE), collapse = " ")
  text <- stringr::str_squish(text)

  tibble::tibble(
    file = normalizePath(file, winslash = "/", mustWork = TRUE),
    text_id = tools::file_path_sans_ext(basename(file)),
    cts_urn = paste0("urn:cts:latinLit:", tools::file_path_sans_ext(basename(file))),
    author = latin_xml_first_text(doc, ".//teiHeader//titleStmt/author[1]"),
    title = latin_xml_first_text(doc, ".//teiHeader//titleStmt/title[1]"),
    source_title = latin_xml_first_text(doc, ".//teiHeader//sourceDesc//title[1]"),
    edition_date = latin_xml_first_text(doc, ".//teiHeader//sourceDesc//date[1]"),
    text = text,
    n_chars = nchar(text),
    parse_ok = TRUE,
    parse_error = NA_character_
  )
}

#' Convert Latin period definitions to a tibble
#'
#' @param definitions List of period definitions from YAML.
#'
#' @return A tibble with `id`, `label`, `min_year`, and `max_year`.
#' @keywords internal
latin_period_definitions_df <- function(definitions) {
  if (is.null(definitions) || !length(definitions)) {
    return(tibble::tibble(
      id = character(),
      label = character(),
      min_year = numeric(),
      max_year = numeric()
    ))
  }

  purrr::map_dfr(definitions, function(x) {
    tibble::tibble(
      id = as.character(x$id),
      label = as.character(x$label %||% x$id),
      min_year = as.numeric(x$min_year %||% NA_real_),
      max_year = as.numeric(x$max_year %||% NA_real_)
    )
  })
}

#' Assign configured broad periods to Perseus texts
#'
#' Matches text authors and titles against configured metadata rules. This keeps
#' ancient dating assumptions out of the TEI parser and inside an editable YAML
#' block.
#'
#' @param texts Text metadata tibble.
#' @param author_periods List of metadata rules with `pattern`, `period`, and
#'   optional `date_year`.
#' @param period_definitions Broad period definitions.
#' @param default_period Fallback period ID for unmatched texts.
#'
#' @return `texts` with `period`, `period_label`, `date_year`, and
#'   `period_source` columns.
assign_latin_periods <- function(texts,
                                 author_periods = list(),
                                 period_definitions = list(),
                                 default_period = NA_character_) {
  texts <- texts %>%
    dplyr::mutate(
      period = default_period,
      date_year = NA_real_,
      period_source = dplyr::if_else(
        is.na(default_period), "unmatched", "default"
      )
    )

  matched <- rep(FALSE, nrow(texts))
  haystack <- paste(texts$author %||% "", texts$title %||% "", texts$source_title %||% "")

  for (rule in author_periods %||% list()) {
    pattern <- rule$pattern %||% ""
    if (!nzchar(pattern)) {
      next
    }
    hit <- !matched & stringr::str_detect(
      haystack,
      stringr::regex(pattern, ignore_case = TRUE)
    )
    if (any(hit)) {
      texts$period[hit] <- rule$period %||% texts$period[hit]
      texts$date_year[hit] <- as.numeric(rule$date_year %||% texts$date_year[hit])
      texts$period_source[hit] <- pattern
      matched[hit] <- TRUE
    }
  }

  period_defs <- latin_period_definitions_df(period_definitions)
  if (nrow(period_defs)) {
    texts <- texts %>%
      dplyr::left_join(
        period_defs %>% dplyr::select(period = id, period_label = label),
        by = "period"
      )
  } else {
    texts$period_label <- texts$period
  }

  texts
}

#' Read all configured Perseus Latin texts
#'
#' @param manifest File manifest from `list_perseus_latin_files()`.
#' @param cfg Merged Latin pipeline configuration.
#'
#' @return A tibble with one row per TEI edition.
read_perseus_latin_texts <- function(manifest, cfg) {
  texts <- purrr::map_dfr(manifest$file, read_perseus_latin_text)
  texts <- manifest %>%
    dplyr::select(file, relative_path, group_id, work_id) %>%
    dplyr::left_join(texts, by = "file")

  assign_latin_periods(
    texts,
    author_periods = cfg$metadata$author_periods %||% list(),
    period_definitions = cfg$periods$definitions %||% list(),
    default_period = cfg$metadata$default_period %||% NA_character_
  )
}

#' Tokenize Perseus Latin texts
#'
#' Splits body text into one row per token. The prototype is intentionally
#' form-based; lemmatized medieval/classical alignment can be added later.
#'
#' @param texts Text tibble from `read_perseus_latin_texts()`.
#' @param token_pattern Regular expression used to extract tokens.
#'
#' @return A tibble with token positions and document metadata.
tokenize_latin_texts <- function(texts, token_pattern = "\\p{L}+") {
  purrr::pmap_dfr(
    texts %>%
      dplyr::select(
        text_id, cts_urn, author, title, period, period_label, date_year, text
      ),
    function(text_id, cts_urn, author, title, period, period_label, date_year, text) {
      tokens <- stringr::str_extract_all(text, token_pattern)[[1]]
      if (!length(tokens)) {
        return(tibble::tibble())
      }
      tibble::tibble(
        text_id = text_id,
        cts_urn = cts_urn,
        author = author,
        title = title,
        period = period,
        period_label = period_label,
        date_year = date_year,
        position = seq_along(tokens),
        token = tokens,
        token_lower = stringr::str_to_lower(tokens)
      )
    }
  )
}

#' Extract occurrence windows for a Latin feature
#'
#' Finds configured forms of the target lemma and stores left/right concordance
#' text plus the lowercased context tokens around each hit.
#'
#' @param tokens Token tibble from `tokenize_latin_texts()`.
#' @param feature_forms Character vector of target forms.
#' @param window_size Number of tokens to retain on each side.
#'
#' @return A tibble with one row per target occurrence.
extract_latin_windows <- function(tokens,
                                  feature_forms,
                                  window_size = 50) {
  feature_forms <- stringr::str_to_lower(as.character(feature_forms))
  feature_forms <- unique(feature_forms[!is.na(feature_forms) & nzchar(feature_forms)])
  if (!length(feature_forms)) {
    stop("No Latin feature forms were configured.", call. = FALSE)
  }

  by_text <- split(tokens, tokens$text_id)

  purrr::imap_dfr(by_text, function(text_tokens, text_id) {
    hit_rows <- which(text_tokens$token_lower %in% feature_forms)
    if (!length(hit_rows)) {
      return(tibble::tibble())
    }

    purrr::map_dfr(hit_rows, function(hit_index) {
      hit_pos <- text_tokens$position[[hit_index]]
      start_pos <- max(1L, hit_pos - window_size)
      end_pos <- min(max(text_tokens$position), hit_pos + window_size)
      context <- text_tokens %>%
        dplyr::filter(position >= start_pos, position <= end_pos)

      left <- context %>%
        dplyr::filter(position < hit_pos) %>%
        dplyr::pull(token) %>%
        paste(collapse = " ")
      right <- context %>%
        dplyr::filter(position > hit_pos) %>%
        dplyr::pull(token) %>%
        paste(collapse = " ")

      tibble::tibble(
        window_id = paste(text_id, hit_pos, sep = "::"),
        text_id = text_id,
        cts_urn = text_tokens$cts_urn[[hit_index]],
        author = text_tokens$author[[hit_index]],
        title = text_tokens$title[[hit_index]],
        period = text_tokens$period[[hit_index]],
        period_label = text_tokens$period_label[[hit_index]],
        date_year = text_tokens$date_year[[hit_index]],
        position = hit_pos,
        matched_form = text_tokens$token_lower[[hit_index]],
        left_context = left,
        right_context = right,
        concordance = stringr::str_squish(paste(left, "[", text_tokens$token[[hit_index]], "]", right)),
        context_tokens = list(context$token_lower[context$position != hit_pos])
      )
    })
  })
}

#' Summarize Latin feature occurrence windows
#'
#' @param windows Window tibble from `extract_latin_windows()`.
#'
#' @return Counts by period, author, and work.
summarize_latin_windows <- function(windows) {
  if (!nrow(windows)) {
    return(tibble::tibble())
  }

  windows %>%
    dplyr::count(period, period_label, author, title, name = "n_occurrences") %>%
    dplyr::arrange(period, dplyr::desc(n_occurrences), author, title)
}

#' Compute context-term weights from Latin occurrence windows
#'
#' Counts context words around the target lemma and computes a simple
#' group-distinctiveness score. If there is only one group, the score falls back
#' to the term's rate per window.
#'
#' @param windows Window tibble from `extract_latin_windows()`.
#' @param group_vars Character vector of grouping columns, e.g. `"period"` or
#'   `"author"`.
#' @param feature_forms Target forms to remove from context counts.
#' @param stopwords Words to remove.
#' @param min_token_length Minimum token length.
#' @param min_count Minimum count per group.
#'
#' @return A tibble of term weights by group.
latin_window_term_weights <- function(windows,
                                      group_vars = c("period"),
                                      feature_forms = character(),
                                      stopwords = character(),
                                      min_token_length = 3,
                                      min_count = 2) {
  if (!nrow(windows)) {
    return(tibble::tibble())
  }

  group_vars <- intersect(group_vars, names(windows))
  if (!length(group_vars)) {
    stop("No requested group variables exist in `windows`.", call. = FALSE)
  }

  feature_forms <- stringr::str_to_lower(as.character(feature_forms))
  stopwords <- stringr::str_to_lower(as.character(stopwords))

  context <- windows %>%
    dplyr::select(window_id, dplyr::all_of(group_vars), context_tokens) %>%
    tidyr::unnest_longer(context_tokens, values_to = "word") %>%
    dplyr::filter(!is.na(word), nchar(word) >= min_token_length) %>%
    dplyr::filter(!word %in% feature_forms, !word %in% stopwords) %>%
    dplyr::filter(stringr::str_detect(word, "^\\p{L}+$"))

  if (!nrow(context)) {
    return(tibble::tibble())
  }

  group_window_counts <- windows %>%
    dplyr::distinct(window_id, dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::count(dplyr::across(dplyr::all_of(group_vars)), name = "n_windows")

  counts <- context %>%
    dplyr::count(dplyr::across(dplyr::all_of(group_vars)), word, name = "n") %>%
    dplyr::filter(n >= min_count) %>%
    dplyr::left_join(group_window_counts, by = group_vars)

  if (!nrow(counts)) {
    return(tibble::tibble())
  }

  group_totals <- counts %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::summarise(group_total = sum(n), .groups = "drop")
  term_totals <- counts %>%
    dplyr::group_by(word) %>%
    dplyr::summarise(term_total = sum(n), .groups = "drop")
  global_total <- sum(counts$n)
  n_groups <- counts %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(group_vars))) %>%
    nrow()

  counts %>%
    dplyr::left_join(group_totals, by = group_vars) %>%
    dplyr::left_join(term_totals, by = "word") %>%
    dplyr::mutate(
      rate_per_window = n / n_windows,
      p_word_given_group = n / group_total,
      p_word_global = term_total / global_total,
      log_ratio = log2(p_word_given_group / p_word_global),
      distinctiveness = log1p(n) * pmax(log_ratio, 0),
      value = if (n_groups > 1L) distinctiveness else rate_per_window
    ) %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(group_vars)), dplyr::desc(value), word)
}

#' Filter tracked context terms
#'
#' @param term_weights Weights from `latin_window_term_weights()`.
#' @param tracked_terms Terms to retain.
#'
#' @return Filtered term weights.
latin_tracked_term_weights <- function(term_weights, tracked_terms = character()) {
  tracked_terms <- stringr::str_to_lower(as.character(tracked_terms))
  tracked_terms <- tracked_terms[!is.na(tracked_terms) & nzchar(tracked_terms)]
  if (!length(tracked_terms) || !nrow(term_weights)) {
    return(tibble::tibble())
  }
  term_weights %>%
    dplyr::filter(word %in% tracked_terms)
}

#' Clean token vectors for Latin context analysis
#'
#' Removes configured target forms, stopwords, short tokens, and non-letter
#' tokens from a lowercased token vector.
#'
#' @param tokens Character vector of lowercased tokens.
#' @param feature_forms Target forms to remove.
#' @param stopwords Stopwords to remove.
#' @param min_token_length Minimum retained token length.
#'
#' @return A filtered character vector.
#' @keywords internal
latin_clean_context_tokens <- function(tokens,
                                       feature_forms = character(),
                                       stopwords = character(),
                                       min_token_length = 3) {
  tokens <- stringr::str_to_lower(as.character(tokens))
  feature_forms <- stringr::str_to_lower(as.character(feature_forms))
  stopwords <- stringr::str_to_lower(as.character(stopwords))

  tokens[!is.na(tokens) &
           nchar(tokens) >= min_token_length &
           stringr::str_detect(tokens, "^\\p{L}+$") &
           !tokens %in% feature_forms &
           !tokens %in% stopwords]
}

#' Count terms in Latin context windows
#'
#' Converts the list-column of window context tokens into one row per
#' window-term count, preserving period, author, and title metadata.
#'
#' @param windows Window tibble from `extract_latin_windows()`.
#' @param feature_forms Target forms to remove from context counts.
#' @param stopwords Stopwords to remove.
#' @param min_token_length Minimum retained token length.
#'
#' @return A tibble with `window_id`, metadata columns, `word`, and `n`.
make_latin_context_term_counts <- function(windows,
                                           feature_forms = character(),
                                           stopwords = character(),
                                           min_token_length = 3) {
  if (!nrow(windows)) {
    return(tibble::tibble())
  }

  windows %>%
    dplyr::select(
      window_id, text_id, author, title, period, period_label, date_year,
      matched_form, context_tokens
    ) %>%
    dplyr::mutate(
      context_tokens = purrr::map(
        context_tokens,
        latin_clean_context_tokens,
        feature_forms = feature_forms,
        stopwords = stopwords,
        min_token_length = min_token_length
      )
    ) %>%
    tidyr::unnest_longer(context_tokens, values_to = "word") %>%
    dplyr::filter(!is.na(word)) %>%
    dplyr::count(
      window_id, text_id, author, title, period, period_label, date_year,
      matched_form, word,
      name = "n"
    )
}

#' Build a DFM from Latin context-window counts
#'
#' Creates a quanteda DFM with one document per target-occurrence window and one
#' feature per cleaned context token.
#'
#' @param context_counts Counts from `make_latin_context_term_counts()`.
#' @param min_term_count Minimum total count required for a term to be retained.
#'
#' @return A quanteda `dfm` with window metadata stored as docvars.
latin_context_counts_to_dfm <- function(context_counts, min_term_count = 2) {
  if (!nrow(context_counts)) {
    stop("No Latin context counts are available for DFM construction.", call. = FALSE)
  }

  keep_terms <- context_counts %>%
    dplyr::group_by(word) %>%
    dplyr::summarise(term_total = sum(n), .groups = "drop") %>%
    dplyr::filter(term_total >= min_term_count) %>%
    dplyr::pull(word)

  counts <- context_counts %>%
    dplyr::filter(word %in% keep_terms)

  if (!nrow(counts)) {
    stop("No Latin context terms met the minimum count threshold.", call. = FALSE)
  }

  dfm <- tidytext::cast_dfm(counts, window_id, word, n)

  metadata <- counts %>%
    dplyr::distinct(
      window_id, text_id, author, title, period, period_label, date_year,
      matched_form
    )
  metadata <- metadata[match(quanteda::docnames(dfm), metadata$window_id), ]
  quanteda::docvars(dfm) <- metadata %>%
    dplyr::select(-window_id) %>%
    as.data.frame()

  dfm
}

#' Count Latin corpus terms for PPMI baselines
#'
#' Counts terms in the full Perseus Latin corpus after applying the same
#' filtering rules as the context windows. These counts provide the denominator
#' for target-window PMI/PPMI.
#'
#' @param tokens Token tibble from `tokenize_latin_texts()`.
#' @param group_vars Optional grouping columns, such as period metadata.
#' @param feature_forms Target forms to remove from baseline counts.
#' @param stopwords Stopwords to remove.
#' @param min_token_length Minimum retained token length.
#'
#' @return A tibble with grouped corpus term counts.
latin_corpus_term_counts <- function(tokens,
                                     group_vars = character(),
                                     feature_forms = character(),
                                     stopwords = character(),
                                     min_token_length = 3) {
  group_vars <- intersect(group_vars, names(tokens))
  feature_forms <- stringr::str_to_lower(as.character(feature_forms))
  stopwords <- stringr::str_to_lower(as.character(stopwords))

  cleaned <- tokens %>%
    dplyr::mutate(
      word = stringr::str_to_lower(token_lower)
    ) %>%
    dplyr::filter(
      !is.na(word),
      nchar(word) >= min_token_length,
      stringr::str_detect(word, "^\\p{L}+$"),
      !word %in% feature_forms,
      !word %in% stopwords
    )

  if (!nrow(cleaned)) {
    return(tibble::tibble())
  }

  if (length(group_vars)) {
    cleaned %>%
      dplyr::count(dplyr::across(dplyr::all_of(group_vars)), word, name = "corpus_n")
  } else {
    cleaned %>%
      dplyr::count(word, name = "corpus_n")
  }
}

#' Compute PPMI-style association for Latin context terms
#'
#' Compares a term's rate in target-occurrence windows with its rate in the
#' corresponding full-corpus baseline. This is PMI for the event "token occurs
#' inside a target window" and is zeroed below zero to produce PPMI.
#'
#' @param context_counts Counts from `make_latin_context_term_counts()`.
#' @param corpus_counts Baseline counts from `latin_corpus_term_counts()`.
#' @param group_vars Optional shared grouping columns.
#' @param min_count Minimum target-window count to retain.
#' @param base Log base for PMI.
#'
#' @return A tibble of context association weights.
latin_context_ppmi <- function(context_counts,
                               corpus_counts,
                               group_vars = character(),
                               min_count = 2,
                               min_corpus_count = 1,
                               base = 2) {
  group_vars <- intersect(group_vars, intersect(names(context_counts), names(corpus_counts)))
  drop_global_group <- !length(group_vars)

  if (!nrow(context_counts) || !nrow(corpus_counts)) {
    return(tibble::tibble())
  }

  if (drop_global_group) {
    group_vars <- ".latin_group"
    context_counts <- context_counts %>%
      dplyr::mutate(.latin_group = "all")
    corpus_counts <- corpus_counts %>%
      dplyr::mutate(.latin_group = "all")
  }

  context_word_counts <- context_counts %>%
    dplyr::count(dplyr::across(dplyr::all_of(group_vars)), word,
                 wt = n, name = "context_n")
  context_totals <- context_counts %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::summarise(
      context_total = sum(n),
      n_windows = dplyr::n_distinct(window_id),
      .groups = "drop"
    )
  corpus_totals <- corpus_counts %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::summarise(corpus_total = sum(corpus_n), .groups = "drop")

  join_cols <- c(group_vars, "word")

  context_word_counts %>%
    dplyr::filter(context_n >= min_count) %>%
    dplyr::inner_join(corpus_counts, by = join_cols) %>%
    dplyr::filter(corpus_n >= min_corpus_count) %>%
    dplyr::left_join(context_totals, by = group_vars) %>%
    dplyr::left_join(corpus_totals, by = group_vars) %>%
    dplyr::mutate(
      context_rate = context_n / context_total,
      corpus_rate = corpus_n / corpus_total,
      pmi = log(context_rate / corpus_rate, base = base),
      ppmi = pmax(pmi, 0),
      value = ppmi * log1p(context_n)
    ) %>%
    dplyr::filter(ppmi > 0) %>%
    dplyr::arrange(
      dplyr::across(dplyr::all_of(group_vars)),
      dplyr::desc(value),
      word
    ) %>%
    {
      if (drop_global_group) {
        dplyr::select(., -dplyr::all_of(".latin_group"))
      } else {
        .
      }
    }
}

#' Compute SVD over Latin context windows
#'
#' Applies the existing project convention of PPMI or tf-idf weighting to a
#' window-term DFM, then computes a truncated SVD with `irlba`. The returned
#' object keeps both term vectors and window scores.
#'
#' @param context_dfm A quanteda DFM from `latin_context_counts_to_dfm()`.
#' @param dims Number of SVD dimensions.
#' @param weight Weighting scheme: `"ppmi"`, `"tfidf"`, or `"none"`.
#' @param base Log base for PPMI when `weight = "ppmi"`.
#'
#' @return A list containing singular values, window scores, term loadings, and
#'   retained window/term names.
compute_latin_context_svd <- function(context_dfm,
                                      dims = 5,
                                      weight = c("ppmi", "tfidf", "none"),
                                      base = 10) {
  weight <- match.arg(weight)

  weighted <- context_dfm
  if (identical(weight, "ppmi")) {
    weighted <- dfm_ppmi(weighted, base = base)
  } else if (identical(weight, "tfidf")) {
    weighted <- quanteda::dfm_tfidf(weighted)
  }

  mat <- tryCatch(
    methods::as(weighted, "dgCMatrix"),
    error = function(e) Matrix::Matrix(as.matrix(weighted), sparse = TRUE)
  )

  row_keep <- Matrix::rowSums(mat) > 0
  col_keep <- Matrix::colSums(mat) > 0
  mat <- mat[row_keep, col_keep, drop = FALSE]

  if (min(dim(mat)) < 2) {
    return(list(
      d = numeric(),
      u = matrix(nrow = 0, ncol = 0),
      v = matrix(nrow = 0, ncol = 0),
      window_ids = character(),
      terms = character(),
      weight = weight
    ))
  }

  dims <- min(as.integer(dims), min(dim(mat)) - 1L)
  svd <- irlba::irlba(mat, nv = dims)

  for (dim_id in seq_len(ncol(svd$v))) {
    anchor <- which.max(abs(svd$v[, dim_id]))
    if (length(anchor) && svd$v[anchor, dim_id] < 0) {
      svd$u[, dim_id] <- -svd$u[, dim_id]
      svd$v[, dim_id] <- -svd$v[, dim_id]
    }
  }

  colnames(svd$u) <- paste0("dim_", seq_len(ncol(svd$u)))
  colnames(svd$v) <- paste0("dim_", seq_len(ncol(svd$v)))
  rownames(svd$u) <- rownames(mat)
  rownames(svd$v) <- colnames(mat)

  list(
    d = svd$d,
    u = svd$u,
    v = svd$v,
    window_ids = rownames(mat),
    terms = colnames(mat),
    weight = weight
  )
}

#' Extract top SVD term loadings
#'
#' @param svd_object Result from `compute_latin_context_svd()`.
#' @param top_n Number of positive and negative terms to keep per dimension.
#'
#' @return A tibble of dimension, side, word, loading, and absolute loading.
latin_svd_term_loadings <- function(svd_object, top_n = 12) {
  if (!length(svd_object$d)) {
    return(tibble::tibble())
  }

  as.data.frame(svd_object$v) %>%
    tibble::rownames_to_column("word") %>%
    tibble::as_tibble() %>%
    tidyr::pivot_longer(-word, names_to = "dimension", values_to = "loading") %>%
    dplyr::mutate(
      dimension = as.integer(stringr::str_remove(dimension, "^dim_")),
      side = dplyr::if_else(loading >= 0, "positive", "negative"),
      abs_loading = abs(loading)
    ) %>%
    dplyr::group_by(dimension, side) %>%
    dplyr::slice_max(abs_loading, n = top_n, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dimension, side, dplyr::desc(abs_loading), word)
}

#' Extract SVD window scores
#'
#' @param svd_object Result from `compute_latin_context_svd()`.
#' @param windows Window tibble from `extract_latin_windows()`.
#'
#' @return A tibble of window scores joined to window metadata.
latin_svd_window_scores <- function(svd_object, windows) {
  if (!length(svd_object$d)) {
    return(tibble::tibble())
  }

  scores <- sweep(svd_object$u, 2, svd_object$d, `*`)
  scores <- as.data.frame(scores) %>%
    tibble::rownames_to_column("window_id") %>%
    tibble::as_tibble()

  metadata <- windows %>%
    dplyr::distinct(
      window_id, text_id, author, title, period, period_label, date_year,
      matched_form
    )

  scores %>%
    dplyr::left_join(metadata, by = "window_id")
}

#' Write a Quarto report for a Latin Perseus run
#'
#' The generated report reads completed Latin targets from the targets store and
#' mostly presents occurrence summaries, PPMI associations, SVD dimensions,
#' tracked term trajectories, and sample concordance lines.
#'
#' @param cfg Merged Latin pipeline configuration.
#' @param output_dir Directory for the generated `.qmd`.
#'
#' @return Path to the generated `.qmd`.
write_latin_perseus_qmd <- function(cfg, output_dir = here::here("Paper")) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  slug <- stringr::str_replace_all(cfg$run_id, "[^A-Za-z0-9]+", "_") %>%
    stringr::str_replace_all("_+", "_") %>%
    stringr::str_remove("^_") %>%
    stringr::str_remove("_$")
  output_path <- file.path(output_dir, paste0("latin_", slug, ".qmd"))

  title <- paste("Latin Perseus Report:", cfg$feature$name %||% cfg$feature$id)
  description <- cfg$run_description %||% ""

  lines <- c(
    "---",
    paste0("title: \"", gsub("\"", "\\\\\"", title), "\""),
    "format:",
    "  html:",
    "    toc: true",
    "    toc-depth: 3",
    "    toc-location: right",
    "    df-print: paged",
    "execute:",
    "  warning: false",
    "  message: false",
    "---",
    "",
    "```{r setup}",
    "library(dplyr)",
    "library(ggplot2)",
    "library(knitr)",
    "library(stringr)",
    "library(targets)",
    "library(here)",
    "targets::tar_config_set(store = here::here(\"_targets\"), config = \"custom.yaml\")",
    "theme_set(theme_minimal(base_size = 12))",
    "```",
    "",
    if (nzchar(description)) description else NULL,
    "",
    "## Occurrences",
    "",
    "```{r occurrence-summary}",
    "summary <- tar_read(latin_window_summary)",
    "if (nrow(summary)) {",
    "  summary %>%",
    "    arrange(desc(n_occurrences)) %>%",
    "    head(30) %>%",
    "    knitr::kable()",
    "} else {",
    "  cat('No occurrences found for this run.')",
    "}",
    "```",
    "",
    "## PPMI Associations",
    "",
    "### Overall",
    "",
    "```{r global_ppmi_terms}",
    "#| fig-height: 7",
    "#| fig-width: 9",
    "global_ppmi <- tar_read(latin_global_ppmi_weights)",
    "if (nrow(global_ppmi)) {",
    "  plot_df <- global_ppmi %>%",
    "    slice_max(value, n = 30, with_ties = FALSE) %>%",
    "    mutate(word = reorder(word, value))",
    "  ggplot(plot_df, aes(value, word)) +",
    "    geom_col(fill = '#2f5f6a') +",
    "    labs(x = 'PPMI x log count', y = NULL)",
    "} else {",
    "  cat('No global PPMI associations available.')",
    "}",
    "```",
    "",
    "### By Period",
    "",
    "```{r period_ppmi_terms}",
    "#| fig-height: 8",
    "#| fig-width: 10",
    "period_ppmi <- tar_read(latin_period_ppmi_weights)",
    "if (nrow(period_ppmi)) {",
    "  label_col <- if ('period_label' %in% names(period_ppmi)) 'period_label' else 'period'",
    "  plot_df <- period_ppmi %>%",
    "    group_by(.data[[label_col]]) %>%",
    "    slice_max(value, n = 10, with_ties = FALSE) %>%",
    "    ungroup() %>%",
    "    mutate(word = tidytext::reorder_within(word, value, .data[[label_col]]))",
    "  ggplot(plot_df, aes(value, word)) +",
    "    geom_col(fill = '#2f5f6a') +",
    "    facet_wrap(vars(.data[[label_col]]), scales = 'free_y') +",
    "    tidytext::scale_y_reordered() +",
    "    labs(x = 'PPMI x log count', y = NULL)",
    "} else {",
    "  cat('No period PPMI associations available.')",
    "}",
    "```",
    "",
    "## SVD Context Dimensions",
    "",
    "### Term Loadings",
    "",
    "```{r svd_term_loadings}",
    "#| fig-height: 10",
    "#| fig-width: 10",
    "svd_terms <- tar_read(latin_svd_terms)",
    "if (nrow(svd_terms)) {",
    "  plot_df <- svd_terms %>%",
    "    filter(dimension <= 5) %>%",
    "    mutate(",
    "      panel = paste('Dimension', dimension, side),",
    "      signed_loading = if_else(side == 'negative', -abs_loading, abs_loading),",
    "      word = tidytext::reorder_within(word, abs_loading, panel)",
    "    )",
    "  ggplot(plot_df, aes(abs_loading, word, fill = side)) +",
    "    geom_col(show.legend = FALSE) +",
    "    facet_wrap(vars(panel), scales = 'free_y', ncol = 2) +",
    "    tidytext::scale_y_reordered() +",
    "    scale_fill_manual(values = c(negative = '#8a4f45', positive = '#2f5f6a')) +",
    "    labs(x = 'Absolute loading', y = NULL)",
    "} else {",
    "  cat('No SVD term loadings available.')",
    "}",
    "```",
    "",
    "### Window Map",
    "",
    "```{r svd_window_map}",
    "#| fig-height: 6",
    "#| fig-width: 8",
    "svd_windows <- tar_read(latin_svd_windows)",
    "if (all(c('dim_1', 'dim_2') %in% names(svd_windows))) {",
    "  label_col <- if ('period_label' %in% names(svd_windows)) 'period_label' else 'period'",
    "  centroids <- svd_windows %>%",
    "    group_by(.data[[label_col]]) %>%",
    "    summarise(dim_1 = median(dim_1, na.rm = TRUE), dim_2 = median(dim_2, na.rm = TRUE), .groups = 'drop')",
    "  ggplot(svd_windows, aes(dim_1, dim_2, color = .data[[label_col]])) +",
    "    geom_point(alpha = 0.35, size = 1.2) +",
    "    geom_text(data = centroids, aes(label = .data[[label_col]]), color = 'black', size = 3.5) +",
    "    labs(x = 'SVD dimension 1', y = 'SVD dimension 2', color = NULL)",
    "} else {",
    "  cat('SVD did not produce at least two dimensions.')",
    "}",
    "```",
    "",
    "## Period-Distinctive Context Terms",
    "",
    "```{r top_period_terms}",
    "#| fig-height: 7",
    "#| fig-width: 10",
    "period_terms <- tar_read(latin_period_term_weights)",
    "if (nrow(period_terms)) {",
    "  label_col <- if ('period_label' %in% names(period_terms)) 'period_label' else 'period'",
    "  plot_df <- period_terms %>%",
    "    group_by(.data[[label_col]]) %>%",
    "    slice_max(value, n = 12, with_ties = FALSE) %>%",
    "    ungroup() %>%",
    "    mutate(word = tidytext::reorder_within(word, value, .data[[label_col]]))",
    "  ggplot(plot_df, aes(value, word)) +",
    "    geom_col(fill = '#2f5f6a') +",
    "    facet_wrap(vars(.data[[label_col]]), scales = 'free_y') +",
    "    tidytext::scale_y_reordered() +",
    "    labs(x = 'Context weight', y = NULL)",
    "} else {",
    "  cat('No period term weights available.')",
    "}",
    "```",
    "",
    "## Top Context Terms By Author",
    "",
    "```{r top_author_terms}",
    "#| fig-height: 10",
    "#| fig-width: 10",
    "author_terms <- tar_read(latin_author_term_weights)",
    "if (nrow(author_terms)) {",
    "  top_authors <- tar_read(latin_window_summary) %>%",
    "    group_by(author) %>%",
    "    summarise(n_occurrences = sum(n_occurrences), .groups = 'drop') %>%",
    "    slice_max(n_occurrences, n = 8, with_ties = FALSE) %>%",
    "    pull(author)",
    "  plot_df <- author_terms %>%",
    "    filter(author %in% top_authors) %>%",
    "    group_by(author) %>%",
    "    slice_max(value, n = 8, with_ties = FALSE) %>%",
    "    ungroup() %>%",
    "    mutate(word = tidytext::reorder_within(word, value, author))",
    "  ggplot(plot_df, aes(value, word)) +",
    "    geom_col(fill = '#6f5d2f') +",
    "    facet_wrap(vars(author), scales = 'free_y') +",
    "    tidytext::scale_y_reordered() +",
    "    labs(x = 'Context weight', y = NULL)",
    "} else {",
    "  cat('No author term weights available.')",
    "}",
    "```",
    "",
    "## Tracked Terms",
    "",
    "```{r tracked_terms}",
    "#| fig-height: 5",
    "#| fig-width: 9",
    "tracked <- tar_read(latin_tracked_terms_period)",
    "if (nrow(tracked)) {",
    "  label_col <- if ('period_label' %in% names(tracked)) 'period_label' else 'period'",
    "  ggplot(tracked, aes(.data[[label_col]], rate_per_window, group = word)) +",
    "    geom_line(color = '#2f5f6a') +",
    "    geom_point(color = '#2f5f6a') +",
    "    facet_wrap(vars(word), scales = 'free_y') +",
    "    labs(x = NULL, y = 'Uses per occurrence window')",
    "} else {",
    "  cat('No configured tracked terms were found in the period weights.')",
    "}",
    "```",
    "",
    "## Concordance Sample",
    "",
    "```{r concordance-sample}",
    "windows <- tar_read(latin_windows)",
    "if (nrow(windows)) {",
    "  windows %>%",
    "    select(author, title, matched_form, concordance) %>%",
    "    head(30) %>%",
    "    knitr::kable()",
    "} else {",
    "  cat('No concordance lines available.')",
    "}",
    "```"
  )

  readr::write_lines(lines, output_path)
  output_path
}
