#' Convert a single Extracted Features JSON file to a DFM
#'
#' Reads a HathiTrust Extracted Features (EF) JSON file and constructs a
#' `quanteda` document-feature matrix (DFM) at the page level.
#'
#' Pages can be filtered by language, minimum sentence count, and whether they
#' contain a token matching `pages_contain`. Tokens are filtered by
#' `include_pattern`, minimum length, and part-of-speech (POS) pattern.
#'
#' Document names are created as `"<htid>_<seq>"`, and page metadata is stored
#' in `docvars()`.
#'
#' @param path Path to an EF JSON file.
#' @param pos_pattern Regex used to keep POS-tagged token features after
#'   flattening (e.g., `"NN|VB|JJ"` matches `token.NN`, `token.VB`, etc.).
#' @param include_pattern Regex used to keep tokens (applied before POS
#'   flattening, on the token string).
#' @param pages_contain Regex; keep only pages where any token name matches this
#'   pattern. Use `"."` to keep any non-empty page.
#' @param min_length Minimum token length to keep.
#' @param page_language Keep only pages whose `calculatedLanguage` equals this
#'   language code (e.g., `"en"`).
#' @param min_sentence_count Keep only pages with at least this many sentences.
#' @param to_lower Logical; if `TRUE`, lowercase feature names after DFM creation.
#'
#' @return A `quanteda` `dfm` with one document per page and `docvars()`
#'   containing page metadata.
json_to_dfm <- function(path,
                        pos_pattern = "NN|VB|JJ",
                        include_pattern = "^\\p{L}+$",
                        pages_contain = ".",
                        min_length = 3,
                        page_language = "en",
                        min_sentence_count = 3,
                        to_lower = TRUE) {
  parsed_json <- jsonlite::read_json(path)

  body <- parsed_json %>%
    purrr::pluck("features", "pages")  %>%
    purrr::map(~purrr::pluck(., "body", "tokenPosCount"))

  pagemeta <- parsed_json %>%
    purrr::pluck("features", "pages") %>%
    purrr::map(function(x) x[c("seq", "tokenCount",
                        "lineCount",
                        "emptyLineCount",
                        "sentenceCount",
                        "calculatedLanguage")]) %>%
    purrr::map(function(x) purrr::discard(x, is.null) %>%
                 tibble::as_tibble_row()) %>%
    purrr::list_rbind()

  is_null_body <- purrr::map_lgl(body, is.null)

  body <- body[!is_null_body]
  pagemeta <- pagemeta[!is_null_body, ]

  body <- body[ pagemeta$calculatedLanguage == page_language &
                  !is.na(pagemeta$calculatedLanguage) &
                  pagemeta$sentenceCount >= min_sentence_count ]

  pagemeta <- pagemeta %>%
    dplyr::filter(calculatedLanguage == page_language,
                  sentenceCount >= min_sentence_count)

  body <- body %>%
    purrr::map(function(x) x[stringr::str_detect(names(x), include_pattern)]) %>%
    purrr::map(function(x) x[stringr::str_length(names(x)) >= min_length]) %>%
    purrr::map(function(x) unlist(x, recursive = FALSE))

  contains_pattern <- body %>%
    purrr::map_lgl(function(x) any(stringr::str_detect(names(x), pages_contain)))

  pagemeta <- pagemeta[contains_pattern, ]

  body <- body[contains_pattern]

  flattened_body <- unlist(body)

  flattened_body <- flattened_body[ stringr::str_detect(names(flattened_body), pos_pattern) ]

  featnames <- sort(unique(names(flattened_body)))

  j <- body %>%
    purrr::map(function(x) match(names(x), featnames)) %>%
    purrr::map(na.omit)

  is_zero <-  (purrr::map_int(j, length) == 0)

  j <- j[!is_zero]

  pagemeta <- pagemeta[!is_zero, ]

  i <- rep(1:length(j), times = lengths(j))

  j <- unlist(j)

  x <- unlist(flattened_body)

  docnames <- paste(parsed_json$htid, as.numeric(pagemeta$seq), sep = "_")

  result <- Matrix::sparseMatrix(i = i, j = j, x = x,
                       dimnames = list(docnames, featnames)) %>%
    quanteda::as.dfm()

  colnames(result) <- stringr::str_replace_all(colnames(result), "(.+)\\.(.+)", "\\1_\\2")

  pagemeta$htid <- parsed_json$htid

  quanteda::docvars(result) <- pagemeta

  result <- result %>%
    quanteda::dfm_subset(subset = quanteda::rowSums(result) > 0)

  if(to_lower) {
    result <- result %>%
      quanteda::dfm_tolower()
  }

  result

}

#' Build a trimmed DFM from multiple EF JSON files
#'
#' Converts each EF JSON file in `paths` via `json_to_dfm()`, pads features so
#' they align across DFMs, row-binds documents, and trims the vocabulary by
#' frequency rank to the top `vocab_size` terms.
#'
#' @param paths Character vector (or list) of EF JSON file paths.
#' @param vocab_size Number of terms to keep after trimming by rank.
#' @param ... Additional arguments passed through to `json_to_dfm()` (e.g.,
#'   `pos_pattern`, `pages_contain`, etc.).
#'
#' @return A combined and trimmed `quanteda` `dfm`.
dfm_from_json <- function(paths,
                          vocab_size = 30000,
                          ...) {
  dfms <- paths %>%
    purrr::map(function(x) {
      json_to_dfm(x, ...)
      })

  new_features <- dfms %>%
    purrr::map(quanteda::featnames) %>%
    purrr::reduce(union)

  new_docvars <- dfms %>%
    purrr::map(quanteda::docvars) %>%
    purrr::list_rbind()

  dfms <- dfms %>%
    purrr::map(function(x) quanteda:::pad_dfm(x, new_features))

  result <- dfms %>%
    purrr::map(function(x) as(x, "sparseMatrix")) %>%
    purrr::reduce(Matrix::rbind2) %>%
    quanteda::as.dfm()

  quanteda::docvars(result) <- new_docvars

  result %>%
    quanteda::dfm_trim(min_termfreq =  vocab_size,
                       termfreq_type = "rank")
}

#' Download/cache EF files and return local JSON paths
#'
#' Uses `hathiTools` to rsync EF JSON files for a set of sampled volumes and
#' returns the local filesystem paths to the cached JSON files.
#'
#' The EF cache directory is derived here from `RESEARCH_DATA_ROOT` rather than
#' relying on the `hathiTools.ef.dir` option set in `_targets.R`: crew workers
#' never source `_targets.R`, so on a worker that option holds the hathiTools
#' load-time default (`./hathi-ef`), which silently misses the real cache.
#'
#' @param democracy_samples A data frame/tibble of sampled volumes accepted by
#'   `hathiTools::rsync_from_hathi()` and `hathiTools::find_cached_htids()` (must
#'   include HTIDs).
#'
#' @return A character vector of local JSON file paths (existing files only).
#' Corrected HTRC rsync path for an htid
#'
#' hathiTools 0.2.0's `stubby_url_to_rsync()` builds remote paths with
#' `id_clean()` (`:` -> `+`, `/` -> `=`) but omits the `.` -> `,` substitution
#' the HTRC rsync tree uses (`id_encode()`), so every htid with a dot in its
#' local id (e.g. `miun.*`, `miua.*`) 404s. The comma encoding applies to both
#' the stub directory and the local-id portion of the filename; the dot
#' separating namespace from local id is preserved. Verified against the live
#' tree 2026-07-07: `miun.abj7655.0004.001` lives at
#' `miun/a750,1/miun.abj7655,0004,001.json.bz2`.
#'
#' @param htids Character vector of HathiTrust ids.
#'
#' @return Character vector of remote paths relative to the rsync module root.
#' @keywords internal
ef_remote_path <- function(htids) {
  parts <- stringr::str_split_fixed(htids, "\\.", 2)
  ns <- parts[, 1]
  enc <- chartr(":/.", "+=,", parts[, 2])
  stub <- vapply(enc, function(x) {
    breaks <- seq(1, by = 3, length.out = ceiling(nchar(x) / 3))
    paste0(strsplit(x, "")[[1]][breaks], collapse = "")
  }, character(1), USE.NAMES = FALSE)
  paste0(ns, "/", stub, "/", ns, ".", enc, ".json.bz2")
}

#' Fetch EF files by comma-encoded remote path, storing at the expected local path
#'
#' Fallback fetcher for the htids `hathiTools::rsync_from_hathi()` cannot
#' retrieve (its encoding bug; see [ef_remote_path()]). Downloads the
#' comma-encoded remote files into `ef_dir` (mirroring the remote layout), then
#' copies each fetched file to the dot-encoded `local_loc` that
#' `find_cached_htids()` expects, so the rest of the pipeline sees a normal
#' cache hit.
#'
#' @param htids Character vector of missing htids.
#' @param local_locs Expected local cache paths for those htids
#'   (`find_cached_htids()$local_loc`), parallel to `htids`.
#' @param ef_dir EF cache root directory.
#'
#' @return Invisibly, a logical vector: which of `local_locs` now exist.
#' @keywords internal
rsync_ef_comma_encoded <- function(htids, local_locs, ef_dir) {
  remote <- ef_remote_path(htids)
  list_file <- tempfile(fileext = ".txt")
  on.exit(unlink(list_file), add = TRUE)
  writeLines(remote, list_file)

  status <- suppressWarnings(system2(
    "rsync",
    c("-a", "--files-from", shQuote(list_file),
      "data.analytics.hathitrust.org::features-2020.03/", shQuote(ef_dir)),
    stdout = FALSE, stderr = FALSE
  ))
  if (!identical(as.integer(status), 0L)) {
    warning("comma-encoded EF rsync fallback exited with status ", status,
            " (some volumes may be genuinely absent upstream).", call. = FALSE)
  }

  fetched <- file.path(ef_dir, remote)
  ok <- file.exists(fetched) & !file.exists(local_locs)
  for (i in which(ok)) {
    dir.create(dirname(local_locs[i]), recursive = TRUE, showWarnings = FALSE)
    file.copy(fetched[i], local_locs[i])
  }
  invisible(file.exists(local_locs))
}

cache_ef_files <- function(democracy_samples) {
  research_data_root <- normalizePath(
    Sys.getenv("RESEARCH_DATA_ROOT", "D:/ResearchData/corpora"),
    winslash = "/",
    mustWork = FALSE
  )
  ef_dir <- file.path(research_data_root, "hathi", "hathi-ef")
  old_opts <- options(hathiTools.ef.dir = ef_dir)
  on.exit(options(old_opts), add = TRUE)

  # Cache hits need no network. The top-up needs an rsync binary: present on
  # Raapoi nodes (incl. parallel-partition compute nodes, verified 2026-07-07)
  # and in the container image; typically absent on Windows dev machines, where
  # the cache is pre-staged.
  rsync_available <- nzchar(Sys.which("rsync"))
  if (!rsync_available) {
    warning("No rsync binary on PATH; relying on the pre-staged EF cache only.",
            call. = FALSE)
  } else {
    # rsync_from_hathi() returns the rsync exit status but never checks it (a
    # missing binary is exit 127 and looks like success to callers that ignore
    # the return value) - so check it here.
    status <- tryCatch(
      hathiTools::rsync_from_hathi(democracy_samples),
      error = function(e) {
        warning("rsync_from_hathi failed: ", conditionMessage(e), call. = FALSE)
        -1L
      }
    )
    if (!identical(as.integer(status), 0L)) {
      warning("rsync_from_hathi exited with status ", status,
              "; relying on the pre-staged cache and the encoding-fixed fetch.",
              call. = FALSE)
    }
  }

  json_paths <- hathiTools::find_cached_htids(democracy_samples, cache_type = "none")

  # hathiTools cannot fetch htids with dots in the local id (encoding bug, see
  # ef_remote_path()); retry exactly the missing ones with corrected paths.
  missing <- !file.exists(json_paths$local_loc)
  if (any(missing) && rsync_available) {
    rsync_ef_comma_encoded(
      json_paths$htid[missing],
      json_paths$local_loc[missing],
      ef_dir
    )
  }

  paths <- json_paths$local_loc
  paths <- paths[file.exists(paths)]

  still_missing <- nrow(democracy_samples) - length(paths)
  if (still_missing > 0L && length(paths) > 0L) {
    warning(still_missing, " of ", nrow(democracy_samples),
            " sampled volumes have no EF file after the rsync top-up; ",
            "they are dropped from this slice.", call. = FALSE)
  }

  # Zero files for a non-empty sample means the cache dir is wrong or the data
  # is unstaged: fail loudly rather than poisoning downstream targets with an
  # empty-but-"completed" result.
  if (length(paths) == 0L && nrow(democracy_samples) > 0L) {
    stop(
      "No cached EF files under ", ef_dir, " for ", nrow(democracy_samples),
      " sampled volumes, and rsync could not fetch them.",
      call. = FALSE
    )
  }

  paths
}

#' Build a trimmed DFM with page-level restriction/downsampling
#'
#' Like `dfm_from_json()`, but uses `restricted_json_to_dfm()` for each JSON
#' file. This supports restricting to pages containing a target term and
#' optionally downsampling pages that do not contain the term (via `multiplier`).
#'
#' @param paths Character vector (or list) of EF JSON file paths.
#' @param vocab_size Number of terms to keep after trimming by rank.
#' @param ... Additional arguments passed through to `restricted_json_to_dfm()`
#'   (e.g., `pages_contain`, `multiplier`, etc.).
#'
#' @return A combined and trimmed `quanteda` `dfm`.
restricted_dfm_from_json <- function(paths,
                                     vocab_size = 30000,
                                     ...) {

  # Loop through each path and convert to a dfm
  dfms <- paths %>%
    purrr::map(function(x) {
      restricted_json_to_dfm(x, ...)
    }) %>%
    purrr::compact()

  # A period slice can legitimately have no volumes (e.g. rare terms in early
  # decades). Return NULL so downstream targets treat the branch as empty
  # rather than aborting the whole pipeline.
  if (length(dfms) == 0L) {
    return(NULL)
  }

  # Get the union of all features from the dfms
  new_features <- dfms %>%
    purrr::map(quanteda::featnames) %>%
    purrr::reduce(union)

  # Row bind all the docvars
  new_docvars <- dfms %>%
    purrr::map(quanteda::docvars) %>%
    purrr::list_rbind()

  # Pad the dfms to have uniform features
  dfms <- dfms %>%
    purrr::map(function(x) quanteda:::pad_dfm(x, new_features))

  # Col bind the dfms into one dfm
  result <- dfms %>%
    purrr::map(function(x) as(x, "sparseMatrix")) %>%
    purrr::reduce(Matrix::rbind2) %>%
    quanteda::as.dfm()

  # Add docvars
  quanteda::docvars(result) <- new_docvars

  # Trim vocabulary
  result %>%
    quanteda::dfm_trim(min_termfreq =  vocab_size,
                       termfreq_type = "rank")
}

#' Restrict a DFM to features present in target-feature documents
#'
#' Subsets the DFM to documents containing `target_feature`, determines which
#' features occur in that subset, and then returns the original DFM restricted to
#' those features.
#'
#' @param dfm A `quanteda` `dfm`.
#' @param target_feature A `quanteda` dictionary used with `dfm_lookup()` to
#'   identify documents containing the target feature.
#' @param pattern Unused (reserved for backwards compatibility).
#'
#' @return A `quanteda` `dfm` containing only features that appear in documents
#'   where `target_feature` is present.
compress_dfm_to_target_features <- function(dfm, target_feature, pattern) {

  # Find which docs contain the target feature
  docs_containing_feature <- dfm %>%
    quanteda::dfm_lookup(target_feature) %>%
    rowSums()

  # Subset the dfm to those docs
  new_dfm <- dfm %>%
    quanteda::dfm_subset(docs_containing_feature > 0)

  # Get the features in this subset
  new_features <- new_dfm %>%
    quanteda::dfm_trim(min_termfreq = 1) %>% # Keep all features with freq > 0
    quanteda::featnames() # Extract feature names

  # Subset the original dfm to these features
  dfm[, new_features]

}

#' Convert an EF JSON file to a DFM with page-level downsampling
#'
#' Reads an EF JSON file and builds a page-level DFM, keeping all pages that
#' contain the `pages_contain` token pattern and optionally downsampling pages
#' that do not contain it.
#'
#' Downsampling is controlled by `multiplier`: when finite, up to
#' `sample_size * multiplier` non-matching pages are sampled (with replacement)
#' and kept. When `multiplier` is `Inf`, all non-matching pages are kept.
#'
#' @param path Path to an EF JSON file.
#' @param pos_pattern Regex used to keep POS-tagged token features after
#'   flattening (e.g., `"NN|VB|JJ"`).
#' @param include_pattern Regex used to keep tokens (applied before POS
#'   flattening).
#' @param pages_contain Token pattern (interpreted as a regex, case-insensitive)
#'   used to define "matching" pages.
#' @param min_length Minimum token length to keep.
#' @param page_language Keep only pages whose `calculatedLanguage` equals this
#'   language code.
#' @param min_sentence_count Keep only pages with at least this many sentences.
#' @param to_lower Logical; if `TRUE`, lowercase feature names after DFM creation.
#' @param multiplier Ratio of non-matching pages to keep per matching page.
#'   Use `Inf` to keep all pages.
#'
#' @return A `quanteda` `dfm` (or a null DFM if no pages match `pages_contain`).
restricted_json_to_dfm <- function(path,
                                   pos_pattern = "NN|VB|JJ",
                                   include_pattern = "^\\p{L}+$",
                                   pages_contain = "democracy",
                                   min_length = 3,
                                   page_language = "en",
                                   min_sentence_count = 3,
                                   to_lower = TRUE,
                                   multiplier = 1) {
  parsed_json <- jsonlite::read_json(path)

  body <- parsed_json %>%
    purrr::pluck("features", "pages")  %>%
    purrr::map(~purrr::pluck(., "body", "tokenPosCount"))

  pagemeta <- parsed_json %>%
    purrr::pluck("features", "pages") %>%
    purrr::map(function(x) x[c("seq", "tokenCount",
                               "lineCount",
                               "emptyLineCount",
                               "sentenceCount",
                               "calculatedLanguage")]) %>%
    purrr::map(function(x) purrr::discard(x, is.null) %>%
                 tibble::as_tibble_row()) %>%
    purrr::list_rbind()

  required_pagemeta <- c("seq", "sentenceCount", "calculatedLanguage")
  missing_pagemeta <- setdiff(required_pagemeta, names(pagemeta))
  for(missing_col in missing_pagemeta) {
    pagemeta[[missing_col]] <- NA
  }
  pagemeta$calculatedLanguage <- as.character(pagemeta$calculatedLanguage)
  pagemeta$sentenceCount <- suppressWarnings(as.numeric(pagemeta$sentenceCount))

  is_null_body <- purrr::map_lgl(body, is.null)

  body <- body[!is_null_body]
  pagemeta <- pagemeta[!is_null_body, ]

  keep_pages <- !is.na(pagemeta$calculatedLanguage) &
    pagemeta$calculatedLanguage == page_language &
    !is.na(pagemeta$sentenceCount) &
    pagemeta$sentenceCount >= min_sentence_count

  body <- body[keep_pages]
  pagemeta <- pagemeta[keep_pages, , drop = FALSE]

  if(!length(body)) {
    return(quanteda:::make_null_dfm())
  }

  body <- body %>%
    purrr::map(function(x) x[stringr::str_detect(names(x), include_pattern)]) %>%
    purrr::map(function(x) x[stringr::str_length(names(x)) >= min_length]) %>%
    purrr::map(function(x) unlist(x, recursive = FALSE))

  contains_pattern <- body %>%
    purrr::map_lgl(function(x) any(
      stringr::str_detect(
        names(x),
        stringr::regex(pages_contain, ignore_case = TRUE))))

  sample_size <- sum(contains_pattern)
  if(!sample_size) {
    return(quanteda:::make_null_dfm())
  }

  idx_pattern <- which(contains_pattern)
  idx_no_pattern <- which(!contains_pattern)

  if(!is.infinite(multiplier) && length(idx_no_pattern) > 0) {
    idx_no_pattern <- idx_no_pattern %>%
      sample(sample_size * multiplier, replace = TRUE) %>%
      unique()
  }

  pagemeta <- pagemeta[c(idx_pattern, idx_no_pattern), ]

  body <- body[c(idx_pattern, idx_no_pattern)]

  flattened_body <- unlist(body)

  flattened_body <- flattened_body[ stringr::str_detect(names(flattened_body), pos_pattern) ]

  if(!length(flattened_body)) {
    return(quanteda:::make_null_dfm())
  }

  featnames <- sort(unique(names(flattened_body)))

  j <- body %>%
    purrr::map(function(x) match(names(x), featnames)) %>%
    purrr::map(na.omit)

  is_zero <-  (purrr::map_int(j, length) == 0)

  j <- j[!is_zero]

  pagemeta <- pagemeta[!is_zero, ]

  if(!length(j)) {
    return(quanteda:::make_null_dfm())
  }

  i <- rep(1:length(j), times = lengths(j))

  j <- unlist(j)

  x <- unlist(flattened_body)

  docnames <- paste(parsed_json$htid, as.numeric(pagemeta$seq), sep = "_")

  result <- Matrix::sparseMatrix(i = i, j = j, x = x,
                                 dimnames = list(docnames, featnames)) %>%
    quanteda::as.dfm()

  colnames(result) <- stringr::str_replace_all(colnames(result), "(.+)\\.(.+)", "\\1_\\2")

  pagemeta$htid <- parsed_json$htid

  quanteda::docvars(result) <- pagemeta

  result <- result %>%
    quanteda::dfm_subset(subset = quanteda::rowSums(result) > 0)

  if(to_lower) {
    result <- result %>%
      quanteda::dfm_tolower()
  }

  result

}

#' Group a page-level DFM by volume and feature presence
#'
#' Adds a `feature` indicator derived from `target_feature` to `docvars()`, then
#' groups documents by `split = paste(htid, feature, sep = \"_\")`. This
#' effectively collapses pages into two documents per volume: feature-present vs
#' feature-absent (depending on the lookup result).
#'
#' @param dfm A `quanteda` `dfm` with `docvars(dfm)$htid` present.
#' @param target_feature A `quanteda` dictionary used with `dfm_lookup()` to
#'   create the feature indicator.
#'
#' @return A grouped `quanteda` `dfm`.
collapse_dfm <- function(dfm, target_feature) {
  target_column <- dfm %>%
    quanteda::dfm_lookup(target_feature) %>%
    quanteda::convert(to = "data.frame")

  docvars(dfm) <- docvars(dfm) %>%
    dplyr::bind_cols(target_column %>%
                       dplyr::rename_with(~"feature", dplyr::starts_with(names(target_feature)))) %>%
    dplyr::mutate(split = paste(htid, as.character(feature), sep = "_"))

  dfm %>%
    dfm_group(groups = split)

}

#' Filter a DFM to documents containing a target feature
#'
#' Subsets the DFM to documents where `dfm_lookup(target_feature)` is non-zero.
#' Optionally groups the resulting DFM by `htid` to aggregate pages to volumes.
#'
#' @param dfm A `quanteda` `dfm`.
#' @param target_feature A `quanteda` dictionary used with `dfm_lookup()` to
#'   identify feature-containing documents.
#' @param by_htid Logical; if `TRUE`, group the filtered DFM by `htid`.
#'
#' @return A filtered (and optionally grouped) `quanteda` `dfm`.
dfm_feature_only <- function(dfm, target_feature, by_htid = TRUE) {
  # Find which docs contain the target feature
  docs_containing_feature <- dfm %>%
    quanteda::dfm_lookup(target_feature) %>%
    rowSums()

  # Subset the dfm to those docs
  dfm <- dfm %>%
    quanteda::dfm_subset(docs_containing_feature > 0) %>%
    quanteda::dfm_trim(min_termfreq = 1)

  if(by_htid) {
    dfm <- dfm %>%
      quanteda::dfm_group(htid)
  }

  dfm

}

#' Filter and combine a list of DFMs to feature-containing documents
#'
#' Applies `dfm_feature_only()` to each DFM in `dfm_list`, pads features so they
#' align across DFMs, row-binds documents, and attaches combined docvars.
#'
#' @param dfm_list List of `quanteda` `dfm` objects.
#' @param target_feature A `quanteda` dictionary used to identify
#'   feature-containing documents.
#' @param by_htid Logical; passed to `dfm_feature_only()` to control grouping.
#'
#' @return A combined `quanteda` `dfm` containing only feature-containing documents.
dfm_feature_only_list <- function(dfm_list, target_feature, by_htid = TRUE) {
  dfms <- dfm_list %>%
    purrr::map(function(x) dfm_feature_only(x, target_feature = target_feature,
                                     by_htid = by_htid))

  # Get the union of all features from the dfms
  new_features <- dfms %>%
    purrr::map(quanteda::featnames) %>%
    purrr::reduce(union)

  # Row bind all the docvars
  new_docvars <- dfms %>%
    purrr::map(quanteda::docvars) %>%
    purrr::list_rbind()

  # Pad the dfms to have uniform features
  dfms <- dfms %>%
    purrr::map(function(x) quanteda:::pad_dfm(x, new_features))

  # Col bind the dfms into one dfm
  result <- dfms %>%
    purrr::map(function(x) as(x, "sparseMatrix")) %>%
    purrr::reduce(Matrix::rbind2) %>%
    quanteda::as.dfm()

  # Add docvars
  quanteda::docvars(result) <- new_docvars

  result

}

