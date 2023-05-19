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

dfm_from_json <- function(paths,
                          vocab_size = 30000,
                          ...) {
  pb <- cli::cli_progress_bar("Building dfms from JSON EF files", total = length(paths))

  dfms <- paths %>%
    purrr::map(function(x) {
      cli::cli_progress_update(status = paste("Now reading", x), id = pb);
      json_to_dfm(x, ...)
      })
  cli::cli_progress_done()

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

  browser()

  result %>%
    quanteda::dfm_trim(min_termfreq =  vocab_size,
                       termfreq_type = "rank")
}

cache_ef_files <- function(democracy_samples) {
  hathiTools::rsync_from_hathi(democracy_samples)
  json_paths <- hathiTools::find_cached_htids(democracy_samples, cache_type = "none")
  json_paths$local_loc
}