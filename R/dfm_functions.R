compute_dfm <- function(dataset, cache_format = "rds",
                        max_features = 30000, min_length = 3,
                        min_sentence_count = 3) {

  dfm <- dataset |>
    hathiTools::read_cached_htids(cache_format = cache_format, cache_type = c("ef", "pagemeta")) |>
    dplyr::filter(sentenceCount >= min_sentence_count,
                  stringr::str_detect(POS, "NN|JJ|VB"),
                  stringr::str_length(token) >= min_length,
                  stringr::str_detect(token, "^\\p{L}+$"),
                  section == "body") |>
    dplyr::mutate(token = paste(token, POS, sep = "_"),
                  doc_id = paste(htid, page)) |>
    dplyr::count(doc_id, token, wt = count) |>
    dplyr::mutate(token = stringr::str_remove(token, "(?<=_[NJVB]{2}).+")) |>
    tidytext::cast_dfm(doc_id, token, n) |>
    quanteda::dfm_tolower() |>
    quanteda::dfm_trim(max_features, termfreq_type = "rank")

  dfm

}
