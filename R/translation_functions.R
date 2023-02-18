democracy_translations_freqs <- function(translations) {
  top_langs <- readxl::read_excel(translations)

  multilingual_result <- purrr::map2_df(top_langs$translation, top_langs$language,
                                        ~{hathiTools::query_bookworm(.x,
                                                                     counttype = c("WordsPerMillion",
                                                                                   "WordCount",
                                                                                   "TotalWords"),
                                                                     lims = c(1700, 2020),
                                                                     languages = .y)},
                                        .id = "language")

  multilingual_result <- multilingual_result %>%
    dplyr::filter(value != 0) %>%
    dplyr::mutate(language = as.integer(language),
                  language = top_langs$language[language]) %>%
    dplyr::left_join(hathiTools::iso639 %>%
                       dplyr::select(`alpha3-b`, English) %>%
                       magrittr::set_names(c("language", "language_long")))

  multilingual_result

}
