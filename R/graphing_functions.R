graph_similarities <- function(df,
                               top_n = 8,
                               var = value,
                               max_n = Inf,
                               collapse_cased = TRUE) {

  df <- prep_df(df, top_n, {{var}}, max_n = max_n, collapse_cased = collapse_cased)

  df %>%
    ggplot2::ggplot(ggplot2::aes(y = forcats::fct_reorder(word, decade_density_max))) +
    ggplot2::geom_tile(ggplot2::aes(fill = {{var}}, x = decade)) +
    ggplot2::geom_text(ggplot2::aes(x = decade_density_max, label = decade_density_max), size = 2) +
    ggplot2::theme_bw() +
    ggplot2::labs(y = "") +
    ggplot2::scale_fill_gradient2(midpoint = median(df %>% dplyr::pull({{var}}), na.rm = TRUE))
}

normalize <- function(x) {
  return (x - min(x)) / (max(x) - min(x))
}

weighted_density_max <- function(x, wt) {
  d <- density(x, weight = wt)
  d$x[which(d$y == max(d$y))]

}

prep_df <- function(df, top_n, var, max_n, collapse_cased) {

  if(nrow(df) == 0) {
    return(df)
  }

  if(collapse_cased) {
    df <- df %>%
      dplyr::mutate(word_id = stringr::str_to_lower(word)) %>%
      dplyr::group_by(decade, word_id) %>%
      dplyr::summarise({{var}} := mean({{var}}),
                       word = paste(word, collapse = ", ")) %>%
      dplyr::ungroup()
  }

  terms_to_plot <- df  %>%
    dplyr::group_by(decade) %>%
    dplyr::slice_max({{var}}, n = top_n) %>%
    dplyr::pull(word) %>%
    unique()

  top_terms <- df %>%
    dplyr::filter(word %in% terms_to_plot) %>%
    dplyr::ungroup() %>%
    dplyr::count(word, wt = {{var}}) %>%
    dplyr::slice_max(n, n = max_n) %>%
    dplyr::pull(word)

  terms_to_plot <- terms_to_plot[terms_to_plot %in% top_terms]

  if(nrow(df) == 0) {
    return(df)
  }
  word_years_df <- df %>%
    dplyr::filter(word %in% terms_to_plot) %>%
    dplyr::group_by(word) %>%
    dplyr::filter(!is.nan({{var}}), !is.na({{var}}), dplyr::n() > 2) %>%
    dplyr::mutate(wt = ({{var}} - min({{var}}))/sum({{var}} - min({{var}}))) %>%
    dplyr::filter(!is.nan(wt)) %>%
    dplyr::summarise(decade_min = min(decade),
                     decade_max = max(decade),
                     decade_key = decade[ {{var}} == max({{var}}, na.rm = TRUE) ],
                     decade_density_max = round(weighted_density_max(decade, wt)))

  df %>%
    dplyr::filter(word %in% terms_to_plot) %>%
    dplyr::inner_join(word_years_df)
}
