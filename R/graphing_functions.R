#' Plot term similarities over time as a heatmap
#'
#' Prepares a term-by-period table (optionally collapsing case variants) and
#' visualizes the selected terms as a tile plot, with the fill mapped to a
#' similarity/weight column and the x-axis as `period`.
#'
#' The helper `prep_df()` selects up to `top_n` terms per period, then limits the
#' overall number of terms using `max_n` based on total weight across periods.
#'
#' @param df A data frame/tibble with at least `word` and `period` columns and a
#'   numeric value column specified by `var`.
#' @param top_n Integer; number of top terms to consider per period.
#' @param var Unquoted column name giving the numeric value to plot (defaults to
#'   `value`).
#' @param max_n Integer; maximum number of unique terms to include overall.
#' @param collapse_cased Logical; if `TRUE`, collapse case variants by lowercasing
#'   `word` within each period and averaging `var`.
#'
#' @return A `ggplot2` plot object.
graph_similarities <- function(df,
                               top_n = 8,
                               var = value,
                               max_n = Inf,
                               collapse_cased = TRUE) {

  df <- prep_df(df, top_n, {{var}}, max_n = max_n, collapse_cased = collapse_cased)

  df %>%
    ggplot2::ggplot(ggplot2::aes(y = forcats::fct_reorder(word, period_density_max))) +
    ggplot2::geom_tile(ggplot2::aes(fill = {{var}}, x = period)) +
    ggplot2::geom_text(ggplot2::aes(x = period_density_max, label = period_density_max), size = 2) +
    ggplot2::theme_bw() +
    ggplot2::labs(y = "") +
    ggplot2::scale_fill_gradient2(midpoint = median(df %>% dplyr::pull({{var}}), na.rm = TRUE))
}

#' Min-max normalize a numeric vector
#'
#' Scales values to the `[0, 1]` range using `(x - min(x)) / (max(x) - min(x))`.
#'
#' @param x Numeric vector.
#'
#' @return A numeric vector of the same length as `x`.
normalize <- function(x) {
  return (x - min(x)) / (max(x) - min(x))
}

#' Mode of a weighted kernel density estimate
#'
#' Computes a weighted density estimate for `x` and returns the `x` value at
#' which the estimated density is maximized.
#'
#' @param x Numeric vector of observations.
#' @param wt Numeric vector of non-negative weights, same length as `x`.
#'
#' @return A numeric scalar giving the density mode location.
weighted_density_max <- function(x, wt) {
  d <- density(x, weight = wt)
  d$x[which(d$y == max(d$y))]

}

#' Prepare a term-by-period table for plotting
#'
#' Selects terms to plot by taking the top `top_n` terms within each period by
#' `var`, then optionally restricting to the `max_n` terms with the largest total
#' weight across periods. Also computes summary columns per term such as the
#' period range and the density-weighted period mode.
#'
#' @param df A data frame/tibble with columns `word`, `period`, and the numeric
#'   column specified by `var`.
#' @param top_n Integer; number of top terms to include per period.
#' @param var Unquoted column name giving the numeric value to rank/plot.
#' @param max_n Integer; maximum number of unique terms to include overall.
#' @param collapse_cased Logical; if `TRUE`, collapse case variants within each
#'   period by lowercasing and averaging `var`.
#'
#' @return A tibble filtered to the selected terms, with additional columns:
#'   `period_min`, `period_max`, `period_key`, and `period_density_max`.
prep_df <- function(df, top_n, var, max_n, collapse_cased) {

  if(nrow(df) == 0) {
    return(df)
  }

  if(collapse_cased) {
    df <- df %>%
      dplyr::mutate(word_id = stringr::str_to_lower(word)) %>%
      dplyr::group_by(period, word_id) %>%
      dplyr::summarise({{var}} := mean({{var}}),
                       word = paste(word, collapse = ", ")) %>%
      dplyr::ungroup()
  }

  terms_to_plot <- df  %>%
    dplyr::group_by(period) %>%
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
    dplyr::summarise(period_min = min(period),
                     period_max = max(period),
                     period_key = period[ {{var}} == max({{var}}, na.rm = TRUE) ],
                     period_density_max = round(weighted_density_max(period, wt)))

  df %>%
    dplyr::filter(word %in% terms_to_plot) %>%
    dplyr::inner_join(word_years_df)
}
