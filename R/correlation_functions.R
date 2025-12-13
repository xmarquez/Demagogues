#' Correlate weight vectors within each period
#'
#' Takes a long weights table, reshapes it into a word-by-model matrix for each
#' period, computes pairwise Pearson correlations across models using
#' `corrr::correlate()`, and returns a tidy long table with model metadata
#' attached.
#'
#' @param weight_object A data frame/tibble with at least columns `weight_id`,
#'   `period`, `word`, and `normalized_value`.
#' @param info_predictive_model_weights A metadata table keyed by `weight_id`,
#'   used to attach `predictive_model_engine` and `workset_meta_id` for each model.
#'
#' @return A tibble with columns `period`, `term`, `term2`, `value` (correlation),
#'   plus `engine`, `engine2`, `workset_meta_id`, and `workset_meta_id2`.
correlate_weights_by_period <- function(weight_object, info_predictive_model_weights) {
  weight_object <- weight_object |>
    select(weight_id, period, word, normalized_value) |>
    tidyr::pivot_wider(id_cols = c(word, period),
                       names_from = weight_id,
                       values_from = normalized_value)

  correlations <- weight_object |>
    split(weight_object$period) |>
    purrr::map(~select(.x, starts_with("weight"))) |>
    purrr::map(corrr::correlate) |>
    purrr::list_rbind(names_to = "period") |>
    mutate(period = as.numeric(period)) |>
    tidyr::pivot_longer(starts_with("weight"), names_to = "term2")

  correlations |>
    mutate(engine = info_predictive_model_weights$predictive_model_engine[
      match(term, info_predictive_model_weights$weight_id)],
      engine2 = info_predictive_model_weights$predictive_model_engine[
        match(term2, info_predictive_model_weights$weight_id)],
      workset_meta_id = info_predictive_model_weights$workset_meta_id[
        match(term, info_predictive_model_weights$weight_id)],
      workset_meta_id2 = info_predictive_model_weights$workset_meta_id[
        match(term2, info_predictive_model_weights$weight_id)]) |>
    filter(!is.na(value))
}

#' Jensen–Shannon divergence of weight vectors within each period
#'
#' For each period, converts the long weights table into a model-by-word matrix,
#' computes pairwise Jensen–Shannon divergences using `philentropy::JSD()`, and
#' returns a tidy long table with model metadata attached.
#'
#' @param weight_object A data frame/tibble with at least columns `weight_id`,
#'   `period`, `word`, and `normalized_value`.
#' @param info_predictive_model_weights A metadata table keyed by `weight_id`,
#'   used to attach `predictive_model_engine` and `workset_meta_id` for each model.
#'
#' @return A tibble with columns `period`, `term`, `term2`, `value` (JSD),
#'   plus `engine`, `engine2`, `workset_meta_id`, and `workset_meta_id2`.
jsd_weights_by_period <- function(weight_object, info_predictive_model_weights) {
  weight_object <- weight_object |>
    select(weight_id, period, word, normalized_value)

  jsd_mats <- weight_object |>
    split(weight_object$period) |>
    purrr::map(~tidytext::cast_dfm(.x, weight_id, word, normalized_value) |>
                 as.matrix())

  dimnames <- jsd_mats |>
    purrr::map(~dimnames(.x)[[1]])

  jsd_df <- jsd_mats |>
    purrr::map2(dimnames, ~philentropy::JSD(.x) |>
                 `dimnames<-`(list(.y, .y))) |>
    purrr::map(~as_tibble(.x, rownames = "term"))  |>
    purrr::list_rbind(names_to = "period") |>
    mutate(period = as.numeric(period)) |>
    tidyr::pivot_longer(starts_with("weight"), names_to = "term2")

  jsd_df |>
    mutate(engine = info_predictive_model_weights$predictive_model_engine[
      match(term, info_predictive_model_weights$weight_id)],
      engine2 = info_predictive_model_weights$predictive_model_engine[
        match(term2, info_predictive_model_weights$weight_id)],
      workset_meta_id = info_predictive_model_weights$workset_meta_id[
        match(term, info_predictive_model_weights$weight_id)],
      workset_meta_id2 = info_predictive_model_weights$workset_meta_id[
        match(term2, info_predictive_model_weights$weight_id)]) |>
    filter(value != 0)
}
