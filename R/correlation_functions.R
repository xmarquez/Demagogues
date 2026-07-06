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

#' Rank-based cross-model agreement within each period
#'
#' Complements [correlate_weights_by_period()] with rank-based agreement measures
#' that are more robust to differences in regularizer sparsity across engines. For
#' each period and each ordered pair of models it computes:
#' - `spearman`: the Spearman rank correlation of `normalized_value` over the
#'   pair's common vocabulary (pairwise-complete observations), and
#' - `top_overlap`: the size of the intersection of each model's `top_n`
#'   highest-weighted words divided by `top_n`.
#'
#' @param weight_object A data frame/tibble with at least columns `weight_id`,
#'   `period`, `word`, and `normalized_value`.
#' @param info_predictive_model_weights A metadata table keyed by `weight_id`,
#'   used to attach `predictive_model_engine` and `workset_meta_id` for each model.
#' @param top_n Number of top-weighted words per model used for `top_overlap`.
#'
#' @return A tibble with columns `period`, `term`, `term2`, `spearman`,
#'   `top_overlap`, plus `engine`, `engine2`, `workset_meta_id`, and
#'   `workset_meta_id2`.
rank_agreement_by_period <- function(weight_object, info_predictive_model_weights, top_n = 100) {
  weight_object <- weight_object |>
    select(weight_id, period, word, normalized_value)

  wide <- weight_object |>
    tidyr::pivot_wider(id_cols = c(word, period),
                       names_from = weight_id,
                       values_from = normalized_value)

  spearman <- wide |>
    split(wide$period) |>
    purrr::map(~select(.x, starts_with("weight"))) |>
    purrr::map(~corrr::correlate(.x, method = "spearman", use = "pairwise.complete.obs")) |>
    purrr::list_rbind(names_to = "period") |>
    mutate(period = as.numeric(period)) |>
    tidyr::pivot_longer(starts_with("weight"), names_to = "term2", values_to = "spearman")

  top_words <- weight_object |>
    group_by(period, weight_id) |>
    slice_max(normalized_value, n = top_n, with_ties = FALSE) |>
    summarise(words = list(word), .groups = "drop")

  overlap <- top_words |>
    rename(term = weight_id, words_a = words) |>
    inner_join(top_words |> rename(term2 = weight_id, words_b = words),
               by = "period", relationship = "many-to-many") |>
    mutate(top_overlap = purrr::map2_dbl(
      words_a, words_b, ~length(intersect(.x, .y)) / top_n
    )) |>
    select(period, term, term2, top_overlap)

  spearman |>
    left_join(overlap, by = c("period", "term", "term2")) |>
    mutate(engine = info_predictive_model_weights$predictive_model_engine[
      match(term, info_predictive_model_weights$weight_id)],
      engine2 = info_predictive_model_weights$predictive_model_engine[
        match(term2, info_predictive_model_weights$weight_id)],
      workset_meta_id = info_predictive_model_weights$workset_meta_id[
        match(term, info_predictive_model_weights$weight_id)],
      workset_meta_id2 = info_predictive_model_weights$workset_meta_id[
        match(term2, info_predictive_model_weights$weight_id)]) |>
    filter(!is.na(spearman))
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
