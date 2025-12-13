#' Reduce a DFM list to top topic-model vocabulary per period
#'
#' Filters the input DFM list to documents containing the target feature, then
#' selects a reduced vocabulary consisting of the top `top_n` positively-weighted
#' terms per period from a weights table. The resulting DFM is restricted to the
#' union of those top terms.
#'
#' @param dfm_list A list of `quanteda` `dfm` objects (typically one per period).
#' @param weights A data frame/tibble of term weights with at least columns
#'   `period`, `word`, and `value`.
#' @param feature A `quanteda` dictionary specifying the target feature used by
#'   `dfm_feature_only_list()`.
#' @param top_n Integer; number of top terms per period to keep.
#'
#' @return A `quanteda` `dfm` restricted to the selected vocabulary.
reduce_dfm <- function(dfm_list, weights, feature, top_n = 50) {
  reduced_dfm <- dfm_feature_only_list(dfm_list, feature)
  features <- weights |>
    dplyr::filter(value > 0) |>
    dplyr::group_by(period) |>
    dplyr::slice_max(value, n = top_n) |>
    dplyr::pull(word) |>
    unique()

  reduced_dfm |>
    quanteda::dfm_select(features)
}

#' Attach topic proportions and labels to document metadata
#'
#' Tidies an `stm` model's document-topic matrix (`theta`), joins document IDs
#' back to `reduced_dfm`, merges in sampling metadata, and adds human-readable
#' topic labels from `stm::labelTopics()`.
#'
#' @param stm_model A fitted `stm` model object.
#' @param reduced_dfm The DFM used to fit the topic model (for docname mapping).
#' @param sample A data frame/tibble of document metadata to join (must match on
#'   `htid` after it is constructed).
#' @param label_type Which label set from `stm::labelTopics()` to use (e.g.,
#'   `"frex"`, `"prob"`, `"lift"`).
#'
#' @return A tibble with one row per document-topic entry, including a
#'   `topic_label` column.
augment_doc_topic <- function(stm_model, reduced_dfm, sample, label_type = "frex") {
  tidied_model <- tidytext::tidy(stm_model, matrix = "theta") |>
    dplyr::mutate(htid = quanteda::docnames(reduced_dfm)[document]) |>
    dplyr::left_join(sample)

  topic_labels <- stm::labelTopics(stm_model)

  tidied_model <- tidied_model |>
    rowwise() |>
    dplyr::mutate(topic_label = topic_labels[[label_type]][topic, 1:7] %>%
                    paste(collapse = ", ")) |>
    ungroup()

  tidied_model
}

#' Embed/cluster documents in topic space with t-SNE
#'
#' Computes a 2D t-SNE embedding of the document-topic matrix (`theta`), removing
#' duplicated theta rows and empty documents to avoid degenerate points, then
#' joins sample metadata.
#'
#' @param stm_model A fitted `stm` model object (must contain `theta`).
#' @param reduced_dfm The DFM used to fit the model (for filtering and docnames).
#' @param sample A data frame/tibble of document metadata to join.
#'
#' @return A tibble with t-SNE coordinates and joined metadata.
cluster_docs <- function(stm_model, reduced_dfm, sample) {
  theta <- stm_model$theta

  duplicated_rows <- theta |> duplicated()

  theta <- theta[ !duplicated_rows, ]

  reduced_dfm <- reduced_dfm |>
    dfm_subset(!as.logical(duplicated_rows))

  row_sums <- reduced_dfm |>
    rowSums()

  reduced_dfm <- reduced_dfm |>
     dfm_subset(row_sums > 0)

  rtsne_dist <- Rtsne::Rtsne(theta)

  res <- rtsne_dist$Y |>
    as_tibble(rownames = "doc") |>
    mutate(htid = quanteda::docnames(reduced_dfm))

  # jsd_dist <- philentropy::JSD(theta)


  # rownames(jsd_dist) <- quanteda::docnames(reduced_dfm)
  #
  # res <- cmdscale(jsd_dist) |>
  #   as_tibble()

  res |>
    left_join(sample)

}
