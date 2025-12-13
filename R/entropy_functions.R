#' Pairwise KL divergence for a sparse DFM
#'
#' Computes the Kullback–Leibler divergence `KL(P || Q)` between every pair of
#' document distributions in a `quanteda` DFM, treating each document as a
#' probability distribution over features (`dfm_weight(dfm, "prop")`).
#'
#' This implementation operates directly on the underlying sparse matrix slots
#' and returns a dense document-by-document matrix.
#'
#' @param dfm A count-weighted `quanteda` `dfm` (documents x features).
#' @param epsilon Small smoothing constant used when `Q` has zero probability
#'   for a feature present in `P`.
#'
#' @return A dense numeric matrix of KL divergences with row/column names set to
#'   `docnames(dfm)`.
kl_sparse <- function(dfm, epsilon = 1e-5) {
  dfm <- dfm_weight(dfm, "prop")
  dp <- diff(dfm@p)
  col_idx <- rep(seq_along(dp), dp)
  row_idx <- dfm@i + 1
  dense_matrix <- matrix(0, nrow = dfm@Dim[1], ncol = dfm@Dim[1],
                         dimnames = c(unname(dfm@Dimnames[1]),
                                      unname(dfm@Dimnames[1])))
  for(i in 1:nrow(dense_matrix)) {
    for(j in 1:ncol(dense_matrix)) {
      idx_features_P <- which(row_idx == i)
      idx_features_Q <- which(row_idx == j)
      cols_P <- col_idx[idx_features_P]
      cols_Q <- col_idx[idx_features_Q]
      cols_P_and_Q <- union(cols_P, cols_Q)
      cols_P_and_Q_nonzero <- intersect(cols_P, cols_Q)
      cols_P_and_Q_zero <- setdiff(cols_P, cols_Q)
      P <- dfm@x[idx_features_P]
      Q <- dfm@x[idx_features_Q]
      PQ_ratio <- numeric(length(P))
      PQ_ratio[cols_P %in% cols_P_and_Q_nonzero] <- P[cols_P %in% cols_P_and_Q_nonzero]/Q[cols_Q %in% cols_P_and_Q_nonzero]
      PQ_ratio[cols_P %in% cols_P_and_Q_zero] <- 1/epsilon
      dense_matrix[i,j] <- sum(P*log2(PQ_ratio))
    }
  }
  dense_matrix
}

#' Intersection of vocabularies across groups
#'
#' Computes the set of words that appear in every level of a grouping variable.
#'
#' @param weight_object A data frame/tibble containing at least a `word` column
#'   and a grouping column.
#' @param var Grouping column name (string or symbol) used to define subsets for
#'   the intersection.
#'
#' @return A character vector of words common to all groups.
vocab_intersection <- function(weight_object, var) {
  vocab_intersection <- unique(weight_object$word)
  weight_object <- weight_object
  for(i in unique(weight_object[[var]])) {
    partial_vocab <- weight_object[ weight_object[[var]] == i, ] |>
      pull(word)
    vocab_intersection <- intersect(vocab_intersection, partial_vocab)
  }
  vocab_intersection
}

#' Pairwise KL divergence across grouped weight distributions
#'
#' Builds a DFM from a long weight table (e.g., model weights by period), turns
#' each group into a probability distribution over words, and computes pairwise
#' KL divergences using `philentropy::kullback_leibler_distance()`.
#'
#' @param weight_object A data frame/tibble with (at minimum) columns `period`,
#'   `word`, and `value`.
#' @param var Grouping column (string or symbol). Used for optional vocabulary
#'   intersection and for shifting weights by subtracting the group minimum.
#' @param common_vocab_only Logical; if `TRUE`, restrict to the vocabulary shared
#'   by all groups (via `vocab_intersection()`).
#' @param epsilon Smoothing constant passed to
#'   `philentropy::kullback_leibler_distance()`.
#'
#' @return A numeric matrix of KL divergences with dimnames taken from the DFM
#'   document names.
kl_simple <- function(weight_object, var, common_vocab_only = FALSE, epsilon = 1e-10) {
  if(common_vocab_only) {
    vocab_intersect <- vocab_intersection(weight_object, var)
    weight_object <- weight_object |>
      filter(word %in% vocab_intersect)
  }
  dfm <- weight_object |>
    group_by(!!var) |>
    mutate(value = value - min(value),
           period_var = period) |>
    tidytext::cast_dfm(period_var, word, value) |>
    quanteda::dfm_weight("prop")

  kl <- matrix(0, nrow = nrow(dfm), ncol = nrow(dfm))
  dimnames(kl) <- list(quanteda::docnames(dfm), quanteda::docnames(dfm))


  for(i in 1:nrow(kl)) {
    for(j in 1:ncol(kl)) {
      kl[i, j] <- philentropy::kullback_leibler_distance(as.matrix(dfm[i, ]),
                                                         as.matrix(dfm[j, ]),
                                                         testNA = FALSE,
                                                         unit = "log2",
                                                         epsilon = epsilon)
    }
  }

  kl
}

#' Convert a KL matrix into novelty/transience/resonance measures
#'
#' Takes a square KL matrix (e.g., from `kl_simple()`), converts it to tidy long
#' form, and derives:
#' - novelty: divergences from later to earlier periods (`P > Q`)
#' - transience: divergences from earlier to later periods (`P < Q`)
#' - resonance: novelty minus transience
#'
#' @param kl_matrix A square numeric matrix with dimnames that can be coerced to
#'   numeric periods.
#'
#' @return A tibble in long form with columns for period identifiers, the
#'   `measure` (`novelty`, `transience`, `resonance`), and the corresponding
#'   value.
kl_matrix_to_df <- function(kl_matrix) {
  kl_df <- kl_matrix |>
    as_tibble(rownames = "P") |>
    tidyr::pivot_longer(-P, names_to = "Q") |>
    filter(value != 0) |>
    mutate(across(P:Q, as.numeric))

  kl_novelty <- kl_df |>
    filter(P > Q) |>
    rename(novelty = value,
           past_period = Q)

  kl_transience <- kl_df |>
    filter(P < Q) |>
    rename(transience = value,
           future_period = Q)

  kl_df <- kl_novelty |>
    full_join(kl_transience, relationship = "many-to-many") |>
    mutate(resonance = novelty - transience) |>
    pivot_longer(c(novelty, transience, resonance), names_to = "measure")

  kl_df
}

#' Entropy of weight distributions by period
#'
#' Treats each `(weight_id, period)` row as a probability distribution over
#' words and computes Shannon entropy `-sum(p * log(p))` (natural log).
#'
#' @param weight_object A data frame/tibble with columns `weight_id`, `period`,
#'   `word`, and `normalized_value`.
#'
#' @return A tibble with columns `weight_id`, `period` (numeric), and `entropy`.
entropy <- function(weight_object) {
  dfm <- weight_object |>
    mutate(period_var = paste(weight_id, period)) |>
    tidytext::cast_dfm(period_var, word, normalized_value) |>
    quanteda::dfm_weight("prop")

  dfm <- dfm |>
    quanteda::dfm_subset(!is.nan(rowSums(dfm)))

  entropy_vec <- numeric(nrow(dfm))
  for(i in 1:nrow(dfm)) {
    current_row <- suppressWarnings(quanteda::dfm_trim(dfm[i, ], 0)) |>
      as.vector()
    entropy_vec[i] <- -sum(current_row * log(current_row))
  }

  tibble(entropy_row = quanteda::docnames(dfm),
         entropy = entropy_vec) |>
    tidyr::separate_wider_delim(entropy_row, delim = " ", names = c("weight_id", "period")) |>
    mutate(period = as.numeric(period))
}

#' Pairwise Jensen–Shannon divergence within a single period
#'
#' Filters a long weight table to a single period, converts each `weight_id` into
#' a probability distribution over words, and computes pairwise Jensen–Shannon
#' divergences with `philentropy::jensen_shannon()`.
#'
#' @param weight_object A data frame/tibble with columns `weight_id`, `period`,
#'   `word`, and `value`.
#' @param period_filter Period value to filter on before computing divergences.
#' @param epsilon Unused (reserved for future use; included for signature
#'   compatibility).
#'
#' @return A numeric matrix of Jensen–Shannon divergences with dimnames from the
#'   constructed DFM.
jsd_simple <- function(weight_object, period_filter = 1700, epsilon = 1e-10) {
  dfm <- weight_object |>
    filter(period == period_filter) |>
    group_by(weight_id, period) |>
    mutate(value = value - min(value)) |>
    tidytext::cast_dfm(weight_id, word, value) |>
    quanteda::dfm_weight("prop")

  jsd <- matrix(0, nrow = nrow(dfm), ncol = nrow(dfm))
  dimnames(jsd) <- list(quanteda::docnames(dfm), quanteda::docnames(dfm))

  for(i in 1:nrow(jsd)) {
    for(j in 1:ncol(jsd)) {
      jsd[i, j] <- philentropy::jensen_shannon(as.matrix(dfm[i, ]),
                                               as.matrix(dfm[j, ]),
                                               testNA = FALSE,
                                               unit = "log2")
    }
  }

  jsd
}

#' Join JSD matrix with model metadata
#'
#' Converts a Jensen–Shannon divergence matrix (e.g., from `jsd_simple()`) into a
#' tidy table and joins metadata for the `P` and `Q` weight IDs.
#'
#' @param jsd_matrix A square numeric matrix with dimnames corresponding to
#'   `weight_id` values.
#' @param info_predictive_model_weights A metadata table keyed by `weight_id`
#'   (typically `info_predictive_model_weights` from the targets pipeline).
#'
#' @return A tibble with divergence values and prefixed metadata columns for `P_`
#'   and `Q_`.
jsd_to_df <- function(jsd_matrix, info_predictive_model_weights) {
  jsd_df <- jsd_matrix |>
    as_tibble(rownames = "P_weight_id") |>
    tidyr::pivot_longer(-P_weight_id, names_to = "Q_weight_id")

  jsd_df |>
    left_join(info_predictive_model_weights |>
                rename_with(~paste0("P_", .x))) |>
    left_join(info_predictive_model_weights |>
                rename_with(~paste0("Q_", .x))) |>
    mutate(P_label = paste(P_workset_meta_id, P_sample_max_vols, P_predictive_model_engine,
                           P_predictive_model_dfm_weight, P_dfm_to_lower),
           Q_label = paste(Q_workset_meta_id, Q_sample_max_vols, Q_predictive_model_engine,
                           Q_predictive_model_dfm_weight, Q_dfm_to_lower)) |>
    select(-ends_with("object"))
}

#' Pairwise KL divergence for one (or two) weight IDs over time
#'
#' Filters a long weight table to one or two `weight_id` values, builds a DFM of
#' `(weight_id, period)` distributions, and computes all pairwise KL divergences
#' between those rows.
#'
#' @param weight_object A data frame/tibble with columns `weight_id`, `period`,
#'   `word`, and `normalized_value`.
#' @param P First `weight_id` to include.
#' @param Q Optional second `weight_id` to include. If omitted, uses `Q = P`.
#' @param epsilon Smoothing constant passed to
#'   `philentropy::kullback_leibler_distance()`.
#'
#' @return A numeric matrix of KL divergences with dimnames from the constructed
#'   DFM.
kl <- function(weight_object, P, Q, epsilon = 1e-10) {
  if(missing(Q)) {
    Q <- P
  }
  dfm <- weight_object |>
    filter(weight_id %in% c(P, Q)) |>
    mutate(doc_id = paste(weight_id, period)) |>
    tidytext::cast_dfm(doc_id, word, normalized_value) |>
    quanteda::dfm_weight("prop")

  kl <- matrix(0, nrow = nrow(dfm), ncol = nrow(dfm))
  dimnames(kl) <- list(quanteda::docnames(dfm), quanteda::docnames(dfm))


  for(i in 1:nrow(kl)) {
    for(j in 1:ncol(kl)) {
      kl[i, j] <- philentropy::kullback_leibler_distance(as.matrix(dfm[i, ]),
                                                         as.matrix(dfm[j, ]),
                                                         testNA = FALSE,
                                                         unit = "log2",
                                                         epsilon = epsilon)
    }
  }

  kl
}

#' Convert a KL matrix with `(weight_id, period)` docnames into measures
#'
#' Like `kl_matrix_to_df()`, but expects matrix dimnames in the form
#' `"<weight_id> <period>"`. Splits `P` and `Q` into `*_weight_id` and
#' `*_period` columns before deriving novelty/transience/resonance.
#'
#' @param kl_matrix A square numeric matrix with dimnames of the form
#'   `"<weight_id> <period>"`.
#'
#' @return A tidy tibble with novelty/transience/resonance in long form.
kl_matrix_to_df2 <- function(kl_matrix) {
  kl_df <- kl_matrix |>
    as_tibble(rownames = "P") |>
    tidyr::pivot_longer(-P, names_to = "Q") |>
    tidyr::separate_wider_delim(P:Q, " ",
                                names = c("weight_id","period"),
                                names_sep = "_") |>
    mutate(across(ends_with("period"), as.numeric))

  kl_novelty <- kl_df |>
    filter(P_period > Q_period) |>
    rename(novelty = value,
           past_period = Q_period)

  kl_transience <- kl_df |>
    filter(P_period < Q_period) |>
    rename(transience = value,
           future_period = Q_period)

  kl_df <- kl_novelty |>
    full_join(kl_transience, relationship = "many-to-many") |>
    mutate(resonance = novelty - transience) |>
    pivot_longer(c(novelty, transience, resonance), names_to = "measure")

  kl_df
}

#' Pairwise KL divergence across weight IDs using word-by-period features
#'
#' Constructs a distribution for each `weight_id` over `word_id = paste(word, period)`,
#' then computes pairwise KL divergences between weight IDs.
#'
#' @param weight_object A data frame/tibble with columns `weight_id`, `word`,
#'   `period`, and `normalized_value`.
#' @param epsilon Smoothing constant passed to
#'   `philentropy::kullback_leibler_distance()`.
#'
#' @return A numeric matrix of KL divergences between `weight_id` values.
kl_weights <- function(weight_object, epsilon = 1e-10) {
  dfm <- weight_object |>
    mutate(word_id = paste(word, period)) |>
    tidytext::cast_dfm(weight_id, word_id, normalized_value) |>
    quanteda::dfm_weight("prop")

  kl <- matrix(0, nrow = nrow(dfm), ncol = nrow(dfm))
  dimnames(kl) <- list(quanteda::docnames(dfm), quanteda::docnames(dfm))


  for(i in 1:nrow(kl)) {
    for(j in 1:ncol(kl)) {
      kl[i, j] <- philentropy::kullback_leibler_distance(as.matrix(dfm[i, ]),
                                                         as.matrix(dfm[j, ]),
                                                         testNA = FALSE,
                                                         unit = "log2",
                                                         epsilon = epsilon)
    }
  }

  kl
}

#' Pairwise Jensen–Shannon divergence across weight IDs using word-by-period features
#'
#' Like `kl_weights()`, but computes Jensen–Shannon divergence between the
#' distributions for each `weight_id`.
#'
#' @param weight_object A data frame/tibble with columns `weight_id`, `word`,
#'   `period`, and `normalized_value`.
#'
#' @return A numeric matrix of Jensen–Shannon divergences between `weight_id`
#'   values.
jsd_weights <- function(weight_object) {
  dfm <- weight_object |>
    mutate(word_id = paste(word, period)) |>
    tidytext::cast_dfm(weight_id, word_id, normalized_value) |>
    quanteda::dfm_weight("prop")

  jsd <- matrix(0, nrow = nrow(dfm), ncol = nrow(dfm))
  dimnames(jsd) <- list(quanteda::docnames(dfm), quanteda::docnames(dfm))


  for(i in 1:nrow(jsd)) {
    for(j in 1:ncol(jsd)) {
      jsd[i, j] <- philentropy::jensen_shannon(as.matrix(dfm[i, ]),
                                               as.matrix(dfm[j, ]),
                                               testNA = FALSE,
                                               unit = "log2")
    }
  }

  jsd
}

#' Join KL weights matrix with model metadata
#'
#' Converts a KL matrix (e.g., from `kl_weights()`) to long form and attaches
#' metadata for the `P` and `Q` weight IDs.
#'
#' @param kl_matrix A square numeric matrix with dimnames corresponding to
#'   `weight_id` values.
#' @param info_predictive_model_weights A metadata table keyed by `weight_id`.
#'
#' @return A tibble with KL values and prefixed metadata columns for `P_` and
#'   `Q_`.
kl_weights_to_df <- function(kl_matrix, info_predictive_model_weights) {
  kl_df <- kl_matrix |>
    as_tibble(rownames = "P_weight_id") |>
    tidyr::pivot_longer(-P_weight_id, names_to = "Q_weight_id") |>
    left_join(info_predictive_model_weights |>
                rename_with(~paste("P", .x, sep = "_"))) |>
    left_join(info_predictive_model_weights |>
                rename_with(~paste("Q", .x, sep = "_")))

  kl_df
}
