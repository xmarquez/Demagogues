# rank_agreement_by_period(): Spearman rank correlation over common vocabulary
# and top-n overlap, with a hand-computable 2-model x 2-period example.

test_that("rank_agreement_by_period computes Spearman and top-n overlap", {
  skip_if_not_installed("corrr")

  # Period 1: model A ranks w1<w2<w3, model B ranks w3<w2<w1 -> Spearman = -1.
  #           top-2 A = {w3, w2}, top-2 B = {w1, w2} -> overlap = 1/2.
  # Period 2: both models rank w1<w2<w3 -> Spearman = 1; top-2 overlap = 1.
  # weight_ids must start with "weight" (the function selects those columns).
  weights <- tibble::tribble(
    ~weight_id, ~period, ~word, ~normalized_value,
    "weight_A", 1, "w1", 0.1,
    "weight_A", 1, "w2", 0.2,
    "weight_A", 1, "w3", 0.7,
    "weight_B", 1, "w1", 0.5,
    "weight_B", 1, "w2", 0.3,
    "weight_B", 1, "w3", 0.2,
    "weight_A", 2, "w1", 0.1,
    "weight_A", 2, "w2", 0.5,
    "weight_A", 2, "w3", 0.9,
    "weight_B", 2, "w1", 0.2,
    "weight_B", 2, "w2", 0.4,
    "weight_B", 2, "w3", 0.8
  )

  info <- tibble::tibble(
    weight_id = c("weight_A", "weight_B"),
    predictive_model_engine = c("glmnet", "xgboost"),
    workset_meta_id = c("ws", "ws")
  )

  res <- suppressWarnings(
    rank_agreement_by_period(weights, info, top_n = 2)
  )

  expect_true(all(c("period", "term", "term2", "spearman", "top_overlap",
                    "engine", "engine2", "workset_meta_id", "workset_meta_id2") %in%
                    names(res)))

  # No self-pairs (diagonal correlations are NA and filtered out).
  expect_false(any(res$term == res$term2))

  p1 <- res[res$period == 1 & res$term == "weight_A" & res$term2 == "weight_B", ]
  expect_equal(p1$spearman, -1, tolerance = 1e-8)
  expect_equal(p1$top_overlap, 0.5, tolerance = 1e-8)

  p2 <- res[res$period == 2 & res$term == "weight_A" & res$term2 == "weight_B", ]
  expect_equal(p2$spearman, 1, tolerance = 1e-8)
  expect_equal(p2$top_overlap, 1, tolerance = 1e-8)

  # Metadata joined correctly.
  expect_equal(p1$engine, "glmnet")
  expect_equal(p1$engine2, "xgboost")
})

test_that("canonical_weight_id coalesces family-specific id columns", {
  # info_all_model_weights has NO `weight_id` column; the id lives in one of
  # weight_pred_id / weight_svd_id / weight_ppmi_id per row (the others NA).
  info <- tibble::tibble(
    weight_pred_id = c("weight_pred_A", NA,             NA),
    weight_svd_id  = c(NA,              "weight_svd_C", NA),
    weight_ppmi_id = c(NA,              "weight_ppmi_B", "weight_ppmi_B"),
    predictive_model_engine = c("glmnet", "SVD word vectors", "featureppmi")
  )
  # predictive > svd > ppmi priority: row 2 is an SVD model (built on PPMI, so
  # weight_ppmi_id is also set) and must resolve to its svd id, not the ppmi one.
  expect_equal(canonical_weight_id(info),
               c("weight_pred_A", "weight_svd_C", "weight_ppmi_B"))
  # Falls back to a direct weight_id column when present (synthetic fixtures).
  expect_equal(canonical_weight_id(tibble::tibble(weight_id = c("x", "y"))),
               c("x", "y"))
})

test_that("rank_agreement_by_period labels engines from family-specific id columns", {
  skip_if_not_installed("corrr")

  weights <- tibble::tribble(
    ~weight_id,   ~period, ~word, ~normalized_value,
    "weight_p_A", 1, "w1", 0.1, "weight_p_A", 1, "w2", 0.2, "weight_p_A", 1, "w3", 0.7,
    "weight_s_B", 1, "w1", 0.5, "weight_s_B", 1, "w2", 0.3, "weight_s_B", 1, "w3", 0.2
  )
  # Real-shape info: no `weight_id` column, ids split across family columns.
  info <- tibble::tibble(
    weight_pred_id = c("weight_p_A", NA),
    weight_svd_id  = c(NA,           "weight_s_B"),
    weight_ppmi_id = c(NA,           NA),
    predictive_model_engine = c("glmnet", "SVD word vectors"),
    workset_meta_id = c("ws", "ws")
  )

  res <- suppressWarnings(rank_agreement_by_period(weights, info, top_n = 2))
  # Before the fix these were all NA (match against a non-existent info$weight_id).
  expect_false(any(is.na(res$engine)))
  expect_setequal(unique(c(res$engine, res$engine2)),
                  c("glmnet", "SVD word vectors"))
})
