# Regression tests for the performance-evaluation fixes surfaced by the first
# full democracy run (2026-07-07): LiblineaR eval matrices must be matched to
# the model's training feature space (predict.LiblineaR errors on any missing
# training feature), naive Bayes eval must stay sparse (dense as.matrix() on
# large OOD DFMs allocated tens of GB), and a NULL model must not break dispatch.
suppressWarnings(suppressMessages({
  library(quanteda)
  # Source every R/ helper so cross-file dependencies (e.g.
  # compress_dfm_to_target_features) resolve, mirroring the pipeline's
  # tar_source().
  for (f in list.files(here::here("R"), pattern = "\\.R$", full.names = TRUE)) source(f)
}))

# A small classification DFM with the feature token present on positive docs,
# plus an rsample split, mirroring the pipeline's shapes.
make_perf_fixture <- function(n = 120) {
  set.seed(42)
  docs <- character(n)
  for (i in seq_len(n)) {
    pos <- i <= n / 2
    words <- sample(c("alpha", "beta", "gamma", "delta", "epsilon", "zeta"),
                    size = sample(4:8, 1), replace = TRUE,
                    prob = if (pos) c(5, 3, 1, 1, 1, 1) else c(1, 1, 3, 4, 2, 2))
    if (pos) words <- c(words, "democracy_nn", "democracy_nn")
    docs[i] <- paste(words, collapse = " ")
  }
  names(docs) <- paste0("d", seq_len(n))
  d <- quanteda::dfm(quanteda::tokens(docs))
  feat <- quanteda::dictionary(list(democracy = c("democracy_nn")))
  # Build the split the way the pipeline does (stratified, with a doc_id column
  # matching docnames) so get_training_sample()/get_test_sample() line up.
  split <- train_test_splits(d, feat, downsampled = FALSE, type = "random")
  list(dfm = d, split = split, feat = feat)
}

test_that("LiblineaR performance evaluates without a train/test column mismatch", {
  skip_if_not_installed("LiblineaR")
  skip_if_not_installed("rsample")
  fx <- make_perf_fixture()
  model <- predictive_model(fx$dfm, fx$split, feat = fx$feat, weight = "none",
                            model_type = "classification", engine = "LiblineaR")
  # Before the fix this threw "columns of 'test' and 'train' differ".
  res <- model_performance(model, fx$dfm, fx$split, feat = fx$feat,
                           weight = "none", use = "testing")
  expect_s3_class(res, "tbl_df")
  expect_true("accuracy" %in% res$.metric)
})

test_that("LiblineaR OOD evaluation matches features against a reference DFM", {
  skip_if_not_installed("LiblineaR")
  skip_if_not_installed("rsample")
  fx <- make_perf_fixture()
  model <- predictive_model(fx$dfm, fx$split, feat = fx$feat, weight = "ppmi",
                            model_type = "classification", engine = "LiblineaR")
  ref <- get_training_sample(fx$dfm, fx$split)
  # A wider "OOD" dfm with extra features the model never saw must not break the
  # match (predict.LiblineaR drops extras; the fix guarantees all train features
  # are present).
  ood <- quanteda::dfm(quanteda::tokens(c(d1 = "alpha beta democracy_nn extraword otherword",
                                          d2 = "gamma delta zeta noise")))
  res <- model_performance(model, ood, fx$split, feat = fx$feat, weight = "ppmi",
                           use = "testing - OOD", reference_dfm = ref)
  expect_s3_class(res, "tbl_df")
})

test_that("naive Bayes performance keeps the eval matrix sparse", {
  skip_if_not_installed("naivebayes")
  skip_if_not_installed("rsample")
  fx <- make_perf_fixture()
  model <- predictive_model(fx$dfm, fx$split, feat = fx$feat, weight = "none",
                            model_type = "classification", engine = "naivebayes")
  res <- model_performance(model, fx$dfm, fx$split, feat = fx$feat,
                           weight = "none", use = "testing")
  expect_s3_class(res, "tbl_df")
  expect_true(nrow(res) > 0)
})

test_that("model_performance on a NULL model returns an empty tibble, not an error", {
  res <- model_performance(NULL, dfm = NULL, initial_split = NULL, feat = NULL,
                           weight = "none")
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 0L)
})
