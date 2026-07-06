# Reference-based weighting must reproduce the self-referenced (quanteda /
# dfm_ppmi) results when the reference set is the DFM itself. This pins the
# refactor of dfm_ppmi() into dfm_ppmi_worker() and the new *_apply() helpers.

make_synthetic_dfm <- function() {
  toks <- quanteda::tokens(c(
    d1 = "apple banana banana cherry",
    d2 = "apple apple cherry date",
    d3 = "banana cherry cherry date date",
    d4 = "apple banana date date cherry"
  ))
  quanteda::dfm(toks)
}

test_that("dfm_tfidf_apply(x, x) equals quanteda::dfm_tfidf(x)", {
  skip_if_not_installed("quanteda")
  x <- make_synthetic_dfm()
  applied <- as.matrix(dfm_tfidf_apply(x, x))
  reference <- as.matrix(quanteda::dfm_tfidf(x))
  # Align feature order before comparing.
  applied <- applied[, colnames(reference), drop = FALSE]
  expect_equal(applied, reference, tolerance = 1e-8)
})

test_that("dfm_ppmi_apply(x, x) equals dfm_ppmi(x) exactly", {
  skip_if_not_installed("quanteda")
  x <- make_synthetic_dfm()
  applied <- as.matrix(dfm_ppmi_apply(x, x))
  reference <- as.matrix(dfm_ppmi(x))
  applied <- applied[, colnames(reference), drop = FALSE]
  expect_equal(applied, reference, tolerance = 1e-12)
})

test_that("dfm_ppmi_apply restricts output to the reference feature space", {
  skip_if_not_installed("quanteda")
  x <- make_synthetic_dfm()
  # Reference without the "date" feature: the output lives in the reference's
  # feature space, so "date" is dropped (downstream dfm_match to the model's
  # features restores zeros where needed).
  reference <- quanteda::dfm_remove(x, "date")
  applied <- dfm_ppmi_apply(x, reference)
  expect_false("date" %in% quanteda::featnames(applied))
  expect_setequal(quanteda::featnames(applied), quanteda::featnames(reference))
})
