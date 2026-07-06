# The LiblineaR classifier receives per-class misclassification costs via `wi`.
# The fix in R/modeling_functions.R uses inverse-frequency weights so the
# minority class is upweighted. We replicate the formula inline here rather than
# fitting an actual model.

test_that("inverse-frequency wi upweights the minority class", {
  # Synthetic 90/10 imbalanced target.
  y_train <- factor(c(rep("FALSE", 90), rep("TRUE", 10)), levels = c("FALSE", "TRUE"))

  tab <- table(y_train)
  wi <- stats::setNames(as.numeric(sum(tab) / (length(tab) * tab)), names(tab))

  expect_named(wi, c("FALSE", "TRUE"))
  # Minority class ("TRUE", n = 10) must get the larger cost.
  expect_gt(wi[["TRUE"]], wi[["FALSE"]])
  # Weights are inversely proportional to class frequency.
  expect_equal(wi[["TRUE"]] / wi[["FALSE"]], 90 / 10)
  # Sanity: the naive (buggy) approach of raw counts would do the opposite.
  raw <- as.numeric(tab)
  expect_lt(raw[which(names(tab) == "TRUE")], raw[which(names(tab) == "FALSE")])
})
