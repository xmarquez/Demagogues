test_that("each method returns a distribution that sums to 1", {
  set.seed(1)
  value <- rnorm(20)
  for (method in c("positive", "shift", "softmax")) {
    normalized <- normalize_weights(value, method = method)
    expect_length(normalized, length(value))
    expect_equal(sum(normalized), 1)
    expect_true(all(normalized >= 0))
    expect_false(any(is.nan(normalized)))
  }
})

test_that("'positive' zeroes out negative weights", {
  value <- c(-2, -1, 0, 1, 3)
  normalized <- normalize_weights(value, method = "positive")
  expect_true(all(normalized[value <= 0] == 0))
  expect_true(all(normalized[value > 0] > 0))
  expect_equal(sum(normalized), 1)
})

test_that("'shift' maps the minimum weight to 0", {
  value <- c(-2, 0.5, 4)
  normalized <- normalize_weights(value, method = "shift")
  expect_equal(normalized[which.min(value)], 0)
  expect_equal(sum(normalized), 1)
})

test_that("no NaN on degenerate inputs across all methods", {
  degenerate <- list(
    all_negative = c(-3, -2, -1),
    all_zero = c(0, 0, 0, 0),
    all_equal = rep(5, 6),
    single = 42
  )
  for (method in c("positive", "shift", "softmax")) {
    for (nm in names(degenerate)) {
      normalized <- normalize_weights(degenerate[[nm]], method = method)
      expect_false(any(is.nan(normalized)),
                   info = paste(method, nm))
      expect_equal(sum(normalized), 1, info = paste(method, nm))
      expect_true(all(normalized >= 0), info = paste(method, nm))
    }
  }
})

test_that("all-equal and single-element inputs yield a uniform distribution", {
  expect_equal(normalize_weights(rep(5, 4), method = "positive"), rep(0.25, 4))
  expect_equal(normalize_weights(42, method = "positive"), 1)
})

test_that("'positive' on all-negative input falls back rather than returning NaN", {
  normalized <- normalize_weights(c(-3, -2, -1), method = "positive")
  expect_false(any(is.nan(normalized)))
  expect_equal(sum(normalized), 1)
  # Fallback is "shift": most negative maps to 0.
  expect_equal(normalized[1], 0)
})
