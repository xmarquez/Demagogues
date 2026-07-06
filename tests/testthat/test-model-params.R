# Config-driven predictive-model hyperparameters (Phase 3, P1).
#
# These tests cover: (1) scalar vs mapping engine-spec parse equivalence and
# wholesale profile override; (2) the params hash entering `predictive_model_id`
# only when params are non-empty (and being order-invariant); (3) engine methods
# merging user params over their hard-coded defaults via modifyList() for glmnet
# and xgboost; and (4) the glmnet `lambda_rule` attribute being respected by
# model_weights.cv.glmnet().
#
# object_parameters.R defines normalize_engines()/normalize_engines_for_merge()/
# predictive_model_params_id() (building the parameter grids from the default
# config as a side effect); modeling_functions.R defines the engine fits.
suppressWarnings(suppressMessages({
  library(dplyr)
  library(quanteda)
  # object_parameters.R reads the YAML config via paths relative to the project
  # root, so source it with the working directory set there (testthat runs from
  # tests/testthat). modeling_functions.R only defines functions.
  withr::with_dir(here::here(), source(here::here("object_parameters.R")))
  source(here::here("R", "modeling_functions.R"))
}))

# --- Engine spec normalization ----------------------------------------------

test_that("scalar and mapping engine forms parse equivalently for empty params", {
  scalar <- normalize_engines(c("glmnet", "naivebayes"))
  mapping <- normalize_engines(list(
    list(name = "glmnet", params = list()),
    list(name = "naivebayes")
  ))
  expect_equal(purrr::map_chr(scalar, "name"), c("glmnet", "naivebayes"))
  expect_equal(scalar, mapping)
  expect_equal(purrr::map(scalar, "params"), list(list(), list()))
})

test_that("mapping form carries per-engine params", {
  eng <- normalize_engines(list(
    list(name = "glmnet", params = list(alpha = 0.5, nfolds = 10)),
    list(name = "xgboost", params = list(max_depth = 6))
  ))
  expect_equal(purrr::map_chr(eng, "name"), c("glmnet", "xgboost"))
  expect_equal(eng[[1]]$params, list(alpha = 0.5, nfolds = 10))
  expect_equal(eng[[2]]$params$max_depth, 6)
})

test_that("a profile override replaces the base engine list wholesale", {
  base <- c("naivebayes", "glmnet", "LiblineaR", "xgboost")
  merged <- normalize_engines_for_merge(base, c("glmnet", "naivebayes"))
  expect_equal(purrr::map_chr(merged, "name"), c("glmnet", "naivebayes"))
  # No profile override -> base engines are used.
  merged_none <- normalize_engines_for_merge(base, NULL)
  expect_equal(purrr::map_chr(merged_none, "name"), base)
})

# --- Params hash in the target id -------------------------------------------

test_that("params hash is empty for default params and non-empty otherwise", {
  expect_identical(predictive_model_params_id(list()), "")
  h <- predictive_model_params_id(list(max_depth = 6, eta = 0.1))
  expect_equal(nchar(h), 8L)
  expect_true(nzchar(h))
})

test_that("params hash is invariant to YAML key order but sensitive to values", {
  h1 <- predictive_model_params_id(list(max_depth = 6, eta = 0.1))
  h2 <- predictive_model_params_id(list(eta = 0.1, max_depth = 6))
  expect_identical(h1, h2)
  expect_false(identical(h1, predictive_model_params_id(list(max_depth = 8, eta = 0.1))))
})

# --- Engine params merged over defaults --------------------------------------

make_model_dfm <- function(n = 80) {
  set.seed(1)
  docs <- character(n)
  for (i in seq_len(n)) {
    pos <- i <= n / 2
    words <- sample(
      c("alpha", "beta", "gamma", "delta", "epsilon"),
      size = sample(3:6, 1), replace = TRUE,
      prob = if (pos) c(4, 3, 1, 1, 1) else c(1, 1, 3, 4, 2)
    )
    if (pos) words <- c(words, "democracy_nn", "democracy_nn")
    docs[i] <- paste(words, collapse = " ")
  }
  names(docs) <- paste0("d", seq_len(n))
  quanteda::dfm(quanteda::tokens(docs))
}

xgb_max_depth <- function(model) {
  xgboost::xgb.config(model)$learner$gradient_booster$tree_train_param$max_depth
}

test_that("xgboost merges user params over defaults (empty params keeps defaults)", {
  skip_if_not_installed("xgboost")
  train <- make_model_dfm()
  feat <- quanteda::dictionary(list(democracy = c("democracy_nn")))

  m_default <- predictive_model.xgboost(train, feat, weight = "none",
                                        model_type = "classification", params = list())
  # Empty params must reproduce the hard-coded default max_depth = 12.
  expect_equal(xgb_max_depth(m_default), "12")

  m_override <- predictive_model.xgboost(train, feat, weight = "none",
                                         model_type = "classification",
                                         params = list(max_depth = 4, nrounds = 6))
  expect_equal(xgb_max_depth(m_override), "4")
})

test_that("xgboost accepts but drops early_stopping_rounds with a warning", {
  skip_if_not_installed("xgboost")
  train <- make_model_dfm()
  feat <- quanteda::dictionary(list(democracy = c("democracy_nn")))
  expect_warning(
    predictive_model.xgboost(train, feat, weight = "none", model_type = "classification",
                             params = list(early_stopping_rounds = 10, nrounds = 5)),
    "early_stopping_rounds"
  )
})

test_that("glmnet strips lambda_rule from params and records it as an attribute", {
  skip_if_not_installed("glmnet")
  train <- make_model_dfm()
  feat <- quanteda::dictionary(list(democracy = c("democracy_nn")))

  m_default <- predictive_model.glmnet(train, feat, weight = "none",
                                       model_type = "classification", params = list())
  expect_identical(attr(m_default, "lambda_rule"), "lambda.min")

  # lambda_rule is a pseudo-param: it must not be forwarded to cv.glmnet (which
  # would error) yet must be captured on the model. alpha is a real cv.glmnet arg.
  m_1se <- predictive_model.glmnet(train, feat, weight = "none", model_type = "classification",
                                   params = list(lambda_rule = "lambda.1se", alpha = 0.5))
  expect_identical(attr(m_1se, "lambda_rule"), "lambda.1se")
})

# --- lambda_rule respected downstream ----------------------------------------

test_that("model_weights.cv.glmnet reads coefficients at the model's lambda_rule", {
  skip_if_not_installed("glmnet")
  train <- make_model_dfm()
  feat <- quanteda::dictionary(list(democracy = c("democracy_nn")))
  model <- predictive_model.glmnet(train, feat, weight = "none",
                                   model_type = "classification", params = list())

  # Force the two rules and confirm the extracted coefficients track the lambda
  # actually stored on the object (lambda.1se is more strongly regularized, so it
  # has at least as many exactly-zero coefficients as lambda.min).
  w_min <- model_weights.cv.glmnet(`attr<-`(model, "lambda_rule", "lambda.min"))
  w_1se <- model_weights.cv.glmnet(`attr<-`(model, "lambda_rule", "lambda.1se"))

  ref_min <- as.numeric(glmnet:::predict.cv.glmnet(model, s = "lambda.min", type = "coef"))
  ref_1se <- as.numeric(glmnet:::predict.cv.glmnet(model, s = "lambda.1se", type = "coef"))
  # Drop the intercept (row 1) to match model_weights()'s output.
  expect_equal(sort(w_min$value), sort(ref_min[-1]))
  expect_equal(sort(w_1se$value), sort(ref_1se[-1]))
})
