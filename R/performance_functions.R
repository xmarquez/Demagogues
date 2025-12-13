
#' Evaluate model performance aggregated by volume (S3 generic)
#'
#' Computes model performance metrics per volume (`htid`) by scoring each page
#' (document) in a DFM and then summarizing metrics within each `htid` group.
#' Methods dispatch on the class of `model` (e.g., `cv.glmnet`, `LiblineaR`,
#' `xgb.Booster`).
#'
#' @param model A fitted model object.
#' @param dfm A `quanteda` `dfm` containing documents to score. Must have
#'   `docvars(dfm)$htid` for volume aggregation.
#' @param feat Target feature specification used by `get_x()`/`get_y()` (often a
#'   `quanteda` dictionary).
#' @param weight Feature weighting scheme used when constructing predictors
#'   (`"ppmi"`, `"tfidf"`, or `"none"`).
#'
#' @return A tibble of `yardstick` metrics (often grouped by `htid`), with an
#'   added `model_type` column describing the engine/task.
model_performance_per_volume <- function(model, dfm, feat, weight = c("ppmi", "tfidf", "none")) {
  invisible()
  UseMethod("model_performance_per_volume")
}

#' Extract feature names from an xgboost model
#'
#' Attempts to retrieve the feature names from several locations used by xgboost
#' objects (`$feature_names`, `attr(model, "feature_names")`, and
#' `model$params$feature_names`).
#'
#' @param model An `xgboost` model object.
#'
#' @return A character vector of feature names, or `NULL` if unavailable.
get_xgb_feature_names <- function(model) {
  feature_names <- model$feature_names
  if(is.null(feature_names)) {
    feature_names <- attr(model, "feature_names")
  }
  if(is.null(feature_names) && !is.null(model$params) && !is.null(model$params$feature_names)) {
    feature_names <- model$params$feature_names
  }
  feature_names
}

#' Per-volume performance for `cv.glmnet` models
#'
#' Scores all documents in `dfm`, matches features to the fitted glmnet model,
#' and computes regression metrics or binary classification metrics grouped by
#' `htid`.
#'
#' @param model A fitted `glmnet::cv.glmnet()` model.
#' @param dfm A `quanteda` `dfm` with `docvars(dfm)$htid` present.
#' @param feat Target feature specification used by `get_x()`/`get_y()`.
#' @param weight Feature weighting scheme (`"ppmi"`, `"tfidf"`, or `"none"`).
#'
#' @return A tibble of metrics by `htid` with a `model_type` label.
model_performance_per_volume.cv.glmnet <- function(model, dfm, feat, weight = c("ppmi", "tfidf", "none")) {

  weight <- match.arg(weight, c("ppmi", "tfidf", "none"))
  if("lognet" %in% class(model$glmnet.fit)) {
    model_type <- "classification"

  } else if("elnet" %in% class(model$glmnet.fit)) {
    model_type <- "regression"
  }

  x_test <- get_x(dfm, feat = feat, weight = weight)

  x_test <- quanteda::dfm_match(x_test, model$glmnet.fit$beta@Dimnames[[1]])

  y_test <- get_y(dfm, feat, model_type = model_type)

  predictions_estimate <- glmnet:::predict.cv.glmnet(model, newx = x_test, s = "lambda.min", type = "response") %>%
    as.vector()

  predictions_class <- glmnet:::predict.cv.glmnet(model, newx = x_test, s = "lambda.min", type = "class") %>%
    factor(levels = c("FALSE", "TRUE"))

  if(model_type == "regression") {
    preds <- tibble::tibble(truth = y_test, estimate = predictions_estimate) %>%
      dplyr::bind_cols(quanteda::docvars(dfm)) %>%
      dplyr::group_by(htid)

    res <- preds  %>%
      yardstick::metrics(truth = truth, estimate = estimate)

  }

  if(model_type == "classification") {
    predictions_class <- glmnet:::predict.cv.glmnet(model, newx = x_test, s = "lambda.min", type = "class") %>%
      factor(levels = c("FALSE", "TRUE"))

    preds <- tibble::tibble(truth = y_test, estimate = predictions_estimate, class = predictions_class)  %>%
      dplyr::bind_cols(quanteda::docvars(dfm)) %>%
      dplyr::group_by(htid)

    res <- binary_metrics(preds)
  }

  res  %>%
    dplyr::mutate(model_type = paste("cv.glmnet", model_type))

}

#' Per-volume performance for `LiblineaR` models
#'
#' Scores all documents in `dfm` using a fitted `LiblineaR` model, computes
#' regression metrics or (for classification) also attaches a confusion matrix.
#' Metrics are computed within `htid` groups.
#'
#' @param model A fitted `LiblineaR::LiblineaR()` model.
#' @param dfm A `quanteda` `dfm` with `docvars(dfm)$htid` present.
#' @param feat Target feature specification used by `get_x()`/`get_y()`.
#' @param weight Feature weighting scheme (`"ppmi"`, `"tfidf"`, or `"none"`).
#'
#' @return A tibble of metrics by `htid` with a `model_type` label.
model_performance_per_volume.LiblineaR <- function(model, dfm, feat, weight = c("ppmi", "tfidf", "none")) {

  weight <- match.arg(weight, c("ppmi", "tfidf", "none"))
  model_type <- ifelse(model$Type %in% c(0:7), "classification",
                       "regression")

  x_test <- quanteda::dfm_match(get_x(dfm, feat = feat, weight = weight),
                                colnames(model$W)[colnames(model$W) != "Bias"]) %>%
    as("dgCMatrix") %>%
    as("RsparseMatrix") %>%
    as("dgRMatrix")

  y_test <- get_y(dfm, feat, model_type = model_type)

  predictions <- LiblineaR:::predict.LiblineaR(model, newx = x_test)

  preds <- tibble::tibble(truth = y_test, estimate = predictions$predictions)  %>%
    dplyr::bind_cols(quanteda::docvars(dfm)) %>%
    dplyr::group_by(htid)

  res <- preds %>%
    yardstick::metrics(truth = truth, estimate = estimate)

  if(model_type == "classification") {
    conf_mat <- preds %>%
      yardstick::conf_mat(truth = truth, estimate = estimate)

    res <- res %>%
      dplyr::mutate(conf_mat = list(conf_mat))
  }

  res  %>%
    dplyr::mutate(model_type = model$TypeDetail)
}

#' Per-volume performance for xgboost boosters
#'
#' Scores all documents in `dfm` using a fitted `xgb.Booster` model and computes
#' regression metrics or binary classification metrics grouped by `htid`.
#'
#' @param model A fitted `xgboost::xgb.Booster` model.
#' @param dfm A `quanteda` `dfm` with `docvars(dfm)$htid` present.
#' @param feat Target feature specification used by `get_x()`/`get_y()`.
#' @param weight Feature weighting scheme (`"ppmi"`, `"tfidf"`, or `"none"`).
#'
#' @return A tibble of metrics by `htid` with a `model_type` label.
model_performance_per_volume.xgb.Booster <-  function(model, dfm, feat,
                                                      weight = c("ppmi", "tfidf", "none")) {

  weight <- match.arg(weight, c("ppmi", "tfidf", "none"))

  model_type <- ifelse(stringr::str_detect(model$params$objective, "reg:"), "regression",
                       "classification")

  x_test <- get_x(dfm, feat = feat, weight = weight)
  feature_names <- get_xgb_feature_names(model)
  if(is.null(feature_names)) {
    feature_names <- quanteda::featnames(x_test)
  }
  x_test <- quanteda::dfm_match(x_test, feature_names)

  y_test <- get_y(dfm, feat, model_type = model_type)

  if(model_type == "classification") {
    y_test <- y_test %>%
      as.numeric()
    y_test <- y_test - 1
  }

  predictions <- xgboost:::predict.xgb.Booster(model, newdata = x_test)

  preds <- tibble::tibble(truth = y_test, estimate = predictions)  %>%
    dplyr::bind_cols(quanteda::docvars(dfm)) %>%
    dplyr::group_by(htid)

  if(model_type == "regression") {
    res <- preds %>%
      yardstick::metrics(truth = truth, estimate = estimate)

  }

  if(model_type == "classification") {

    res <- preds %>%
      dplyr::mutate(truth = factor(truth, levels = c(0, 1)),
                    class = factor(ifelse(estimate < 0.5, 0, 1), levels = c(0, 1))) %>%
      binary_metrics()
  }

  res %>%
    dplyr::mutate(model_type = paste("xgboost gradient boosted trees", model$params$objective))

}

#' Evaluate model performance on a split (S3 generic)
#'
#' Scores either the testing split, the training split, or an out-of-distribution
#' ("testing - OOD") dataset and returns performance metrics. Methods dispatch on
#' the class of `model`.
#'
#' @param model A fitted model object.
#' @param dfm A `quanteda` `dfm` containing both predictors and target feature
#'   column(s).
#' @param initial_split An `rsample` split object used to select training/testing
#'   partitions.
#' @param feat Target feature specification used by `get_x()`/`get_y()`.
#' @param weight Feature weighting scheme (`"ppmi"`, `"tfidf"`, or `"none"`).
#' @param use Which data to evaluate: `"testing"`, `"training"`, or `"testing - OOD"`.
#'
#' @return A tibble of `yardstick` metrics (and sometimes a confusion matrix), with
#'   an added `model_type` column.
model_performance <- function(model, dfm, initial_split, feat, weight, use = "testing") {
  UseMethod("model_performance")
}

#' Performance for multinomial naive Bayes models
#'
#' Evaluates a fitted `naivebayes::multinomial_naive_bayes()` model on the chosen
#' split and returns binary classification metrics.
#'
#' @param model A fitted multinomial naive Bayes model.
#' @param dfm A `quanteda` `dfm` containing both predictors and the target feature.
#' @param initial_split An `rsample` split object.
#' @param feat Target feature specification used by `get_x()`/`get_y()`.
#' @param weight Feature weighting scheme (`"ppmi"`, `"tfidf"`, or `"none"`).
#' @param use Which data to evaluate: `"testing"`, `"training"`, or `"testing - OOD"`.
#'
#' @return A tibble of binary classification metrics with a `model_type` label.
model_performance.multinomial_naive_bayes <- function(model, dfm, initial_split, feat,
                                                      weight = c("ppmi", "tfidf", "none"),
                                                      use = "testing") {
  weight <- match.arg(weight, c("ppmi", "tfidf", "none"))
  use <- match.arg(use, c("testing", "training", "testing - OOD"))
  dfm <- compress_dfm_to_target_features(dfm, feat)
  if(use == "testing") {
    testing_dfm <- get_test_sample(dfm, initial_split)
  } else if(use == "training") {
    testing_dfm <- get_training_sample(dfm, initial_split)
  } else {
    testing_dfm <- dfm
  }

  x_test <- get_x(testing_dfm, feat = feat, weight = weight) %>%
    as("dgCMatrix")

  y_test <- get_y(testing_dfm, feat, model_type = "classification")

  predictions <- naivebayes:::predict.multinomial_naive_bayes(model, x_test, "class")
  predictions_probs <- naivebayes:::predict.multinomial_naive_bayes(model, x_test, "prob")

  preds <- tibble::tibble(truth = y_test, class = predictions) |>
    bind_cols(predictions_probs)

  res <- preds %>%
    dplyr::mutate(estimate = (`TRUE`-`FALSE` + 1)/2) %>%
    binary_metrics()

  res %>%
    dplyr::mutate(model_type = paste("naive bayes classifier", weight, "weights"))
}

#' Performance for `LiblineaR` models
#'
#' Evaluates a fitted `LiblineaR::LiblineaR()` model on the chosen split and
#' returns regression or classification metrics (plus a confusion matrix for
#' classification).
#'
#' @param model A fitted `LiblineaR` model.
#' @param dfm A `quanteda` `dfm` containing both predictors and the target feature.
#' @param initial_split An `rsample` split object.
#' @param feat Target feature specification used by `get_x()`/`get_y()`.
#' @param weight Feature weighting scheme (`"ppmi"`, `"tfidf"`, or `"none"`).
#' @param use Which data to evaluate: `"testing"`, `"training"`, or `"testing - OOD"`.
#'
#' @return A tibble of metrics with a `model_type` label (and `conf_mat` for classification).
model_performance.LiblineaR <- function(model, dfm, initial_split, feat,
                                        weight = c("ppmi", "tfidf", "none"),
                                        use = "testing") {
  weight <- match.arg(weight, c("ppmi", "tfidf", "none"))
  use <- match.arg(use, c("testing", "training", "testing - OOD"))
  if(use == "testing") {
    testing_dfm <- get_test_sample(dfm, initial_split)
  } else if(use == "training") {
    testing_dfm <- get_training_sample(dfm, initial_split)
  } else {
    testing_dfm <- dfm
  }
  model_type <- ifelse(model$Type %in% c(0:7), "classification",
                       "regression")

  x_test <- get_x(testing_dfm, feat = feat, weight = weight) %>%
    as("dgCMatrix") %>%
    as("RsparseMatrix") %>%
    as("dgRMatrix")

  y_test <- get_y(testing_dfm, feat, model_type = model_type)

  predictions <- LiblineaR:::predict.LiblineaR(model, newx = x_test)

  preds <- tibble::tibble(truth = y_test, estimate = predictions$predictions)

  res <- preds %>%
    yardstick::metrics(truth = truth, estimate = estimate)

  if(model_type == "classification") {
    conf_mat <- preds %>%
      yardstick::conf_mat(truth = truth, estimate = estimate)

    res <- res %>%
      dplyr::mutate(conf_mat = list(conf_mat))
  }

  res  %>%
    dplyr::mutate(model_type = model$TypeDetail)

}

#' Performance for xgboost boosters
#'
#' Evaluates a fitted `xgboost::xgb.Booster` model on the chosen split and returns
#' regression metrics or binary classification metrics.
#'
#' @param model A fitted `xgboost` booster.
#' @param dfm A `quanteda` `dfm` containing both predictors and the target feature.
#' @param initial_split An `rsample` split object.
#' @param feat Target feature specification used by `get_x()`/`get_y()`.
#' @param weight Feature weighting scheme (`"ppmi"`, `"tfidf"`, or `"none"`).
#' @param use Which data to evaluate: `"testing"`, `"training"`, or `"testing - OOD"`.
#'
#' @return A tibble of metrics with a `model_type` label.
model_performance.xgb.Booster <-  function(model, dfm, initial_split, feat,
                                           weight = c("ppmi", "tfidf", "none"),
                                           use = "testing") {
  
  weight <- match.arg(weight, c("ppmi", "tfidf", "none"))
  use <- match.arg(use, c("testing", "training", "testing - OOD"))
  if(use == "testing") {
    testing_dfm <- get_test_sample(dfm, initial_split)
  } else if(use == "training") {
    testing_dfm <- get_training_sample(dfm, initial_split)
  } else {
    testing_dfm <- dfm
  }
  model_type <- ifelse(stringr::str_detect(attr(model, "params")$objective, "reg:"), "regression",
                       "classification")

  x_test <- get_x(testing_dfm, feat = feat, weight = weight)

  feature_names <- xgboost::getinfo(model, "feature_name")

  x_test <- quanteda::dfm_match(x_test, feature_names)

  y_test <- get_y(testing_dfm, feat, model_type = model_type)

  if(model_type == "classification") {
    y_test <- y_test %>%
      as.numeric()
    y_test <- y_test - 1
  }

  predictions <- xgboost:::predict.xgb.Booster(model, newdata = x_test)

  preds <- tibble::tibble(truth = y_test, estimate = predictions)

  if(model_type == "regression") {
    res <- preds %>%
      yardstick::metrics(truth = truth, estimate = estimate)

  }

  if(model_type == "classification") {

    res <- preds %>%
      dplyr::mutate(truth = factor(truth, levels = c(0, 1)),
                    class = factor(ifelse(estimate < 0.5, 0, 1), levels = c(0, 1))) %>%
      binary_metrics()
  }

  res %>%
    dplyr::mutate(model_type = paste("xgboost gradient boosted trees", model$params$objective))

}


#' Performance for `cv.glmnet` models
#'
#' Evaluates a fitted `glmnet::cv.glmnet()` model on the chosen split and returns
#' regression metrics or binary classification metrics.
#'
#' @param model A fitted `glmnet` cross-validated model.
#' @param dfm A `quanteda` `dfm` containing both predictors and the target feature.
#' @param initial_split An `rsample` split object.
#' @param feat Target feature specification used by `get_x()`/`get_y()`.
#' @param weight Feature weighting scheme (`"ppmi"`, `"tfidf"`, or `"none"`).
#' @param use Which data to evaluate: `"testing"`, `"training"`, or `"testing - OOD"`.
#'
#' @return A tibble of metrics with a `model_type` label.
model_performance.cv.glmnet <- function(model, dfm, initial_split, feat,
                                        weight = c("ppmi", "tfidf", "none"),
                                        use = "testing") {
  weight <- match.arg(weight, c("ppmi", "tfidf", "none"))
  use <- match.arg(use, c("testing", "training", "testing - OOD"))
  if(use == "testing") {
    testing_dfm <- get_test_sample(dfm, initial_split)
  } else if(use == "training") {
    testing_dfm <- get_training_sample(dfm, initial_split)
  } else {
    testing_dfm <- dfm
  }
  if("lognet" %in% class(model$glmnet.fit)) {
    model_type <- "classification"

  } else if("elnet" %in% class(model$glmnet.fit)) {
    model_type <- "regression"
  }

  x_test <- get_x(testing_dfm, feat = feat, weight = weight) %>%
    quanteda::dfm_match(model$glmnet.fit$beta@Dimnames[[1]])

  y_test <- get_y(testing_dfm, feat, model_type = model_type)

  predictions_estimate <- glmnet:::predict.cv.glmnet(model, newx = x_test, s = "lambda.min", type = "response") %>%
    as.vector()

  predictions_class <- glmnet:::predict.cv.glmnet(model, newx = x_test, s = "lambda.min", type = "class") %>%
    factor(levels = c("FALSE", "TRUE"))

  preds <- tibble::tibble(truth = y_test, estimate = predictions_estimate, class = predictions_class)

  if(model_type == "regression") {
    preds <- tibble::tibble(truth = y_test, estimate = predictions_estimate)
    res <- preds %>%
      yardstick::metrics(truth = truth, estimate = estimate)

  }

  if(model_type == "classification") {
    predictions_class <- glmnet:::predict.cv.glmnet(model, newx = x_test, s = "lambda.min", type = "class") %>%
      factor(levels = c("FALSE", "TRUE"))

    preds <- tibble::tibble(truth = y_test, estimate = predictions_estimate, class = predictions_class)
    res <- binary_metrics(preds)
  }

  res  %>%
    dplyr::mutate(model_type = paste("cv.glmnet", model_type))

}

#' Binary classification metrics helper
#'
#' Computes a standard set of `yardstick` classification metrics from a data
#' frame of predictions, and attaches a confusion matrix as a list-column.
#'
#' Expected columns:
#' - `truth`: factor with the true class labels
#' - `class`: factor with predicted class labels
#' - `estimate`: numeric score/probability used for ROC AUC
#'
#' @param preds A data frame/tibble of predictions with `truth`, `class`, and
#'   `estimate` columns.
#'
#' @return A tibble of metrics with an added `conf_mat` list-column.
binary_metrics <- function(preds) {
  res <- preds %>%
    yardstick::metrics(truth = truth, estimate = class,
                       estimate) %>%
    dplyr::filter(.metric != "roc_auc") %>%
    dplyr::bind_rows(yardstick::roc_auc(preds,
                                        estimate, truth = truth,
                                        event_level = "second"))

  conf_mat <- preds %>%
    yardstick::conf_mat(truth = truth, estimate = class)

  res <- res %>%
    dplyr::mutate(conf_mat = list(conf_mat))

  res
}
