#' Extract feature weights/importance from fitted models (S3 generic)
#'
#' S3 generic for extracting per-feature weights (linear models) or feature
#' importance (tree-based models) from a fitted model object.
#'
#' Methods return a standardized tibble with at least:
#' - `word`: feature name
#' - `value`: raw weight/importance
#' - `scaled_value`: z-scored `value`
#' - `normalized_value`: non-negative `value` rescaled to sum to 1
#'
#' @param model A fitted model object.
#'
#' @return A tibble of feature weights/importance.
model_weights <- function(model) {
  recalculate = FALSE
  UseMethod("model_weights")
}

#' Model weights for LiblineaR models
#'
#' Extracts linear coefficients from a fitted `LiblineaR` model, orients the
#' sign so that positive values correspond to the `TRUE` class when class names
#' are available, and returns a normalized weight table.
#'
#' @param model A fitted `LiblineaR::LiblineaR()` model.
#'
#' @return A tibble of coefficients with normalization columns.
model_weights.LiblineaR <- function(model) {

  ret <- tibble::tibble(colnames(model$W), model$W[1, ])
  colnames(ret) <- c("word", "value")

  if(!is.null(model$ClassNames)) {
    if(!as.logical(model$ClassNames[1])) {
      ret$value <- -ret$value
    }
  }

  ret %>%
    dplyr::filter(word != "Bias") %>%
    dplyr::arrange(desc(value)) %>%
    dplyr::mutate(model_type = model$TypeDetail,
                  scaled_value = as.numeric(scale(value)),
                  normalized_value = value - min(value) + abs(min(value)),
                  normalized_value = normalized_value/sum(normalized_value))

}

#' Model weights for multinomial naive Bayes models
#'
#' Extracts log-probability coefficients from a fitted
#' `naivebayes::multinomial_naive_bayes()` model and computes a single signed
#' weight as the difference between the `TRUE` and `FALSE` class coefficients.
#'
#' @param model A fitted `naivebayes::multinomial_naive_bayes` model.
#'
#' @return A tibble of per-feature weights with normalization columns.
model_weights.multinomial_naive_bayes <- function(model) {

  ret <- naivebayes:::coef.multinomial_naive_bayes(model) %>%
    dplyr::as_tibble(rownames = "word") %>%
    dplyr::mutate(value = `TRUE` - `FALSE`)

  ret %>%
    dplyr::arrange(desc(value)) %>%
    dplyr::mutate(model_type = class(model),
                  scaled_value = as.numeric(scale(value)),
                  normalized_value = value - min(value) + abs(min(value)),
                  normalized_value = normalized_value/sum(normalized_value))

}

#' Model weights for xgboost boosters
#'
#' Uses `xgboost::xgb.importance()` to extract feature importance values and
#' normalizes them for downstream comparison.
#'
#' @param model A fitted `xgboost::xgb.Booster` model.
#'
#' @return A tibble of feature importances with normalization columns.
model_weights.xgb.Booster <- function(model) {

  ret <- tibble::as_tibble(xgboost::xgb.importance(model = model))
  colnames(ret) <- c("word", "value", "cover", "frequency")

  ret <- ret %>%
    dplyr::mutate(scaled_value = as.numeric(scale(value)),
                  normalized_value = value - min(value) + abs(min(value)),
                  normalized_value = normalized_value/sum(normalized_value))

  ret %>%
    dplyr::mutate(model_type = paste("xgboost gradient boosted trees", model$params$objective))

}

#' Model weights for cv.glmnet models
#'
#' Extracts coefficients from a fitted `glmnet::cv.glmnet()` model at
#' `lambda.min`, averages across classes if applicable, and returns a normalized
#' weight table (excluding the intercept).
#'
#' @param model A fitted `glmnet` cross-validated model.
#'
#' @return A tibble of coefficients with normalization columns.
model_weights.cv.glmnet <- function(model) {

  glmnet:::predict.cv.glmnet(model, s = "lambda.min", type = "coef") %>%
    Matrix::rowMeans() %>%
    tibble::enframe() %>%
    dplyr::arrange(-value) %>%
    dplyr::mutate(model_type = paste(class(model), class(model$glmnet.fit), collapse = " "),
                  scaled_value = as.numeric(scale(value)),
                  normalized_value = value - min(value) + abs(min(value)),
                  normalized_value = normalized_value/sum(normalized_value)) %>%
    dplyr::rename(word = name) %>%
    dplyr::filter(word != "(Intercept)")
}
