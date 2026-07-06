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
#' @param method Normalization scheme passed to [normalize_weights()]:
#'   `"positive"` (default), `"shift"`, or `"softmax"`.
#'
#' @return A tibble of feature weights/importance.
model_weights <- function(model, method = c("positive", "shift", "softmax")) {
  UseMethod("model_weights")
}

#' Normalize signed model weights into a probability-like vector
#'
#' Rescales a numeric vector of (possibly signed) model weights or importances
#' into a non-negative vector that sums to 1, so it can be treated as a
#' probability distribution over features for entropy/KL/JSD calculations.
#'
#' Three schemes are supported:
#' - `"positive"` (default): keep only positive evidence, `pmax(value, 0)`
#'   rescaled to sum to 1. Negative weights (evidence *against* the target)
#'   receive zero mass.
#' - `"shift"`: shift by the minimum, `(value - min(value))`, rescaled to sum to
#'   1. The most negative weight is mapped to 0.
#' - `"softmax"`: a numerically stable softmax, `exp(value / temperature)`
#'   rescaled to sum to 1. Order-preserving and dense.
#'
#' Degenerate cases are guarded so the function never returns `NaN`: if the
#' chosen scheme yields a zero or non-finite denominator (e.g. all values are
#' `<= 0` under `"positive"`), it falls back to `"shift"`; if every value is
#' identical, a uniform distribution is returned.
#'
#' @param value A numeric vector of model weights or importances.
#' @param method Normalization scheme: `"positive"`, `"shift"`, or `"softmax"`.
#' @param temperature Positive scalar temperature for `method = "softmax"`;
#'   ignored otherwise.
#'
#' @return A numeric vector the same length as `value`, non-negative and summing
#'   to 1.
normalize_weights <- function(value, method = c("positive", "shift", "softmax"), temperature = 1) {
  method <- match.arg(method)
  n <- length(value)
  if(n == 0L) {
    return(numeric(0))
  }

  # Uniform distribution when all values are identical (also covers n == 1).
  if(all(value == value[1])) {
    return(rep(1 / n, n))
  }

  normalized <- switch(
    method,
    positive = {
      pos <- pmax(value, 0)
      denom <- sum(pos)
      if(!is.finite(denom) || denom == 0) NULL else pos / denom
    },
    shift = {
      shifted <- value - min(value)
      denom <- sum(shifted)
      if(!is.finite(denom) || denom == 0) NULL else shifted / denom
    },
    softmax = {
      z <- value / temperature
      ex <- exp(z - max(z))
      denom <- sum(ex)
      if(!is.finite(denom) || denom == 0) NULL else ex / denom
    }
  )

  # Fall back to "shift" if the requested scheme produced a degenerate result.
  if(is.null(normalized)) {
    shifted <- value - min(value)
    denom <- sum(shifted)
    if(!is.finite(denom) || denom == 0) {
      return(rep(1 / n, n))
    }
    normalized <- shifted / denom
  }

  normalized
}

#' Model weights for LiblineaR models
#'
#' Extracts linear coefficients from a fitted `LiblineaR` model, orients the
#' sign so that positive values correspond to the `TRUE` class when class names
#' are available, and returns a normalized weight table.
#'
#' @param model A fitted `LiblineaR::LiblineaR()` model.
#' @param method Normalization scheme passed to [normalize_weights()].
#'
#' @return A tibble of coefficients with normalization columns.
model_weights.LiblineaR <- function(model, method = c("positive", "shift", "softmax")) {
  method <- match.arg(method)

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
                  normalized_value = normalize_weights(value, method = method))

}

#' Model weights for multinomial naive Bayes models
#'
#' Extracts log-probability coefficients from a fitted
#' `naivebayes::multinomial_naive_bayes()` model and computes a single signed
#' weight as the difference between the `TRUE` and `FALSE` class coefficients.
#'
#' @param model A fitted `naivebayes::multinomial_naive_bayes` model.
#' @param method Normalization scheme passed to [normalize_weights()].
#'
#' @return A tibble of per-feature weights with normalization columns.
model_weights.multinomial_naive_bayes <- function(model, method = c("positive", "shift", "softmax")) {
  method <- match.arg(method)

  ret <- naivebayes:::coef.multinomial_naive_bayes(model) %>%
    dplyr::as_tibble(rownames = "word") %>%
    dplyr::mutate(value = `TRUE` - `FALSE`)

  ret %>%
    dplyr::arrange(desc(value)) %>%
    dplyr::mutate(model_type = class(model),
                  scaled_value = as.numeric(scale(value)),
                  normalized_value = normalize_weights(value, method = method))

}

#' Model weights for xgboost boosters
#'
#' Uses `xgboost::xgb.importance()` to extract feature importance values and
#' normalizes them for downstream comparison.
#'
#' @param model A fitted `xgboost::xgb.Booster` model.
#' @param method Normalization scheme passed to [normalize_weights()].
#'
#' @return A tibble of feature importances with normalization columns.
model_weights.xgb.Booster <- function(model, method = c("positive", "shift", "softmax")) {
  method <- match.arg(method)

  ret <- tibble::as_tibble(xgboost::xgb.importance(model = model))
  colnames(ret) <- c("word", "value", "cover", "frequency")

  ret <- ret %>%
    dplyr::mutate(scaled_value = as.numeric(scale(value)),
                  normalized_value = normalize_weights(value, method = method))

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
#' @param method Normalization scheme passed to [normalize_weights()].
#'
#' @return A tibble of coefficients with normalization columns.
model_weights.cv.glmnet <- function(model, method = c("positive", "shift", "softmax")) {
  method <- match.arg(method)

  glmnet:::predict.cv.glmnet(model, s = "lambda.min", type = "coef") %>%
    Matrix::rowMeans() %>%
    tibble::enframe() %>%
    dplyr::rename(word = name) %>%
    dplyr::filter(word != "(Intercept)") %>%
    dplyr::arrange(-value) %>%
    dplyr::mutate(model_type = paste(class(model), class(model$glmnet.fit), collapse = " "),
                  scaled_value = as.numeric(scale(value)),
                  normalized_value = normalize_weights(value, method = method))
}
