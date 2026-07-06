#' Create train/test splits for a DFM
#'
#' S3 generic for creating an `rsample` split object for supervised learning.
#' Methods dispatch on the type of `feat` (e.g., `dictionary2` vs `character`).
#'
#' Some methods optionally downsample the majority class within the training set
#' to reduce class imbalance.
#'
#' @param dfm A `quanteda` `dfm` used for modeling.
#' @param feat Feature specification used for labeling/dispatch (e.g., a
#'   `quanteda` dictionary or a character feature name).
#' @param downsampled Logical; if `TRUE`, apply a downsampling strategy to the
#'   training set (method-dependent).
#' @param type Downsampling strategy (e.g., `"similarity"` or `"random"`), used
#'   when `downsampled` is `TRUE`.
#'
#' @return An `rsample` `rsplit` object.
train_test_splits <-function(dfm, feat, downsampled, type) {
  UseMethod("train_test_splits", feat)
}

#' Train/test split for dictionary features
#'
#' Creates a stratified `rsample::initial_split()` using the dictionary key as
#' the target column. If `downsampled` is `TRUE`, dispatches to
#' `train_test_splits_downsample()` for additional class balancing.
#'
#' @param dfm A `quanteda` `dfm` used for modeling.
#' @param feat A `quanteda` `dictionary2` defining the target feature.
#' @param downsampled Logical; if `TRUE`, apply a downsampling strategy to the
#'   training set.
#' @param type Downsampling strategy (e.g., `"similarity"` or `"random"`).
#'
#' @return An `rsample` `rsplit` object.
train_test_splits.dictionary2 <- function(dfm, feat, downsampled, type) {

  if(downsampled) {
    UseMethod("train_test_splits_downsample", feat)
  } else {
    df <- dfm %>%
      quanteda::dfm_lookup(feat) %>%
      quanteda::dfm_weight("boolean") %>%
      quanteda::convert(to = "data.frame")

    res <- rsample::initial_split(df, strata = names(feat))
    return(res)
  }

}

#' Train/test split for a character feature name
#'
#' Creates an `rsample::initial_split()` based on a boolean version of the
#' selected feature column. If `type` is `"similarity"` or `"random"`, applies a
#' downsampling step to the training set using `most_similar_downsample()` or
#' `random_downsample()`.
#'
#' @param dfm A `quanteda` `dfm` used for modeling.
#' @param feat Character scalar naming the target feature column in `dfm`.
#' @param downsampled Logical flag for downsampling (currently not used by this
#'   method; `type` controls whether downsampling occurs).
#' @param type Downsampling strategy: `"similarity"` or `"random"`.
#'
#' @return An `rsample` `rsplit` object.
train_test_splits.character <- function(dfm, feat, downsampled, type) {

  df <- dfm%>%
    quanteda::dfm_select(feat)%>%
    quanteda::dfm_weight(scheme = "boolean")%>%
    quanteda::convert(to = "data.frame")

  # `feat` is a character scalar naming the target column produced by `convert()`.
  split <- rsample::initial_split(df, strata = feat)

  if(type == "similarity") {
    split <- most_similar_downsample(df, dfm, split)
  } else if(type == "random") {
    split <- random_downsample(df, dfm, split)
  }

  split

}

#' Downsampled train/test split for dictionary features
#'
#' Creates a stratified `rsample::initial_split()` and then balances the
#' training set by downsampling the negative class. Downsampling can be random
#' (`random_downsample()`) or based on cosine similarity to the positive class
#' centroid (`most_similar_downsample()`).
#'
#' @param dfm A `quanteda` `dfm` used for modeling.
#' @param feat A `quanteda` `dictionary2` defining the target feature.
#' @param downsampled Logical flag (unused; present for signature compatibility).
#' @param type Downsampling strategy: `"similarity"` or `"random"`.
#'
#' @return An `rsample` `rsplit` object with an updated training index.
train_test_splits_downsample.dictionary2 <- function(dfm, feat, downsampled, type) {
  type <- match.arg(type, c("similarity", "random"))

  df <- dfm%>%
    quanteda::dfm_lookup(feat)%>%
    quanteda::dfm_weight(scheme = "boolean")%>%
    quanteda::convert(to = "data.frame")

  split <- rsample::initial_split(df, strata = names(feat))

  if(type == "similarity") {
    split <- most_similar_downsample(df, dfm, split)
  } else if(type == "random") {
    split <- random_downsample(df, dfm, split)
  }

  split
}

#' Downsample negatives by similarity to the positive class
#'
#' Restricts to the training set, computes the centroid of the positive class,
#' and selects the most similar negative documents (by cosine similarity) to
#' match the number of positives. Returns the split with `in_id` updated.
#'
#' @param df A data frame derived from the target feature DFM (must include
#'   `doc_id` and a boolean/0-1 target column).
#' @param dfm The full `quanteda` `dfm` of predictors used to compute similarities.
#' @param split An `rsample` `rsplit` object to modify.
#'
#' @return The updated `rsplit` with a balanced training set.
most_similar_downsample <- function(df, dfm, split) {

  dfm_training <- dfm[ split$in_id, ]
  df <- df[ split$in_id, ]

  dfm_true <- dfm_training%>%
    quanteda::dfm_subset(df[ ,2] == 1)

  dfm_false <- dfm_training%>%
    quanteda::dfm_subset(df[ ,2] == 0)

  dfm_true_vec <- dfm_true%>%
    Matrix::colMeans()%>%
    as.matrix(nrow=1)%>%
    Matrix::t()

  sims_df <- cosine_sims(dfm_false, dfm_true_vec)%>%
    quanteda::as.dfm()%>%
    quanteda::convert(to = "data.frame")%>%
    tibble::as_tibble()

  # The first column is `doc_id`; the second holds the cosine similarity scores.
  sims <- sims_df %>%
    dplyr::slice_max(.data[[names(sims_df)[2]]], n = nrow(dfm_true))

  ids_true <- quanteda::docnames(dfm_true)
  ids_false <- sims$doc_id

  all_ids <- c(ids_true, ids_false)

  in_id <- which(split$data$doc_id %in% all_ids)

  split$in_id <- in_id

  split

}

#' Randomly downsample negatives to match positives
#'
#' Restricts to the training set and randomly samples negative documents so the
#' number of negatives matches the number of positives. Returns the split with
#' `in_id` updated.
#'
#' @param df A data frame derived from the target feature DFM (must include
#'   `doc_id` and a boolean/0-1 target column).
#' @param dfm The full `quanteda` `dfm` used for indexing and sampling.
#' @param split An `rsample` `rsplit` object to modify.
#'
#' @return The updated `rsplit` with a balanced training set.
random_downsample <- function(df, dfm, split) {

  dfm_training <- dfm[ split$in_id, ]
  df <- df[ split$in_id, ]

  dfm_true <- dfm_training%>%
    quanteda::dfm_subset(df[ ,2] == 1)

  dfm_false <- dfm_training%>%
    quanteda::dfm_subset(df[ ,2] == 0)

  dfm_false <- dfm_false%>%
    quanteda::dfm_sample(size = min(nrow(dfm_true), nrow(dfm_false)))

  ids_true <- quanteda::docnames(dfm_true)
  ids_false <- quanteda::docnames(dfm_false)

  all_ids <- c(ids_true, ids_false)

  in_id <- which(split$data$doc_id %in% all_ids)

  split$in_id <- in_id

  split

}

#' Cosine similarity between rows of sparse matrices
#'
#' Computes cosine similarity between all rows of `x` and all rows of `y` using
#' matrix cross-products. Inputs can be sparse matrices such as `dfm` objects or
#' `Matrix` classes.
#'
#' @param x A matrix-like object where rows represent observations.
#' @param y A matrix-like object where rows represent observations.
#'
#' @return A numeric matrix of cosine similarities with `nrow(x)` rows and
#'   `nrow(y)` columns.
cosine_sims <- function(x, y) {
  Matrix::tcrossprod(x, y)/(sqrt(Matrix::tcrossprod(Matrix::rowSums(x^2), Matrix::rowSums(y^2))))
}

#' Fit a predictive model from a DFM
#'
#' Prepares predictors and a target feature column, subsets to the training set
#' from `initial_split`, and fits a model using the requested `engine`.
#'
#' This function removes the raw feature token(s) from the predictors, applies
#' optional weighting, and delegates to engine-specific helpers such as
#' `predictive_model.glmnet()` or `predictive_model.xgboost()`.
#'
#' @param dfm A `quanteda` `dfm` of document-feature counts.
#' @param initial_split An `rsample` `rsplit` object defining training/testing.
#' @param feat Target feature specification (typically a `quanteda` dictionary).
#' @param weight Feature weighting scheme (`"ppmi"`, `"tfidf"`, or `"none"`).
#' @param model_type Modeling task (`"regression"` or `"classification"`).
#' @param engine Modeling engine (`"LiblineaR"`, `"glmnet"`, `"xgboost"`, or
#'   `"naivebayes"`).
#' @param pattern Regular expression used to select predictor features after
#'   removing the target feature token(s).
#' @param params A named list of engine hyperparameters supplied from the YAML
#'   config (see `config/pipeline.yml` / `config/corpus.yml`,
#'   `predictive_models.engines`). Each engine merges these over its hard-coded
#'   defaults with [utils::modifyList()], so an empty list reproduces the
#'   historical defaults exactly. Recognized keys are engine-specific, e.g.
#'   `laplace` (naivebayes); `type`/`cost` (LiblineaR); `alpha`/`nfolds` and the
#'   glmnet-only pseudo-param `lambda_rule` (`"lambda.min"`/`"lambda.1se"`);
#'   `nrounds`/`eta`/`max_depth`/`colsample_bytree`/`lambda`/`subsample` and the
#'   accepted-but-unsupported `early_stopping_rounds` (xgboost).
#' @param ... Additional engine-specific arguments passed through.
#'
#' @return A fitted model object; class depends on the selected `engine`.
#'
#' @details `lambda_rule` is a glmnet-only pseudo-parameter: it is not forwarded
#'   to [glmnet::cv.glmnet()] but instead controls which lambda (`lambda.min` or
#'   `lambda.1se`) is used when reading coefficients/predictions downstream. To
#'   avoid threading it through extra parameter-grid columns and target
#'   signatures, the chosen rule is stored as an attribute on the fitted model
#'   object (`attr(model, "lambda_rule")`) at fit time; `model_weights.cv.glmnet()`
#'   and the `cv.glmnet` performance methods read it back from the model.
predictive_model <- function(dfm, initial_split, feat,
                             weight = c("ppmi", "tfidf", "none"),
                             model_type = c("regression", "classification"),
                             engine = c("LiblineaR", "glmnet", "xgboost"),
                             pattern = ".",
                             params = list(),
                             ...) {
  engine <- match.arg(engine, c("LiblineaR", "glmnet", "xgboost", "naivebayes"))
  weight <- match.arg(weight, c("ppmi", "tfidf", "none"))
  model_type <- match.arg(model_type, c("regression", "classification"))
  params <- if (is.null(params)) list() else as.list(params)
  dfm_feat <- dfm_select(dfm, feat)
  dfm <- compress_dfm_to_target_features(dfm, feat)
  dfm <- dfm %>%
    quanteda::dfm_remove(feat) %>%
    quanteda::dfm_select(pattern = pattern, valuetype = "regex") %>%
    quanteda:::cbind.dfm(dfm_feat)

  training_dfm <- get_training_sample(dfm, initial_split)

  switch(engine,
         LiblineaR = predictive_model.LiblineaR(training_dfm = training_dfm,
                                                feat = feat, weight = weight,
                                                model_type = model_type,
                                                params = params,
                                                ...),
         glmnet = predictive_model.glmnet(training_dfm = training_dfm,
                                          feat = feat, weight = weight,
                                          model_type = model_type,
                                          params = params,
                                          ...),
         xgboost = predictive_model.xgboost(training_dfm = training_dfm,
                                            feat = feat, weight = weight,
                                            model_type = model_type,
                                            params = params,
                                            ...),
         naivebayes = predictive_model.naivebayes(training_dfm = training_dfm,
                                                  feat = feat, weight = weight,
                                                  model_type = model_type,
                                                  params = params,
                                                  ...))

}

#' Fit a multinomial naive Bayes model
#'
#' Fits `naivebayes::multinomial_naive_bayes()` on the training data. Only
#' supports classification.
#'
#' @param training_dfm A training-only `quanteda` `dfm` (includes target column).
#' @param feat Target feature specification used by `get_x()`/`get_y()`.
#' @param weight Weighting scheme passed to `get_x()`.
#' @param model_type Modeling task; must be `"classification"` for this engine.
#' @param params Named list of hyperparameters merged over the defaults
#'   (`list(laplace = 1)`).
#' @param ... Unused; present for interface consistency.
#'
#' @return A fitted naive Bayes model object.
predictive_model.naivebayes <- function(training_dfm, feat, weight, model_type, params = list(), ...) {

  defaults <- list(laplace = 1)
  params <- utils::modifyList(defaults, params)

  x_train <- get_x(training_dfm, feat = feat, weight = weight)

  y_train <- get_y(training_dfm, feat, model_type = model_type)

  x_train <- as(x_train, "dgCMatrix")

  stopifnot(model_type == "classification")

  model <- naivebayes::multinomial_naive_bayes(x_train, y_train, laplace = params$laplace)

  model
}

#' Fit a LiblineaR model
#'
#' Fits linear models from `LiblineaR::LiblineaR()` for either regression or
#' classification. The `type` argument can be provided via `...` to select a
#' LiblineaR solver.
#'
#' @param training_dfm A training-only `quanteda` `dfm` (includes target column).
#' @param feat Target feature specification used by `get_x()`/`get_y()`.
#' @param weight Weighting scheme passed to `get_x()`.
#' @param model_type Modeling task (`"regression"` or `"classification"`).
#' @param params Named list of hyperparameters merged over the defaults. For
#'   classification the defaults are `list(type = 1, cost = <heuristic>)`; for
#'   regression `list(type = 11, cost = <heuristic>, svr_eps = 0.1)`. `cost`
#'   defaults to `1 / mean(sqrt(rowSums(x_train^2)))`.
#' @param ... Optional engine arguments (superseded by `params`).
#'
#' @return A fitted LiblineaR model object.
predictive_model.LiblineaR <- function(training_dfm, feat, weight, model_type, params = list(), ...) {

  x_train <- get_x(training_dfm, feat = feat, weight = weight)

  y_train <- get_y(training_dfm, feat, model_type = model_type)

  cost_default <- 1/mean(sqrt(Matrix::rowSums(x_train^2)))

  x_train <- as(x_train, "dgCMatrix") %>%
    as("RsparseMatrix") %>%
    as("dgRMatrix")

  if(model_type == "regression") {
    defaults <- list(type = 11, cost = cost_default, svr_eps = 0.1)
    params <- utils::modifyList(defaults, params)
    stopifnot(params$type %in% c(11:13))

    svm_model <- LiblineaR::LiblineaR(x_train, y_train, cost = params$cost,
                                      type = params$type, svr_eps = params$svr_eps)
  } else {
    defaults <- list(type = 1, cost = cost_default)
    params <- utils::modifyList(defaults, params)
    stopifnot(params$type %in% c(0:7))

    tab <- table(y_train)
    wi <- stats::setNames(as.numeric(sum(tab) / (length(tab) * tab)), names(tab))

    svm_model <- LiblineaR::LiblineaR(x_train, y_train, cost = params$cost,
                                      type = params$type, wi = wi)
  }

  svm_model

}

#' Fit an XGBoost model
#'
#' Fits an `xgboost::xgboost()` model for regression or binary classification.
#' Engine options can be passed via `...`, notably a `params` list and `nrounds`.
#'
#' For classification, this function sets several defaults (AUC metric, class
#' weighting, and tree hyperparameters) unless overridden by `params`.
#'
#' @param training_dfm A training-only `quanteda` `dfm` (includes target column).
#' @param feat Target feature specification used by `get_x()`/`get_y()`.
#' @param weight Weighting scheme passed to `get_x()`.
#' @param model_type Modeling task (`"regression"` or `"classification"`).
#' @param params Named list of hyperparameters merged over the engine defaults
#'   with [utils::modifyList()]. For classification the defaults are
#'   `objective = "binary:logistic"`, `scale_pos_weight` (class-imbalance ratio),
#'   `eval_metric = "auc"`, `eta = 0.1`, `max_depth = 12`,
#'   `colsample_bytree = 0.5`, `lambda = 0.5`, `subsample = 0.5`, `nrounds = 120`;
#'   for regression `objective = "reg:squarederror"`, `nrounds = 20`. `nrounds`
#'   may be supplied in `params`. `early_stopping_rounds` is accepted but not
#'   supported here (the fit has no held-out validation set) and is dropped with
#'   a warning; use a tuning study (`xgb.cv`) to choose `nrounds`.
#' @param ... Optional engine arguments (superseded by `params`).
#'
#' @return A fitted `xgboost` model.
predictive_model.xgboost <- function(training_dfm = training_dfm,
                                     feat = feat, weight = weight,
                                     model_type = model_type,
                                     params = list(),
                                     ...) {
  xgboost_nthread <- as.integer(Sys.getenv("XGBOOST_NTHREAD", "1"))
  if(is.na(xgboost_nthread) || xgboost_nthread < 1) {
    xgboost_nthread <- 1
  }

  x_train <- get_x(training_dfm, feat = feat, weight = weight)

  y_train <- get_y(training_dfm, feat, model_type = model_type)

  if(model_type == "classification") {
    y_train <- y_train %>%
      as.numeric()
    y_train <- y_train - 1
  }

  print(table(y_train))

  params <- if (is.null(params)) list() else as.list(params)

  # `nrounds` and `early_stopping_rounds` are handled outside the xgboost
  # `params` list (they are arguments to xgboost(), not booster parameters).
  nrounds_override <- params[["nrounds"]]
  params[["nrounds"]] <- NULL
  early_stopping_rounds <- params[["early_stopping_rounds"]]
  params[["early_stopping_rounds"]] <- NULL

  if(model_type == "regression") {
    defaults <- list(objective = "reg:squarederror")
    nrounds_default <- 20
  } else {
    defaults <- list(
      objective = "binary:logistic",
      scale_pos_weight = sum(y_train == 0) / sum(y_train == 1),
      eval_metric = "auc",
      eta = 0.1,
      max_depth = 12,
      colsample_bytree = 0.5,
      lambda = 0.5,
      subsample = 0.5
    )
    nrounds_default <- 120
  }

  params <- utils::modifyList(defaults, params)

  if(model_type == "regression") {
    stopifnot(stringr::str_detect(params[["objective"]], "reg:"))
  } else {
    stopifnot(stringr::str_detect(params[["objective"]], "binary:"))
  }

  nrounds <- if (is.null(nrounds_override)) nrounds_default else nrounds_override
  params[["nthread"]] <- xgboost_nthread

  if(!is.null(early_stopping_rounds)) {
    warning("`early_stopping_rounds` is not supported by predictive_model.xgboost(): ",
            "the fit has no held-out validation set. Ignoring it; use a tuning ",
            "study (xgb.cv) to choose `nrounds` instead.", call. = FALSE)
  }

  # Use the low-level xgb.train()/xgb.DMatrix() interface: as of xgboost 3.x the
  # high-level xgboost(data=, label=, params=) interface is deprecated and
  # *silently drops* the `params` list, so hyperparameters (max_depth, eta, ...)
  # would never reach the booster. xgb.train() is the stable interface that
  # honors a `params` list.
  dtrain <- xgboost::xgb.DMatrix(data = x_train, label = y_train, nthread = xgboost_nthread)
  xgboost_model <- xgboost::xgb.train(params = params, data = dtrain, nrounds = nrounds,
                                      verbose = 0)

  attr(xgboost_model, "demagogues_objective") <- params[["objective"]]
  xgboost_model
}



#' Fit a glmnet model with cross-validation
#'
#' Fits `glmnet::cv.glmnet()` on the training data, using a Gaussian family for
#' regression and binomial for classification. Runs sequentially by default
#' inside targets workers; set `GLMNET_CORES` greater than 1 to opt into a local
#' PSOCK cluster for cross-validation.
#'
#' @param training_dfm A training-only `quanteda` `dfm` (includes target column).
#' @param feat Target feature specification used by `get_x()`/`get_y()`.
#' @param weight Weighting scheme passed to `get_x()`.
#' @param model_type Modeling task (`"regression"` or `"classification"`).
#' @param params Named list of hyperparameters forwarded to
#'   [glmnet::cv.glmnet()] (e.g. `alpha`, `nfolds`; defaults are cv.glmnet's own
#'   `alpha = 1`, `nfolds = 10`). The glmnet-only pseudo-param `lambda_rule`
#'   (`"lambda.min"` or `"lambda.1se"`, default `"lambda.min"`) is *not* passed to
#'   cv.glmnet; instead it is recorded as `attr(model, "lambda_rule")` and read
#'   back downstream (see [predictive_model()] details).
#' @param ... Unused; present for interface consistency (use `params` to pass
#'   cv.glmnet arguments).
#'
#' @return A fitted `cv.glmnet` object, carrying a `"lambda_rule"` attribute.
predictive_model.glmnet <- function(training_dfm, feat, weight, model_type, params = list(), ...) {

  glmnet_cores <- as.integer(Sys.getenv("GLMNET_CORES", "1"))
  if(is.na(glmnet_cores) || glmnet_cores < 1) {
    glmnet_cores <- 1
  }

  use_parallel <- glmnet_cores > 1
  if(use_parallel) {
    cl <- parallel::makeCluster(glmnet_cores)
    doParallel::registerDoParallel(cl)
    on.exit({
      try(parallel::stopCluster(cl), silent = TRUE)
      doParallel::stopImplicitCluster()
    }, add = TRUE)
  } else if(requireNamespace("foreach", quietly = TRUE)) {
    foreach::registerDoSEQ()
  }

  params <- if (is.null(params)) list() else as.list(params)

  # `lambda_rule` is a pseudo-parameter: it selects which lambda is used when
  # reading coefficients/predictions downstream, not a cv.glmnet() argument.
  lambda_rule <- params[["lambda_rule"]]
  params[["lambda_rule"]] <- NULL
  if(is.null(lambda_rule)) {
    lambda_rule <- "lambda.min"
  }
  lambda_rule <- match.arg(lambda_rule, c("lambda.min", "lambda.1se"))

  x_train <- get_x(training_dfm, feat = feat, weight = weight)

  y_train <- get_y(training_dfm, feat, model_type = model_type)

  family <- if(model_type == "regression") "gaussian" else "binomial"

  glmnet_model <- do.call(
    glmnet::cv.glmnet,
    c(list(x = x_train, y = y_train, family = family, parallel = use_parallel), params)
  )

  attr(glmnet_model, "lambda_rule") <- lambda_rule

  glmnet_model

}

#' Prepare predictors from a DFM (S3 generic)
#'
#' S3 generic that removes the target feature column(s) and optionally applies
#' weighting (e.g., PPMI or TF-IDF) to produce the predictor matrix `x`.
#'
#' @param dfm A `quanteda` `dfm` of document-feature counts (includes target column).
#' @param feature Target feature specification used for dispatch.
#' @param weight Weighting scheme (`"ppmi"`, `"tfidf"`, or `"none"`).
#'
#' @return A `quanteda` `dfm` (or matrix-like object) of predictors.
get_x <- function(dfm, feature, weight = c("ppmi", "tfidf", "none")) {
  UseMethod("get_x", feature)
}

#' Prepare predictors for dictionary targets
#'
#' Removes all feature tokens in the dictionary from the predictor matrix and
#' applies the requested weighting scheme.
#'
#' @param dfm A `quanteda` `dfm` of document-feature counts.
#' @param feature A `quanteda` `dictionary2` defining the target feature tokens.
#' @param weight Weighting scheme (`"ppmi"`, `"tfidf"`, or `"none"`).
#'
#' @return A `quanteda` `dfm` of predictors.
get_x.dictionary2 <- function(dfm, feature, weight = c("ppmi", "tfidf", "none")) {
  weight <- match.arg(weight, c("ppmi", "tfidf", "none"))
  dfm <- dfm %>%
    quanteda::dfm_remove(as.list(feature) %>%
                           unlist())

  if(weight == "ppmi") {
    return(dfm %>%
             dfm_ppmi())
  } else if(weight == "tfidf") {
    return(dfm %>%
             quanteda::dfm_tfidf())
  } else {
    return(dfm)
  }

}

#' Prepare predictors for character targets
#'
#' Removes the feature column named by `feature` and applies the requested
#' weighting scheme.
#'
#' @param dfm A `quanteda` `dfm` of document-feature counts.
#' @param feature Character scalar naming the target feature column.
#' @param weight Weighting scheme (`"ppmi"`, `"tfidf"`, or `"none"`).
#'
#' @return A `quanteda` `dfm` of predictors.
get_x.character <- function(dfm, feature, weight = c("ppmi", "tfidf", "none")) {
  weight <- match.arg(weight, c("ppmi", "tfidf", "none"))
  dfm <- dfm %>%
    quanteda::dfm_remove(feature)

  if(weight == "ppmi") {
    return(dfm %>%
             dfm_ppmi())
  } else if(weight == "tfidf") {
    return(dfm %>%
             quanteda::dfm_tfidf())
  } else {
    return(dfm)
  }

}

#' Extract response values from a DFM (S3 generic)
#'
#' S3 generic that extracts a response vector `y` from a DFM based on `feature`.
#' Returns a numeric vector for regression and a two-level factor for binary
#' classification.
#'
#' @param dfm A `quanteda` `dfm` containing the target feature column(s).
#' @param feature Target feature specification used for dispatch.
#' @param model_type Modeling task (`"regression"` or `"classification"`).
#'
#' @return A numeric vector (regression) or factor with levels `FALSE`, `TRUE`
#'   (classification).
get_y <- function(dfm, feature, model_type = c("regression", "classification")) {
  UseMethod("get_y", feature)
}

#' Extract response for dictionary targets
#'
#' Looks up the dictionary in the DFM and returns the resulting feature column
#' as a numeric vector (regression) or boolean factor (classification).
#'
#' @param dfm A `quanteda` `dfm` containing the target feature column(s).
#' @param feature A `quanteda` `dictionary2` defining the target feature.
#' @param model_type Modeling task (`"regression"` or `"classification"`).
#'
#' @return A numeric vector or factor, depending on `model_type`.
get_y.dictionary2 <- function(dfm, feature, model_type = c("regression", "classification")) {
  model_type <- match.arg(model_type, c("regression", "classification"))
  dfm <- dfm %>%
    quanteda::dfm_lookup(feature)

  if(model_type == "regression") {
    return(dfm %>%
             as.vector())
  } else if(model_type == "classification") {
    return(dfm %>%
             as.vector() %>%
             as.logical() %>%
             factor(levels = c(FALSE, TRUE)))
  }

}

#' Extract response for character targets
#'
#' Selects the single feature column named by `feature` and returns it as a
#' numeric vector (regression) or boolean factor (classification).
#'
#' @param dfm A `quanteda` `dfm` containing the target feature column.
#' @param feature Character scalar naming the target feature column.
#' @param model_type Modeling task (`"regression"` or `"classification"`).
#'
#' @return A numeric vector or factor, depending on `model_type`.
get_y.character <- function(dfm, feature, model_type = c("regression", "classification")) {
  model_type <- match.arg(model_type, c("regression", "classification"))
  stopifnot(length(feature) == 1)
  dfm <- dfm %>%
    quanteda::dfm_select(feature)

  if(model_type == "regression") {
    return(dfm %>%
             as.vector())
  } else if(model_type == "classification") {
    return(dfm %>%
             as.vector() %>%
             as.logical() %>%
             factor(levels = c(FALSE, TRUE)))
  }

}

#' Subset a DFM to the training partition
#'
#' Uses an `rsample` split object to select documents belonging to the training
#' set. Assumes `rsample::training(initial_split)` contains a `doc_id` column
#' that matches `quanteda::docnames(dfm)`.
#'
#' @param dfm A `quanteda` `dfm` with document names matching the split data.
#' @param initial_split An `rsample` `rsplit` object.
#'
#' @return A `quanteda` `dfm` containing only training documents.
get_training_sample <- function(dfm, initial_split) {
  training <- rsample::training(initial_split)
  dfm[ quanteda::docnames(dfm) %in% training$doc_id, ]
}

#' Subset a DFM to the testing partition
#'
#' Uses an `rsample` split object to select documents belonging to the testing
#' set. Assumes `rsample::testing(initial_split)` contains a `doc_id` column
#' that matches `quanteda::docnames(dfm)`.
#'
#' @param dfm A `quanteda` `dfm` with document names matching the split data.
#' @param initial_split An `rsample` `rsplit` object.
#'
#' @return A `quanteda` `dfm` containing only testing documents.
get_test_sample <- function(dfm, initial_split) {
  testing <- rsample::testing(initial_split)
  dfm[ quanteda::docnames(dfm) %in% testing$doc_id, ]
}
