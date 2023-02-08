train_test_splits <-function(dfm, feat) {
  UseMethod("train_test_splits", feat)
}

train_test_splits.dictionary2 <- function(dfm, feat) {
  df <- dfm %>%
    quanteda::dfm_lookup(feat) %>%
    quanteda::dfm_weight("boolean") %>%
    quanteda::convert(to = "data.frame")

  rsample::initial_split(df, strata = names(feat))
}

train_test_splits.character <- function(dfm, feat) {
  df <- dfm %>%
    quanteda::dfm_select(feat) %>%
    quanteda::dfm_weight("boolean") %>%
    quanteda::convert(to = "data.frame")

  rsample::initial_split(df, strata = feat)
}

predictive_model <- function(dfm, initial_split, feat,
                             weight = c("ppmi", "tfidf", "none"),
                             model_type = c("regression", "classification"),
                             engine = c("LiblineaR", "glmnet", "xgboost"), ...) {

  engine <- match.arg(engine, c("LiblineaR", "glmnet", "xgboost"))
  weight <- match.arg(weight, c("ppmi", "tfidf", "none"))
  model_type <- match.arg(model_type, c("regression", "classification"))
  training_dfm <- get_training_sample(dfm, initial_split)

  switch(engine,
         LiblineaR = predictive_model.LiblineaR(training_dfm = training_dfm,
                                                feat = feat, weight = weight,
                                                model_type = model_type,
                                                ...),
         glmnet = predictive_model.glmnet(training_dfm = training_dfm,
                                          feat = feat, weight = weight,
                                          model_type = model_type,
                                          ...),
         xgboost = predictive_model.xgboost(training_dfm = training_dfm,
                                            feat = feat, weight = weight,
                                            model_type = model_type,
                                            ...))

}

predictive_model.LiblineaR <- function(training_dfm, feat, weight, model_type, ...) {

  x_train <- get_x(training_dfm, feat = feat, weight = weight)

  y_train <- get_y(training_dfm, feat, model_type = model_type)

  cost <- 1/mean(sqrt(Matrix::rowSums(x_train^2)))

  x_train <- as(x_train, "dgCMatrix") %>%
    as("RsparseMatrix") %>%
    as("dgRMatrix")

  if(model_type == "regression") {
    dots <- list(...)
    if("type" %in% names(dots)) {
      type <- dots[["type"]]
      stopifnot(type %in% c(11:13))
    } else {
      type <- 11
    }
    svm_model <- LiblineaR::LiblineaR(x_train, y_train, cost = cost,
                                      type = type, svr_eps = 0.1)
  } else {
    dots <- list(...)
    if("type" %in% names(dots)) {
      type <- dots[["type"]]
      stopifnot(type %in% c(0:7))
    } else {
      type <- 1
    }
    svm_model <- LiblineaR::LiblineaR(x_train, y_train, cost = cost,
                                      type = type)
  }

  svm_model

}

predictive_model.xgboost <- function(training_dfm = training_dfm,
                                     feat = feat, weight = weight,
                                     model_type = model_type,
                                     ...) {

  x_train <- get_x(training_dfm, feat = feat, weight = weight)

  y_train <- get_y(training_dfm, feat, model_type = model_type)

  if(model_type == "classification") {
    y_train <- y_train %>%
      as.numeric()
    y_train <- y_train - 1
  }

  print(table(y_train))

  if(model_type == "regression") {
    dots <- list(...)
    if("params" %in% names(dots)) {
      params <- dots[["params"]]
      stopifnot(is.list(params))
      if("objective" %in% names(params)) {
        stopifnot(stringr::str_detect(params[["objective"]], "reg:"))
      } else {
        params <- c(params, objective = "reg:squarederror")
      }
    } else {
      params <- list(objective = "reg:squarederror")
    }
    if("nrounds" %in% names(dots)) {
      nrounds <- dots[["nrounds"]]
    } else {
      nrounds <- 20
    }
    xgboost_model <- xgboost::xgboost(data = x_train, label = y_train,
                                      params = params, nrounds = nrounds)
  } else {
    dots <- list(...)
    if("params" %in% names(dots)) {
      params <- dots[["params"]]
      stopifnot(is.list(params))
      if("objective" %in% names(params)) {
        stopifnot(stringr::str_detect(params[["objective"]], "binary:"))
      } else {
        params <- c(params, objective = "binary:logistic")
      }
    } else {
      params <- list(objective = "binary:logistic")
    }
    if("nrounds" %in% names(dots)) {
      nrounds <- dots[["nrounds"]]
    } else {
      nrounds <- 20
    }
    xgboost_model <- xgboost::xgboost(data = x_train, label = y_train,
                                      params = params, nrounds = nrounds)
  }


  xgboost_model
}


model_weights <- function(model) {
  UseMethod("model_weights")
}

model_weights.LiblineaR <- function(model) {
  ret <- tibble::tibble(colnames(model$W), model$W[1, ])
  colnames(ret) <- c("word", "value")

  if(!is.null(model$ClassNames)) {
    ret$value <- -ret$value
  }
  ret %>%
    dplyr::filter(word != "Bias") %>%
    dplyr::arrange(desc(value)) %>%
    dplyr::mutate(model_type = model$TypeDetail)

}

model_weights.xgb.Booster <- function(model) {
  ret <- tibble::as_tibble(xgboost::xgb.importance(model = model))
  colnames(ret) <- c("word", "value", "cover", "frequency")

  ret %>%
    dplyr::mutate(model_type = paste("xgboost gradient boosted trees", model$params$objective))

}

model_performance <- function(model, dfm, initial_split, feat, weight, use = "testing") {
  UseMethod("model_performance")
}

model_performance.LiblineaR <- function(model, dfm, initial_split, feat,
                                        weight = c("ppmi", "tfidf", "none"),
                                        use = "testing") {
  weight <- match.arg(weight, c("ppmi", "tfidf", "none"))
  use <- match.arg(use, c("testing", "training"))
  if(use == "testing") {
    testing_dfm <- get_test_sample(dfm, initial_split)
  } else {
    testing_dfm <- get_training_sample(dfm, initial_split)
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
    yardstick::metrics(truth = truth, estimate = estimate) %>%
    dplyr::mutate(model_type = model$TypeDetail)

  if(model_type == "classification") {
    conf_mat <- preds %>%
      yardstick::conf_mat(truth = truth, estimate = estimate) %>%
      yardstick::tidy()

    res <- res %>%
      dplyr::mutate(conf_mat = list(conf_mat))
  }

  res

}

model_performance.xgb.Booster <-  function(model, dfm, initial_split, feat,
                                           weight = c("ppmi", "tfidf", "none"),
                                           use = "testing") {

  weight <- match.arg(weight, c("ppmi", "tfidf", "none"))
  use <- match.arg(use, c("testing", "training"))
  if(use == "testing") {
    testing_dfm <- get_test_sample(dfm, initial_split)
  } else {
    testing_dfm <- get_training_sample(dfm, initial_split)
  }
  model_type <- ifelse(stringr::str_detect(model$params$objective, "reg:"), "regression",
                       "classification")

  x_test <- get_x(testing_dfm, feat = feat, weight = weight)

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
      yardstick::metrics(truth = truth, estimate = estimate) %>%
      dplyr::mutate(model_type = paste("xgboost gradient boosted trees", model$params$objective))

  }

  if(model_type == "classification") {

    res <- preds %>%
      dplyr::mutate(truth = factor(1-truth, levels = c(1, 0)),
                    class = factor(ifelse(estimate > 0.5, 1, 0), levels = c(1, 0))) %>%
      yardstick::metrics(truth = truth, estimate = class,
                         estimate) %>%
      dplyr::mutate(model_type = paste("xgboost gradient boosted trees", model$params$objective))

    conf_mat <- preds %>%
      dplyr::mutate(truth = factor(1-truth, levels = c(1, 0)),
                    class = factor(ifelse(estimate > 0.5, 1, 0), levels = c(1, 0))) %>%
      yardstick::conf_mat(truth = truth, estimate = class) %>%
      yardstick::tidy()

    res <- res %>%
      dplyr::mutate(conf_mat = list(conf_mat))
  }

  res

}

get_x <- function(dfm, feature, weight = c("ppmi", "tfidf", "none")) {
  UseMethod("get_x", feature)
}

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

get_y <- function(dfm, feature, model_type = c("regression", "classification")) {
  UseMethod("get_y", feature)
}

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
             as.factor())
  }

}

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
             as.factor())
  }

}

get_training_sample <- function(dfm, initial_split) {
  training <- rsample::training(initial_split)
  dfm[ quanteda::docnames(dfm) %in% training$doc_id, ]
}

get_test_sample <- function(dfm, initial_split) {
  testing <- rsample::testing(initial_split)
  dfm[ quanteda::docnames(dfm) %in% testing$doc_id, ]
}

