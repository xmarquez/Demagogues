train_test_splits <-function(dfm, feat, downsampled, type) {
  UseMethod("train_test_splits", feat)
}

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

train_test_splits.character <- function(dfm, feat, downsampled, type) {

  df <- dfm%>%
    quanteda::select(feat)%>%
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

  sims <- cosine_sims(dfm_false, dfm_true_vec)%>%
    quanteda::as.dfm()%>%
    quanteda::convert(to = "data.frame")%>%
    tibble::as_tibble()%>%
    dplyr::arrange(-2)%>%
    dplyr::slice_max(feat1, n = nrow(dfm_true))

  ids_true <- quanteda::docnames(dfm_true)
  ids_false <- sims$doc_id

  all_ids <- c(ids_true, ids_false)

  in_id <- which(split$data$doc_id %in% all_ids)

  split$in_id <- in_id

  split

}

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

cosine_sims <- function(x, y) {
  Matrix::tcrossprod(x, y)/(sqrt(Matrix::tcrossprod(Matrix::rowSums(x^2), Matrix::rowSums(y^2))))
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

  preds <- tibble::tibble(truth = 1 - y_test, estimate = predictions)

  if(model_type == "regression") {
    res <- preds %>%
      yardstick::metrics(truth = truth, estimate = estimate) %>%
      dplyr::mutate(model_type = paste("xgboost gradient boosted trees", model$params$objective))

  }

  if(model_type == "classification") {

    res <- preds %>%
      dplyr::mutate(truth = factor(truth, levels = c(0, 1)),
                    class = factor(ifelse(estimate < 0.5, 0, 1), levels = c(0, 1))) %>%
      yardstick::metrics(truth = truth, estimate = class,
                         estimate) %>%
      dplyr::mutate(model_type = paste("xgboost gradient boosted trees", model$params$objective))

    conf_mat <- preds %>%
      dplyr::mutate(truth = factor(truth, levels = c(0, 1)),
                    class = factor(ifelse(estimate < 0.5, 0, 1), levels = c(0, 1))) %>%
      yardstick::conf_mat(truth = truth, estimate = class) %>%
      yardstick::tidy()

    res <- res %>%
      dplyr::mutate(conf_mat = list(conf_mat))
  }

  res

}

predictive_model.glmnet <- function(training_dfm, feat, weight, model_type, ...) {

  x_train <- get_x(training_dfm, feat = feat, weight = weight)

  y_train <- get_y(training_dfm, feat, model_type = model_type)

  if(model_type == "regression") {
    glmnet_model <- glmnet::cv.glmnet(x = x_train, y = y_train, family = "gaussian", ...)
  } else {
    glmnet_model <- glmnet::cv.glmnet(x = x_train, y = y_train, family = "binomial", ...)
  }

  glmnet_model

}

model_weights.cv.glmnet <- function(model) {
  glmnet::predict.cv.glmnet(model, s = "lambda.min", type = "coef") %>%
    Matrix::rowMeans() %>%
    tibble::enframe() %>%
    dplyr::arrange(-value) %>%
    dplyr::mutate(model_type = paste(class(model), class(model$glmnet.fit), collapse = " "))
}

model_performance.cv.glmnet <- function(model, dfm, initial_split, feat,
                                        weight = c("ppmi", "tfidf", "none"),
                                        use = "testing") {
  weight <- match.arg(weight, c("ppmi", "tfidf", "none"))
  use <- match.arg(use, c("testing", "training"))
  if(use == "testing") {
    testing_dfm <- get_test_sample(dfm, initial_split)
  } else {
    testing_dfm <- get_training_sample(dfm, initial_split)
  }
  if("lognet" %in% class(model$glmnet.fit)) {
    model_type <- "classification"

  } else if("elnet" %in% class(model$glmnet.fit)) {
    model_type <- "regression"
  }

  x_test <- get_x(testing_dfm, feat = feat, weight = weight)

  y_test <- get_y(testing_dfm, feat, model_type = model_type)

  measures <- glmnet::assess.glmnet(model, newx = x_test, newy = y_test, s = "lambda.min")

  measures <- tibble::tibble(.metric = names(measures),
                             .estimator = "assess.glmnet",
                             .estimate = unlist(measures))

  if(model_type == "classification") {
    conf_mat <- glmnet::confusion.glmnet(model, newx = x_test, newy = y_test, s = "lambda.min") %>%
      tibble::as_tibble()
    measures <- measures %>%
      dplyr::mutate(conf_mat = list(conf_mat))

  }

  measures %>%
    dplyr::mutate(model_type = paste("cv.glmnet", model_type))

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
             factor(levels = c(FALSE, TRUE)))
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
             factor(levels = c(FALSE, TRUE)))
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

