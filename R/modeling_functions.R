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
    quanteda::dfm_select(feat)%>%
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
                             engine = c("LiblineaR", "glmnet", "xgboost"),
                             pattern = ".",
                             ...) {
  engine <- match.arg(engine, c("LiblineaR", "glmnet", "xgboost", "naivebayes"))
  weight <- match.arg(weight, c("ppmi", "tfidf", "none"))
  model_type <- match.arg(model_type, c("regression", "classification"))
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
                                                ...),
         glmnet = predictive_model.glmnet(training_dfm = training_dfm,
                                          feat = feat, weight = weight,
                                          model_type = model_type,
                                          ...),
         xgboost = predictive_model.xgboost(training_dfm = training_dfm,
                                            feat = feat, weight = weight,
                                            model_type = model_type,
                                            ...),
         naivebayes = predictive_model.naivebayes(training_dfm = training_dfm,
                                                  feat = feat, weight = weight,
                                                  model_type = model_type,
                                                  ...))

}

predictive_model.naivebayes <- function(training_dfm, feat, weight, model_type, ...) {

  x_train <- get_x(training_dfm, feat = feat, weight = weight)

  y_train <- get_y(training_dfm, feat, model_type = model_type)

  x_train <- as(x_train, "dgCMatrix")

  stopifnot(model_type == "classification")

  model <- naivebayes::multinomial_naive_bayes(x_train, y_train, laplace = 1)

  model
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

    wi <- table(y_train)

    svm_model <- LiblineaR::LiblineaR(x_train, y_train, cost = cost,
                                      type = type, wi = wi)
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

  scale_pos_weight = sum(y_train)/sum(y_train == 0)

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
      nrounds <- 120
    }
    params <- c(params, list("scale_pos_weight" = scale_pos_weight,
                             "eval_metric" = "auc",
                             "eta" = 0.1,
                             "max_depth" = 12,
                             colsample_bytree = 0.5,
                             lambda = 0.5,
                             subsample = 0.5,
                             "nthread" = parallel::detectCores()))

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
    if(!as.logical(model$ClassNames[1])) {
      ret$value <- -ret$value
    }
  }

  ret %>%
    dplyr::filter(word != "Bias") %>%
    dplyr::arrange(desc(value)) %>%
    dplyr::mutate(model_type = model$TypeDetail,
                  scaled_value = as.numeric(scale(value)),
                  pnormed_value = pnorm(scaled_value),
                  sigmoid_value = plogis(scaled_value))

}

model_weights.multinomial_naive_bayes <- function(model) {

  ret <- naivebayes:::coef.multinomial_naive_bayes(model) %>%
    dplyr::as_tibble(rownames = "word") %>%
    dplyr::mutate(value = `TRUE` - `FALSE`)

  ret %>%
    dplyr::arrange(desc(value)) %>%
    dplyr::mutate(model_type = class(model),
                  scaled_value = value - min(value),
                  scaled_value = scaled_value/sum(scaled_value),
                  pnormed_value = NA,
                  sigmoid_value = NA)

}

model_weights.xgb.Booster <- function(model) {

  ret <- tibble::as_tibble(xgboost::xgb.importance(model = model))
  colnames(ret) <- c("word", "value", "cover", "frequency")

  other_features <- tibble::tibble(word = model$feature_names)

  ret <- ret %>%
    dplyr::full_join(other_features, by = "word") %>%
    dplyr::mutate(dplyr::across(value:frequency, ~ifelse(is.na(.), 0, .)),
                  scaled_value = as.numeric(scale(value)),
                  pnormed_value = pnorm(scaled_value),
                  sigmoid_value = plogis(scaled_value))

  ret %>%
    dplyr::mutate(model_type = paste("xgboost gradient boosted trees", model$params$objective))

}

predictive_model.glmnet <- function(training_dfm, feat, weight, model_type, ...) {

  n_cores <- parallel::detectCores()
  doParallel::registerDoParallel(n_cores)

  x_train <- get_x(training_dfm, feat = feat, weight = weight)

  y_train <- get_y(training_dfm, feat, model_type = model_type)

  if(model_type == "regression") {
    glmnet_model <- glmnet::cv.glmnet(x = x_train, y = y_train, family = "gaussian",
                                      parallel = TRUE, ...)
  } else {
    glmnet_model <- glmnet::cv.glmnet(x = x_train, y = y_train, family = "binomial",
                                      parallel = TRUE, ...)
  }

  doParallel::stopImplicitCluster()

  glmnet_model

}

model_weights.cv.glmnet <- function(model) {

  glmnet:::predict.cv.glmnet(model, s = "lambda.min", type = "coef") %>%
    Matrix::rowMeans() %>%
    tibble::enframe() %>%
    dplyr::arrange(-value) %>%
    dplyr::mutate(model_type = paste(class(model), class(model$glmnet.fit), collapse = " "),
                  scaled_value = as.numeric(scale(value)),
                  pnormed_value = pnorm(scaled_value),
                  sigmoid_value = plogis(scaled_value)) %>%
    dplyr::rename(word = name) %>%
    dplyr::filter(word != "(Intercept)")
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
