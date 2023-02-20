compute_dfm <- function(dataset, cache_format = "rds",
                        max_features = 30000, min_length = 3,
                        min_sentence_count = 3) {

  dfm <- dataset %>%
    hathiTools::read_cached_htids(cache_format = cache_format, cache_type = c("ef", "pagemeta")) %>%
    dplyr::filter(sentenceCount >= min_sentence_count,
                  stringr::str_detect(POS, "NN|JJ|VB"),
                  stringr::str_length(token) >= min_length,
                  stringr::str_detect(token, "^\\p{L}+$"),
                  section == "body") %>%
    dplyr::mutate(token = paste(token, POS, sep = "_"),
                  doc_id = paste(htid, page)) %>%
    dplyr::count(doc_id, token, wt = count) %>%
    dplyr::mutate(token = stringr::str_remove(token, "(?<=_[NJVB]{2}).+")) %>%
    tidytext::cast_dfm(doc_id, token, n) %>%
    # quanteda::dfm_tolower() %>%
    quanteda::dfm_trim(max_features, termfreq_type = "rank")

  dfm

}

dfm_ppmi <- function(dfm, base = 10) {
  # this is for a column-oriented sparse matrix; transpose if necessary
  dfm_row_sum <- Matrix::rowSums(dfm)
  dfm_col_sum <- Matrix::colSums(dfm)
  N <- sum(dfm_row_sum)
  col_prob <- dfm_col_sum/N
  row_prob <- dfm_row_sum/N
  pp = dfm@p+1
  ip = dfm@i+1
  tmpx = rep(0,length(dfm@x)) # new values go here, just a numeric vector
  # iterate through sparse matrix:
  all_zeros <- which(dfm_col_sum == 0)
  col_indexes_used <- 1:(length(dfm@p)-1)
  col_indexes_used <- col_indexes_used[ !col_indexes_used %in% all_zeros ]
  for(i in col_indexes_used){
    ind = pp[i]:(pp[i+1]-1)
    not0 = ip[ind]
    icol = dfm@x[ind]
    tmp = log( (icol/N) / (row_prob[not0] * col_prob[i] ), base = base) # PMI
    tmpx[ind] = tmp
  }
  dfm@x = tmpx
  # to convert to PPMI, replace <0 values with 0 and do a Matrix::drop0().
  dfm@x[which(dfm@x < 0)] <- 0
  dfm <- Matrix::drop0(dfm)
  quanteda::as.dfm(dfm)
}

feature_ppmi <- function(dfm, feature, base = 10) {
  UseMethod("feature_ppmi", feature)
}

feature_ppmi.dictionary2 <- function(dfm, feature, base = 10) {
  dfm <- dfm %>%
    quanteda::dfm_lookup(feature, exclusive = FALSE)

  feature_ppmi.default(dfm, stringr::str_to_upper(names(feature)), base = base)

}

feature_ppmi.default <- function(dfm, feature, base = 10) {

  dfm_col_sum <- Matrix::colSums(dfm)
  feat_sum <- Matrix::colSums(dfm %>%
                                quanteda::dfm_select(pattern = feature))
  total_sum <- sum(dfm)
  cooccurrence_vec <- cooccurrence_feature(dfm, feature)

  p_x_y <- cooccurrence_vec / sum(cooccurrence_vec)
  p_x <- feat_sum / total_sum
  p_y <- dfm_col_sum / total_sum

  ppmi_vector <- log(p_x_y/ tcrossprod(p_x, p_y), base = base)
  ppmi_vector[ which(ppmi_vector@x < 0) ] <- 0
  ppmi_vector %>%
    Matrix::t() %>%
    as.matrix() %>%
    tibble::as_tibble(rownames = "word") %>%
    dplyr::rename_with(function(x) {"value"}, -word)

}

cooccurrence_feature <- function(dfm, feature) {
  # Get the feature column
  feature_col <- dfm[,feature]
  # Get the transpose of the feature column
  feature_col_t <- Matrix::t(feature_col)
  # Get the co-occurrence values by matrix cross-product of the feature column and the document-feature matrix
  cooccurrence_vector <- feature_col_t %*% dfm
  cooccurrence_vector
}

fcm_ppmi <- function(fcm, dfm, base = 10) {

  feat_sum <- Matrix::colSums(dfm)
  total_sum <- sum(dfm)

  p_x_y <- fcm / sum(fcm)
  p_x <- feat_sum / total_sum
  p_y <- feat_sum / total_sum

  pp = fcm@p+1
  ip = fcm@i+1
  tmpx = rep(0,length(fcm@x)) # new values go here, just a numeric vector
  # iterate through sparse matrix:
  all_zeros <- which(feat_sum == 0)
  col_indexes_used <- 1:(length(fcm@p)-1)
  col_indexes_used <- col_indexes_used[ !col_indexes_used %in% all_zeros ]
  for(i in col_indexes_used){
    ind = pp[i]:(pp[i+1]-1)
    not0 = ip[ind]
    icol = fcm@x[ind]
    tmp = log( (icol/total_sum) / (p_x[not0] * p_y[i] ), base = base) # PMI
    tmpx[ind] = tmp
  }
  fcm@x = tmpx
  fcm@x[which(fcm@x < 0)] <- 0
  fcm <- Matrix::drop0(fcm)
  fcm

}


dfm_svd_wvs <- function(dfm, nv = 50) {
  embeddings <- irlba::irlba(dfm, nv = nv)
  embeddings <- embeddings$v

  rownames(embeddings) <- colnames(dfm)
  embeddings
}

svd_word_vectors <- function(dfm, initial_split, nv = 50, weight = c("ppmi", "tfidf", "none")) {
  dfm <- get_training_sample(dfm, initial_split)
  if(is.na(weight) || is.null(weight)) {
    weight <- "none"
  }

  weight <- match.arg(weight, c("ppmi", "tfidf"))
  if(weight == "ppmi") {
    embeddings <- dfm %>%
      dfm_ppmi() %>%
      dfm_svd_wvs(nv = nv) %>%
      wordVectors::as.VectorSpaceModel()
  } else if(weight == "tfidf") {
    embeddings <- dfm %>%
      quanteda::dfm_tfidf() %>%
      dfm_svd_wvs(nv = nv) %>%
      wordVectors::as.VectorSpaceModel()
  } else if(weight == "none") {
    embeddings <- dfm %>%
      dfm_svd_wvs(nv = nv) %>%
      wordVectors::as.VectorSpaceModel()
  }
  embeddings
}

ppmi_similarities <- function(dfm, feat) {
  UseMethod("ppmi_similarities", feat)
}

ppmi_similarities.dictionary2 <- function(dfm, feat) {
  dfm <- dfm %>%
    quanteda::dfm_lookup(feat, exclusive = FALSE) %>%
    Matrix::t()

  ppmi_sims <- cosine_sims(dfm[ stringr::str_to_upper(names(feat)), ], dfm) %>%
    as.vector() %>%
    tibble::enframe() %>%
    dplyr::mutate(word = rownames(dfm), .before = everything()) %>%
    dplyr::arrange(-value) %>%
    dplyr::select(-name)

  ppmi_sims

}

ppmi_similarities.character <- function(dfm, feat) {
  dfm <- dfm %>%
    Matrix::t()

  ppmi_sims <- cosine_sims(dfm[ feat, ], dfm) %>%
    as.vector() %>%
    tibble::enframe() %>%
    dplyr::mutate(word = rownames(dfm), .before = everything()) %>%
    dplyr::arrange(-value) %>%
    dplyr::select(-name)

  ppmi_sims

}

embed_docs <- function(dfm, embeddings, feat) {
  print(dim(dfm))
  print(dim(embeddings))

  UseMethod("embed_docs", feat)
}

embed_docs.dictionary2 <- function(dfm, embeddings, feat) {

  feature <- dfm %>%
    quanteda::dfm_lookup(feat)

  dfm <- dfm %>%
    quanteda::dfm_match(rownames(embeddings))

  print(dim(dfm))
  print(dim(embeddings))


  res <- Matrix::crossprod(Matrix::t(dfm), embeddings)
  res <- quanteda::as.dfm(res)

  feature %>% quanteda:::cbind.dfm(res)

}

embed_docs.character <- function(dfm, embeddings, feat) {

  feature <- dfm %>%
    quanteda::dfm_select(feat)

  res <- Matrix::crossprod(Matrix::t(dfm), embeddings)
  res <- quanteda::as.dfm(res)

  feature %>% quanteda:::cbind.dfm(res)

}

compute_fcm <- function(dfm,
                        weight = c("ppmi", "none"),
                        count = c("frequency", "boolean"),
                        tri = FALSE) {
  weight <- match.arg(weight, c("ppmi", "none"))
  count <- match.arg(count, c("frequency", "boolean"))

  if(count == "boolean") {
    dfm <- quanteda::dfm_weight(dfm, "boolean")
  }

  fcm <- Matrix::crossprod(dfm)

  if(tri) {
    fcm <- Matrix::triu(fcm)
  }

  if(weight == "ppmi") {
    fcm <- fcm_ppmi(fcm, dfm)
  }
  fcm

}
