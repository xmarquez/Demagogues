#' Compute a document-feature matrix (DFM) from cached EF data
#'
#' Reads cached HathiTrust Extracted Features token/POS counts (via
#' `hathiTools::read_cached_htids()`), filters to body tokens with sufficient
#' sentence counts, and returns a trimmed `quanteda` DFM.
#'
#' @param dataset A data frame of HTIDs/metadata accepted by
#'   `hathiTools::read_cached_htids()`.
#' @param cache_format Cache format passed through to
#'   `hathiTools::read_cached_htids()` (e.g., `"rds"`).
#' @param max_features Maximum number of features to keep after trimming by
#'   term-frequency rank.
#' @param min_length Minimum token length to keep.
#' @param min_sentence_count Minimum `sentenceCount` per page to keep.
#'
#' @return A `quanteda` `dfm` with documents identified as `"<htid> <page>"`.
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

#' Compute a hashed/SRP DFM from cached EF data
#'
#' Builds a token DFM (similar to `compute_dfm()`), then applies the signed
#' random projection in `dfm_srp()` to produce a lower-dimensional document
#' representation.
#'
#' Optionally, dictionary features can be looked up on the original DFM and
#' appended to the projected representation.
#'
#' @param dataset A data frame of HTIDs/metadata accepted by
#'   `hathiTools::read_cached_htids()`.
#' @param added_feat Optional `quanteda` dictionary (e.g., `dictionary2`) to look
#'   up on the original DFM and append as additional columns. If `NULL`, only
#'   the SRP representation is returned.
#' @param cache_format Cache format passed to `hathiTools::read_cached_htids()`.
#' @param min_length Minimum token length to keep.
#' @param pos_pattern Regular expression for POS tags to keep.
#' @param min_sentence_count Minimum `sentenceCount` per page to keep.
#' @param dims Output dimensionality of the SRP representation.
#'
#' @return A `quanteda` `dfm` with `dims` projected features (and optionally the
#'   looked-up dictionary features).
compute_dfm_srp <- function(dataset, added_feat = NULL, cache_format = "rds", min_length = 3,
                            pos_pattern = "NN|JJ|VB",
                            min_sentence_count = 3, dims = 160) {

  dfm <- dataset %>%
    hathiTools::read_cached_htids(cache_format = cache_format, cache_type = c("ef", "pagemeta")) %>%
    dplyr::filter(sentenceCount >= min_sentence_count,
                  stringr::str_detect(POS, !!pos_pattern),
                  stringr::str_length(token) >= min_length,
                  stringr::str_detect(token, "^\\p{L}+$"),
                  section == "body") %>%
    dplyr::mutate(token = paste(token, POS, sep = "_"),
                  doc_id = paste(htid, page)) %>%
    dplyr::count(doc_id, token, wt = count) %>%
    dplyr::collect() %>%
    dplyr::mutate(token = stringr::str_remove(token, "(?<=_[NJVB]{2}).+")) %>%
    tidytext::cast_dfm(doc_id, token, n)

  if(!is.null(added_feat)) {
    added_feat <- dfm %>%
      quanteda::dfm_lookup(added_feat)
  }

  dfm <- dfm_srp(dfm, dims = dims)
  if (is.null(added_feat)) {
    return(dfm)
  }
  quanteda:::cbind.dfm(added_feat, dfm)

}

#' Compute a feature-restricted DFM from cached EF data
#'
#' Builds a token DFM and restricts the documents to those containing the
#' feature term(s) (case-insensitive), then uses `quanteda::dfm_lookup()` to add
#' the feature indicator(s) and trims vocabulary by rank.
#'
#' @param dataset A data frame of HTIDs/metadata accepted by
#'   `hathiTools::read_cached_htids()`.
#' @param feat A `quanteda` dictionary (e.g., `dictionary2`) defining the target
#'   feature term(s).
#' @param cache_format Cache format passed to `hathiTools::read_cached_htids()`.
#' @param max_features Maximum number of features to keep after trimming by
#'   term-frequency rank.
#' @param min_length Minimum token length to keep.
#' @param min_sentence_count Minimum `sentenceCount` per page to keep.
#'
#' @return A `quanteda` `dfm` restricted to documents containing the feature.
compute_dfm_feat <- function(dataset, feat, cache_format = "rds",
                             max_features = 30000, min_length = 3,
                             min_sentence_count = 3) {
  feat <- force(feat)
  char_feat <- paste("^", names(feat), "$")

  dfm <- dataset %>%
    hathiTools::read_cached_htids(cache_format = cache_format, cache_type = c("ef", "pagemeta")) %>%
    dplyr::filter(sentenceCount >= min_sentence_count,
                  stringr::str_detect(POS, "NN|JJ|VB"),
                  stringr::str_length(token) >= min_length,
                  stringr::str_detect(token, "^\\p{L}+$"),
                  section == "body") %>%
    dplyr::mutate(token = paste(token, POS, sep = "_"),
                  doc_id = paste(htid, page)) %>%
    dplyr::collect() %>%
    dplyr::group_by(doc_id) %>%
    dplyr::filter(any(stringr::str_detect(token, stringr::regex(char_feat, ignore_case = TRUE)))) %>%
    dplyr::ungroup() %>%
    dplyr::count(doc_id, token, wt = count) %>%
    dplyr::mutate(token = stringr::str_remove(token, "(?<=_[NJVB]{2}).+")) %>%
    tidytext::cast_dfm(doc_id, token, n) %>%
    quanteda::dfm_tolower() %>%
    quanteda::dfm_lookup(feat, exclusive = FALSE) %>%
    quanteda::dfm_trim(max_features, termfreq_type = "rank")

  dfm

}

#' Combine a list of DFMs and attach document metadata
#'
#' Row-binds a list of DFMs and constructs `docvars` by splitting the document
#' names into `htid` and `page` (assuming documents are named `"<htid> <page>"`),
#' then left-joins additional metadata.
#'
#' @param dfm_list List of `quanteda` `dfm` objects to combine.
#' @param dfm_meta A data frame containing metadata to join onto `docvars`. Must
#'   include `htid` and `page` columns (or matching keys).
#'
#' @return A combined `quanteda` `dfm` with `docvars` attached.
combine_dfm_list <- function(dfm_list, dfm_meta) {
  combined_dfm <- do.call(quanteda:::rbind.dfm, dfm_list)
  docvars_df <- rownames(combined_dfm) %>%
    as_tibble() %>%
    tidyr::separate(value,
                    into = c("htid", "page"),
                    sep = " ")

  docvars_df <- dplyr::left_join(docvars_df, dfm_meta)

  quanteda::docvars(combined_dfm) <- docvars_df
  combined_dfm
}


#' Latent semantic analysis (LSA) document embeddings for a DFM
#'
#' Computes a truncated SVD of the DFM using `irlba::irlba()` and returns the
#' document embedding matrix (`u`) as a DFM.
#'
#' @param dfm A `quanteda` `dfm` (or compatible sparse matrix).
#' @param nu Number of latent dimensions to compute.
#'
#' @return A `quanteda` `dfm` whose features are the LSA dimensions.
dfm_lsa <- function(dfm, nu = 50) {
  embeddings <- irlba::irlba(dfm, nv = nu)
  embeddings <- embeddings$u
  rownames(embeddings) <- rownames(dfm)
  quanteda::as.dfm(embeddings)

}

#' t-SNE projection for DFM rows
#'
#' Converts a DFM to a dense matrix, removes duplicate rows, runs t-SNE via
#' `Rtsne::Rtsne()`, and returns a tibble of coordinates.
#'
#' @param dfm A `quanteda` `dfm` (or matrix-like) of document vectors.
#' @param ... Additional arguments passed to `Rtsne::Rtsne()`.
#'
#' @return A tibble with columns `doc_id`, `x`, and `y`.
dfm_tsne <- function(dfm, ...) {
  dfm <- as.matrix(dfm)
  dfm <- dfm[ !duplicated(dfm), ]
  res <- Rtsne::Rtsne(dfm, num_threads = parallel::detectCores(), ...)
  rownames(res$Y) <- rownames(dfm)
  colnames(res$Y) <- c("x", "y")
  tibble::as_tibble(res$Y, rownames = "doc_id")
}

#' Convert counts to positive pointwise mutual information (PPMI)
#'
#' Computes PMI for each non-zero entry of a sparse document-feature matrix and
#' zeroes out negative values to yield PPMI.
#'
#' @param dfm A count-weighted `quanteda` `dfm` (or `dgCMatrix`). The function
#'   operates on the underlying sparse matrix slots.
#' @param base Log base for PMI.
#'
#' @return A `quanteda` `dfm` weighted by PPMI.
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

#' Compute feature-specific PPMI scores (S3 generic)
#'
#' Computes PPMI association between a target feature term and all other terms
#' in the DFM.
#'
#' @param dfm A `quanteda` `dfm` (or compatible sparse matrix).
#' @param feature Feature definition; dispatches on the class of `feature`.
#' @param base Log base for PMI.
#'
#' @return A tibble with columns `word` and `value`.
feature_ppmi <- function(dfm, feature, base = 10) {
  UseMethod("feature_ppmi", feature)
}

#' Compute feature-specific PPMI scores (dictionary2 method)
#'
#' Looks up the dictionary in the DFM and computes PPMI for the (uppercased)
#' dictionary key name(s) against all terms.
#'
#' @param dfm A `quanteda` `dfm` (or compatible sparse matrix).
#' @param feature A `quanteda` `dictionary2` defining the feature term(s).
#' @param base Log base for PMI.
#'
#' @return A tibble with columns `word` and `value`.
feature_ppmi.dictionary2 <- function(dfm, feature, base = 10) {
  dfm <- dfm %>%
    quanteda::dfm_lookup(feature, exclusive = FALSE)

  feature_ppmi.default(dfm, stringr::str_to_upper(names(feature)), base = base)

}

#' Compute feature-specific PPMI scores (default method)
#'
#' Computes PPMI association between a feature term (typically a single token
#' present in the DFM) and all other terms.
#'
#' @param dfm A `quanteda` `dfm` (or compatible sparse matrix).
#' @param feature Character vector of feature term(s) to use. This function is
#'   primarily designed for a single term.
#' @param base Log base for PMI.
#'
#' @return A tibble with columns `word` and `value`.
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

#' Co-occurrence counts between a feature term and all tokens
#'
#' Computes the feature-by-token co-occurrence vector via matrix multiplication
#' of a feature column with the full document-feature matrix.
#'
#' @param dfm A `quanteda` `dfm` (or compatible sparse matrix).
#' @param feature A feature term present in `colnames(dfm)` (typically length 1).
#'
#' @return A 1-by-`ncol(dfm)` sparse matrix of co-occurrence counts.
cooccurrence_feature <- function(dfm, feature) {
  # Get the feature column
  feature_col <- dfm[,feature]
  # Get the transpose of the feature column
  feature_col_t <- Matrix::t(feature_col)
  # Get the co-occurrence values by matrix cross-product of the feature column and the document-feature matrix
  cooccurrence_vector <- feature_col_t %*% dfm
  cooccurrence_vector
}

#' Apply PPMI weighting to a feature co-occurrence matrix
#'
#' Converts a feature co-occurrence matrix (typically from
#' `MatrixExtra::crossprod()`) into a PPMI-weighted matrix using marginal term
#' probabilities derived from the original DFM.
#'
#' @param fcm A sparse feature co-occurrence matrix (features x features).
#' @param dfm The original count DFM used to derive marginals.
#' @param base Log base for PMI.
#'
#' @return A sparse matrix of the same shape as `fcm`, weighted by PPMI.
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


#' Compute term embeddings from a DFM via truncated SVD
#'
#' Computes right singular vectors (`v`) of the DFM using `irlba::irlba()`. If
#' `nv` is too large for the matrix shape, it is reduced to `min(nrow, ncol) - 1`.
#'
#' @param dfm A `quanteda` `dfm` (or compatible sparse matrix).
#' @param nv Number of singular vectors to compute.
#'
#' @return A numeric matrix of size `ncol(dfm)` by `nv`, with row names set to
#'   the DFM feature names.
dfm_svd_wvs <- function(dfm, nv = 50) {
  print(paste("Old nv:", nv))
  print(min(nrow(dfm), ncol(dfm)))
  if(nv >= min(nrow(dfm), ncol(dfm))) {
    nv <- min(nrow(dfm), ncol(dfm)) - 1
  }
  print(paste("New nv:", nv))

  embeddings <- irlba::irlba(dfm, nv = nv)
  embeddings <- embeddings$v

  rownames(embeddings) <- colnames(dfm)
  embeddings
}

#' SVD-based word vectors from a training split
#'
#' Subsets a DFM to the training sample (via `get_training_sample()`), applies an
#' optional weighting scheme, computes SVD term embeddings, and wraps them as a
#' `wordVectors` vector space model.
#'
#' @param dfm A `quanteda` `dfm` of document-feature counts.
#' @param initial_split A split object used by `get_training_sample()` (created
#'   elsewhere in the pipeline).
#' @param nv Number of singular vectors to compute.
#' @param weight Weighting scheme to apply before SVD (`"ppmi"`, `"tfidf"`, or
#'   `"none"`).
#'
#' @return A `wordVectors` `VectorSpaceModel`.
svd_word_vectors <- function(dfm, initial_split, nv = 50, weight = c("ppmi", "tfidf", "none")) {
  dfm <- get_training_sample(dfm, initial_split)
  if(is.na(weight) || is.null(weight)) {
    weight <- "none"
  }

  weight <- match.arg(weight, c("ppmi", "tfidf", "none"))
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

#' Cosine similarities in a PPMI space (S3 generic)
#'
#' Computes cosine similarities between a target feature row and all other rows
#' of a transposed DFM representation. Dispatches on the class of `feat`.
#'
#' @param dfm A `quanteda` `dfm` (typically already weighted), where features are
#'   compared in a term-by-document space after transposition.
#' @param feat Feature specification; dispatches on the class of `feat`.
#'
#' @return A tibble with columns `word` and `value`, sorted descending.
ppmi_similarities <- function(dfm, feat) {
  UseMethod("ppmi_similarities", feat)
}

#' Cosine similarities in a PPMI space (dictionary2 method)
#'
#' Looks up a dictionary in the DFM, transposes to term-by-document, then
#' computes cosine similarity between the dictionary key (uppercased) and all
#' terms.
#'
#' @param dfm A `quanteda` `dfm`.
#' @param feat A `quanteda` `dictionary2`.
#'
#' @return A tibble with columns `word` and `value`.
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

#' Cosine similarities in a PPMI space (character method)
#'
#' Transposes the DFM to term-by-document and computes cosine similarity between
#' `feat` and all terms.
#'
#' @param dfm A `quanteda` `dfm`.
#' @param feat Character scalar giving the target term name (a row of the
#'   transposed matrix).
#'
#' @return A tibble with columns `word` and `value`.
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

#' Embed documents using precomputed term embeddings (S3 generic)
#'
#' Computes document embeddings by multiplying a DFM (documents x terms) by a
#' term embedding matrix (terms x dims). Dispatches on the class of `feat` to
#' decide how the target feature is extracted/attached.
#'
#' @param dfm A `quanteda` `dfm` of document-feature counts.
#' @param embeddings A numeric matrix of term embeddings with row names
#'   corresponding to term names.
#' @param feat Feature specification; dispatches on the class of `feat`.
#'
#' @return A `quanteda` `dfm` containing the feature column(s) and embedding
#'   dimensions.
embed_docs <- function(dfm, embeddings, feat) {
  print(dim(dfm))
  print(dim(embeddings))

  UseMethod("embed_docs", feat)
}

#' Embed documents and attach dictionary feature(s) (dictionary2 method)
#'
#' Looks up the dictionary feature(s) and cbinds them to document embeddings
#' computed from the DFM and the provided term embedding matrix.
#'
#' @param dfm A `quanteda` `dfm` of document-feature counts.
#' @param embeddings A numeric matrix of term embeddings with row names matching
#'   DFM feature names.
#' @param feat A `quanteda` `dictionary2`.
#'
#' @return A `quanteda` `dfm` with the dictionary feature(s) and embedding dims.
embed_docs.dictionary2 <- function(dfm, embeddings, feat) {

  feature <- dfm %>%
    quanteda::dfm_lookup(feat)

  dfm <- dfm %>%
    quanteda::dfm_match(rownames(embeddings))

  print(dim(dfm))
  print(dim(embeddings))


  res <- MatrixExtra::crossprod(Matrix::t(dfm), embeddings)
  res <- quanteda::as.dfm(res)

  feature %>% quanteda:::cbind.dfm(res)

}

#' Embed documents and attach a single feature column (character method)
#'
#' Selects the feature column from the DFM and cbinds it to document embeddings
#' computed from the DFM and the provided term embedding matrix.
#'
#' @param dfm A `quanteda` `dfm` of document-feature counts.
#' @param embeddings A numeric matrix of term embeddings (terms x dims).
#' @param feat Character scalar naming a DFM feature column to attach.
#'
#' @return A `quanteda` `dfm` with the selected feature and embedding dims.
embed_docs.character <- function(dfm, embeddings, feat) {

  feature <- dfm %>%
    quanteda::dfm_select(feat)

  res <- MatrixExtra::crossprod(Matrix::t(dfm), embeddings)
  res <- quanteda::as.dfm(res)

  feature %>% quanteda:::cbind.dfm(res)

}

#' Compute a feature co-occurrence matrix (FCM) from a DFM
#'
#' Computes a feature co-occurrence matrix via `MatrixExtra::crossprod()`, with
#' optional boolean weighting of the DFM and optional PPMI reweighting.
#'
#' @param dfm A `quanteda` `dfm` of counts.
#' @param weight Weighting to apply to the resulting co-occurrence matrix
#'   (`"ppmi"` or `"none"`).
#' @param count Whether to treat counts as `"frequency"` or `"boolean"` before
#'   co-occurrence computation.
#' @param tri Logical; if `TRUE`, keep only the upper triangle of the matrix.
#'
#' @return A sparse feature co-occurrence matrix.
compute_fcm <- function(dfm,
                        weight = c("ppmi", "none"),
                        count = c("frequency", "boolean"),
                        tri = FALSE) {
  weight <- match.arg(weight, c("ppmi", "none"))
  count <- match.arg(count, c("frequency", "boolean"))

  if(count == "boolean") {
    dfm <- quanteda::dfm_weight(dfm, "boolean")
  }

  fcm <- MatrixExtra::crossprod(dfm)

  if(tri) {
    fcm <- Matrix::triu(fcm)
  }

  if(weight == "ppmi") {
    fcm <- fcm_ppmi(fcm, dfm)
  }
  fcm

}

#' Train GloVe word vectors from a co-occurrence matrix
#'
#' Fits a GloVe model using `rsparse::GloVe` on a feature co-occurrence matrix
#' and returns the resulting embeddings as a `wordVectors` vector space model.
#'
#' @param fcm A feature co-occurrence matrix (sparse).
#' @param nv Embedding dimensionality (rank).
#' @param n_iter Number of training iterations.
#' @param convergence_tol Convergence tolerance passed to `fit_transform()`.
#' @param n_threads Number of threads to use.
#' @param x_max GloVe `x_max` parameter.
#' @param learning_rate GloVe learning rate.
#' @param alpha GloVe `alpha` parameter.
#' @param lambda L2 regularization parameter.
#' @param shuffle Logical; whether to shuffle training examples.
#' @param init Optional initialization list passed to `rsparse::GloVe$new()`.
#'
#' @return A `wordVectors` `VectorSpaceModel`.
fcm_glove_wvs <- function(fcm, nv = 50,
                          n_iter = 10L,
                          convergence_tol = -1,
                          n_threads = parallel::detectCores(),
                          x_max = 10,
                          learning_rate = 0.15,
                          alpha = 0.75,
                          lambda = 0,
                          shuffle = TRUE,
                          init = list(w_i = NULL, b_i = NULL, w_j = NULL, b_j = NULL)) {

  model <- rsparse::GloVe$new(rank = nv, x_max = x_max,
                              learning_rate = learning_rate,
                              alpha = alpha,
                              lambda = lambda,
                              shuffle = shuffle,
                              init = init)

  embeddings <- model$fit_transform(fcm, n_iter = n_iter,
                                    convergence_tol = convergence_tol,
                                    n_threads = n_threads)

  embeddings <- embeddings + t(model$components)

  embeddings %>%
    wordVectors::as.VectorSpaceModel()

}

#' Deterministic signed hash vector for a token
#'
#' Hashes a token using SHA1 and converts the resulting bits into a vector of
#' `-1/+1` values. For `dims > 160`, the hash is extended by re-hashing modified
#' tokens; for `dims < 160`, it is truncated.
#'
#' @param token Character scalar token to hash.
#' @param dims Output dimensionality.
#'
#' @return A numeric vector of length `dims` with values `-1` and `1`.
hash_fun <- function(token, dims = 160) {
  if (dims > 160) {
    start <- hash_fun(token, dims = 160)
    remainder <- hash_fun(paste0(token, "_"), dims = dims - 160)
    return(c(start, remainder))
  }
  if(dims < 160) {
    start <- hash_fun(token, dims = 160)
    return(start[1:dims])
  }

  res <- rawToBits(digest::digest(token,
                                  algo = "sha1",
                                  serialize = FALSE,
                                  raw = TRUE))

  as.logical(res)*2 - 1

}

#' Hash a set of tokens into a signed projection matrix
#'
#' Applies `hash_fun()` to each token and returns a matrix suitable for SRP.
#'
#' @param tokens Character vector of token strings.
#' @param dims Output dimensionality.
#'
#' @return A numeric matrix of size `dims` by `length(tokens)`.
hash_tokens <- function(tokens, dims) {
  res <- sapply(tokens, hash_fun, simplify = TRUE, dims = dims)
  res
}

#' Signed random projection (SRP) embedding for a DFM
#'
#' Computes an SRP document embedding by multiplying the DFM with a signed hash
#' projection of its feature names.
#'
#' @param dfm A `quanteda` `dfm` of document-feature counts.
#' @param dims Output dimensionality of the projection.
#'
#' @return A `quanteda` `dfm` with `dims` projected features.
dfm_srp <- function(dfm, dims = 160) {
  hashed_tokens <- hash_tokens(colnames(dfm), dims = dims)
  MatrixExtra::tcrossprod(dfm, hashed_tokens) %>%
    quanteda::as.dfm()
}
