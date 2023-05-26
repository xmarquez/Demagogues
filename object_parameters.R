library(tidyverse)

generate_params_df <- function(prefix, params) {
  params_digest <- params %>%
    pmap(list) %>%
    map_chr(digest::digest)
  expand_grid(nesting(prefix, params)) %>%
    mutate(result = paste(prefix, params_digest, sep = "_"),
           across(any_of(c("result", "sources", "split")), rlang::syms),
           id = as.character(result))
}

# Features -----
feature <- list(democracy = c("democracy_nn", "democracy_nnp", "democracy_nns"))

# DFM parameters -----
dfm_params <- expand_grid(target_feature = names(feature),
                          vocab_size = 30000,
                          pos_pattern = "NN|VB|JJ",
                          include_pattern = "^\\p{L}+$",
                          pages_contain = ".",
                          min_length = 2,
                          page_language = "en",
                          min_sentence_count = 3,
                          nesting(
                            to_lower = c(TRUE, FALSE),
                            description = c(
                              paste(
                                "DFM of texts containing democracy,",
                                "30000 word vocab, lowercased,",
                                "english pages only, 2 letter",
                                "min sequence, 3 sentences per page",
                                "unicode letters only",
                                "(no numbers or punctuation),",
                                "NN/VB/JJ parts of speech only"
                                ),
                              paste(
                                "DFM of texts containing democracy,",
                                "30000 word vocab, not lowercased,",
                                "english pages only, 2 letter",
                                "min sequence, 3 sentences per page",
                                "unicode letters only",
                                "(no numbers or punctuation),",
                                "NN/VB/JJ parts of speech only"
                              )
                              )
                          )
                          )


dfms_df <- generate_params_df("dfm", dfm_params)

dfm_restricted_params <- expand_grid(target_feature = names(feature),
                                     vocab_size = 30000,
                                     pos_pattern = "NN|VB|JJ",
                                     include_pattern = "^\\p{L}+$",
                                     pages_contain = names(feature),
                                     min_length = 2,
                                     page_language = "en",
                                     min_sentence_count = 3,
                                     nesting(
                                       multiplier = c(1),
                                       to_lower = c(TRUE),
                                       description = c(
                                         paste(
                                           "DFM of texts containing democracy,",
                                           "30000 word vocab, lowercased,",
                                           "english pages only, 2 letter",
                                           "min sequence, 3 sentences per page",
                                           "unicode letters only",
                                           "(no numbers or punctuation),",
                                           "NN/VB/JJ parts of speech only,",
                                           "pages containing '", names(feature),
                                           "' and", multiplier,
                                           "x the number of pages not",
                                           "containing '", names(feature), "'"
                                           )
                                         )
                                       )
                                     )


dfm_restricted_df <- generate_params_df("dfm", dfm_restricted_params)

# Splits params -----
sampling_params <- tibble(downsample = c(FALSE, TRUE),
                          type = c(NA, "random"),
                          description =  c(
                            paste(
                              "Test/train split, no downsampling"
                            ),
                            paste(
                              "Test/train split, random downsampling"
                            )
                          )) %>%
  nesting() %>%
  expand_grid(sources = c(dfms_df$result, dfm_restricted_df$result)) %>%
  filter(!downsample)

splits_df <- generate_params_df("split", sampling_params)

# Predictive models -----
model_params <- expand_grid(model_type = "classification",
                            weight = "ppmi",
                            engine = c("LiblineaR", "xgboost", "glmnet"),
                            nesting(splits_df %>%
                                      rename(split = result) %>%
                                      select(-prefix))) %>%
  mutate(description = case_when(
    engine == "LiblineaR" & !downsample ~
      "SVM classification, LiblineaR engine, no downsampling",
    engine == "LiblineaR" & downsample ~
      "SVM classification, LiblineaR engine, random downsampling of majority category",
    engine == "xgboost" & !downsample ~
      "Gradient-boosted random forest classification, xgboost engine, no downsampling",
    engine == "xgboost" & downsample ~
      "Gradient-boosted random forest classification, xgboost, random downsampling of majority category",
    engine == "glmnet" & !downsample ~
      "Logistic regression classification, glmnet engine, no downsampling",
    engine == "glmnet" & downsample ~
      "Logistic regression classification, glmnet engine, random downsampling of majority category"))

models_df <- generate_params_df("predictive_model", model_params)

# SVD -----
svd_params <- expand_grid(weight = "ppmi",
                          dims = 50,
                          nesting(splits_df %>%
                                    rename(split = result) %>%
                                    select(-prefix))) %>%
  mutate(description = case_when(
    weight == "ppmi" & !downsample ~
      "SVD decomposition of document-term matrix, no downsampling",
    weight == "ppmi" & downsample ~
      "SVD decomposition of document-term matrix, random downsampling of majority category"))

svd_word_vectors_df <- generate_params_df("svd_word_vectors", svd_params)

# Weights -----
model_weights_params <- models_df %>%
  select(-prefix) %>%
  rename(model = result) %>%
  mutate(sampling_strategy = case_when(
    type == "random" ~ "random downsampling",
    TRUE ~ "no downsampling"),
    weight_names = case_when(engine == "glmnet" ~ "Coefficients of logistic regression",
                             engine == "LiblineaR" ~ "Weights of SVM model",
                             engine == "xgboost" ~ "Weights of gradient-boosted random forest model"),
    description = paste(weight_names, engine, sampling_strategy, sep = ", "),
    measure = "Model weights")

model_weights_df <- generate_params_df("weights", model_weights_params)

# Predictive model performance -----
model_performance_params <- expand_grid(
  use = c("testing", "training"),
  nesting(models_df %>%
            rename(model = result) %>%
            select(-prefix))) %>%
  mutate(description = paste0(description, ", ", use, " sample"))

model_performance_df <- generate_params_df("performance", model_performance_params)

model_performance_per_volume_params <- model_performance_df %>%
  filter(!downsample, use == "training") %>%
  select(-prefix, -use, -split, -result, -id, -type) %>%
  distinct()

model_performance_per_volume_df <- generate_params_df("performance_per_volume",
                                                      model_performance_per_volume_params)

# SVD word vector cosine similarities ----
sims_svd_params <- svd_word_vectors_df %>%
  select(-prefix) %>%
  rename(model = result) %>%
  mutate(sampling_strategy = case_when(type == "random" ~ "random downsampling",
                                       TRUE ~ "no downsampling"),
         weight_names = str_glue("Cosine similarity to target term in SVD word vector space, {str_to_upper(weight)} weights"),
         description = paste(weight_names, sampling_strategy, sep = ", "))

sims_svd_word_vectors_df <- generate_params_df("sims_svd_word_vectors", sims_svd_params)

# PPMI word vectors ----
ppmi_word_vectors_params <- expand_grid(nesting(svd_params),
                                    funs = c("feature_ppmi", "ppmi_similarities")) %>%
  mutate(weight_names = case_when(funs == "feature_ppmi" ~ "Single-feature PPMI to target feature",
                                  funs == "ppmi_similarities" ~ "Cosine similarity to target feature across PPMI-weighted DFM"),
         sampling_strategy = case_when(
           type == "random" ~ "random downsampling",
           TRUE ~ "no downsampling"),
         description = paste(weight_names, sampling_strategy, sep = ", "))

ppmi_word_vectors_df <- generate_params_df("sims_ppmi_word_vectors", ppmi_word_vectors_params)

# Graphs -----
graphs_params <- bind_rows(sims_svd_word_vectors_df,
                           model_weights_df,
                           ppmi_word_vectors_df) %>%
  select(-prefix) %>%
  rename(dfms = sources,
         sources = result) %>%
  mutate(dfms = as.character(dfms)) %>%
  left_join(dfms_df %>%
              rename(dfms = id) %>%
              select(-prefix, -result, -description)) %>%
  nesting() %>%
  expand_grid(nesting(tibble(
    graph_pos_patterns = c(".","(_nn|_NN)","(_vb|_VB)","(_jj|_JJ)", "ism_", "^[A-Z].+NN"),
    collapse_cased = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
    max_per_decade = 8,
    max_num = 60))) %>%
  mutate(description = case_when(
    graph_pos_patterns == "." ~ paste(description, "all POS", sep = ", "),
    graph_pos_patterns == "(_nn|_NN)" ~ paste(description, "nouns only", sep = ", "),
    graph_pos_patterns == "(_vb|_VB)" ~ paste(description, "verbs only", sep = ", "),
    graph_pos_patterns == "(_jj|_JJ)" ~ paste(description, "adjectives only", sep = ", "),
    graph_pos_patterns == "ism_" ~ paste(description, "words ending in ism only", sep = ", "),
    graph_pos_patterns == "^[A-Z].+NN" ~ paste(description, "terms beginning with uppercase", sep = ", "))) %>%
  filter(!(to_lower & graph_pos_patterns == "^[A-Z].+NN"))

graphs_df <- generate_params_df("graphs", graphs_params)

# fcms_df <- tibble::tibble(result = "decade_fcm") %>%
#   dplyr::mutate(across(dplyr::any_of(c("result", "sources", "split")), rlang::syms))

# glove_word_vectors_df <- tidyr::nesting(prefix = "glove_word_vectors",
#                                         sources = fcms_df$result,
#                                         dims = 50) %>%
#   tidyr::unite(col = "result", prefix, sources, dims, remove = FALSE, na.rm = TRUE) %>%
#   dplyr::mutate(across(dplyr::any_of(c("result", "sources", "split")), rlang::syms))
#
# models_simplified_eval_df <- tidyr::expand_grid(sources = "predictive_classification_decade_dfm_glmnet",
#                                                 decades_model = seq(1700, 2010, by = 5),
#                                                 dfms = "decade_dfm",
#                                                 decades_dfms = seq(1700, 2010, by = 5))
#
# sims_glove_word_vectors_df <- tidyr::nesting(prefix = "sims",
#                                              sources = glove_word_vectors_df$result,
#                                              source_names = as.character(sources)) %>%
#   tidyr::unite(col = "result", prefix, sources, remove = FALSE, na.rm = TRUE) %>%
#   dplyr::mutate(across(dplyr::any_of(c("result", "sources", "split")), rlang::syms),
#                 weight_names = "Cosine similarity to target term in GLOVE word vector space, PPMI weights",
#                 object_names = paste(weight_names, sep = ", "))





