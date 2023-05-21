# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tidyverse)
library(hathiTools)
library(rlang)
library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("tibble", "magrittr", "dplyr"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multiprocess")

# tar_make_future() configuration (okay to leave alone):
future::plan(future.batchtools::batchtools_slurm, template = "batchtools.slurm.tmpl")

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# Resources for cluster ---------------------------------------------------------------------
source("cluster_resources.R")

# Object parameters ----------------------------------------------------------------------------------
source("object_parameters.R")

# Replace the target list below with your own:
list(

# Decade selection --------------------------------------------------------

  tar_target(
    name = decades,
    command = seq(1700, 2010, by = 10),
    deployment = "main"
  ),

# Tracking of big catalog HathiFile ---------------------------------------

  tar_target(
    name = hathi_catalog,
    command = here::here("raw-hathifiles/hathi_full_20230101.txt.gz"),
    format = "file",
    deployment = "main"
  ),

# Workset creation and metadata addition ----------------------------------

  tar_target(
    name = worksets,
    command = workset_builder(names(feature), pub_date = decades:(decades+9)) %>%
      mutate(decade = decades),
    pattern = map(decades),
    deployment = "main"
  ),

  tar_target(
    name = cached_hathi_catalog,
    command = hathiTools::load_raw_hathifile(hathi_catalog),
    deployment = "main"
  ),

  tar_target(
    name = worksets_meta,
    command = cached_hathi_catalog %>%
      dplyr::filter(htid %in% worksets$htid),
    deployment = "main"
  ),

  tar_target(
    name = usable_htids,
    command = worksets_meta %>%
      dplyr::left_join(worksets) %>%
      dplyr::filter(rights_date_used >= decade, rights_date_used < decade+10,
                    decade == decades) %>%
      dplyr::mutate(rights_date_used2 = stringr::str_extract(imprint, "[0-9]{4}") %>%
                      as.double()) %>%
      dplyr::filter(rights_date_used2 <= rights_date_used,
                    lang == "eng"),
    pattern = map(decades),
    deployment = "main"
    ),

  tar_target(
    name = samples,
    command = usable_htids %>%
      dplyr::sample_n(min(750, dplyr::n()), weight = n),
    pattern = map(usable_htids),
    deployment = "main"
  ),

# File caching -------------------------------------------------------------

  tar_target(
    name = files,
    command = cache_ef_files(samples) ,
    pattern = map(samples),
    resources = tar_resources(future = tar_resources_future(
      plan = future::tweak(future.batchtools::batchtools_slurm,
                           resources = files_resources),
      resources = files_resources)),
    storage = "worker",
    retrieval = "worker",
    format = "file"
  ),

# DFM creation -------------------------------------------------------------

  tar_eval(
    values = dfms_df,
    tar_target(
      name = result,
      command = dfm_from_json(files,
                              vocab_size = vocab_size,
                              pos_pattern = pos_pattern,
                              include_pattern = include_pattern,
                              min_length = min_length,
                              page_language = page_language,
                              min_sentence_count = min_sentence_count,
                              to_lower = to_lower),
      pattern = map(files),
      packages = c("quanteda"),
      resources = tar_resources(future = tar_resources_future(
        plan = future::tweak(future.batchtools::batchtools_slurm,
                             resources = dfm_resources),
        resources = dfm_resources)),
      iteration = "list",
      memory = "transient",
      garbage_collection = TRUE,
      deployment = "worker",
      storage = "worker"
    )
  ),

  tar_target(
    name = target_feature,
    command = quanteda::dictionary(
      feature,
      tolower = FALSE),
    deployment = "main"
  ),

# FCM Creation ------------------------------------------------------------
#
#
# tar_target(
#   name = decade_fcm,
#   command = decade_dfm %>%
#     quanteda::dfm_lookup(target_feature, exclusive = FALSE)  %>%
#     compute_fcm(weight = "ppmi"),
#   pattern = map(decade_dfm),
#   resources = tar_resources(future = tar_resources_future(
#     plan = future::tweak(future.batchtools::batchtools_slurm,
#                          resources = fcm_resources),
#     resources = fcm_resources)),
#   storage = "worker",
#   retrieval = "worker",
#   iteration = "list"
# ),
#
# # Compute GLOVE word vectors -----------------------------------------
#
# tar_eval(
#   values = glove_word_vectors_df,
#   tar_target(
#     name = result,
#     command = fcm_glove_wvs(sources, nv = dims, n_iter = 30),
#     pattern = map(sources),
#     iteration = "list",
#     resources = tar_resources(future = tar_resources_future(
#       plan = future::tweak(future.batchtools::batchtools_slurm,
#                            resources = glove_word_vectors_resources),
#       resources = glove_word_vectors_resources)),
#     storage = "worker",
#     retrieval = "worker"
#   )
# ),

# Compute test-train splits -----------------------------------------

  tar_eval(
    values = splits_df,
    tar_target(
      name = result,
      command = train_test_splits(sources,
                                  feat = target_feature,
                                  downsample = downsample,
                                  type = type),
      pattern = map(sources),
      packages = c("quanteda"),
      iteration = "list",
      memory = "transient",
      garbage_collection = TRUE,
      deployment = "main"
    )
  ),

# Predictive models -------------------------------------------------------

  tar_eval(
    values = models_df,
    tar_target(
      name = result,
      command = predictive_model(dfm = sources,
                                 weight = weight,
                                 initial_split = split,
                                 feat = target_feature,
                                 engine = engine,
                                 model_type = model_type),
      pattern = map(sources, split),
      packages = c("quanteda"),
      resources = tar_resources(future = tar_resources_future(
        plan = future::tweak(future.batchtools::batchtools_slurm,
                             resources = predictive_model_resources),
        resources = predictive_model_resources)),
      storage = "worker",
      retrieval = "worker",
      iteration = "list",
      garbage_collection = TRUE
    )
  ),

# Predictive model weight extraction ---------------------------------

  tar_eval(
    values = model_weights_df,
    tar_target(
      name = result,
      command = model_weights(model) %>%
        dplyr::mutate(id = id,
                      decade = decades),
      pattern = map(model, decades),
      deployment = "main",
      packages = "quanteda"
    )
  ),

# Predictive model evaluation ---------------------------------------------

  tar_eval(
    values = model_performance_df,
    tar_target(
      name = result,
      command = model_performance(model, sources, split, feat = target_feature, use = use) %>%
        dplyr::mutate(id = id,
                      decade = decades),
      pattern = map(model, sources, split, decades),
      packages = c("quanteda"),
      resources = tar_resources(future = tar_resources_future(
        plan = future::tweak(future.batchtools::batchtools_slurm,
                             resources = evaluation_model_resources),
        resources = evaluation_model_resources)),
      storage = "worker",
      retrieval = "worker"
    )
  ),

  tar_eval(
    values = model_performance_per_volume_df,
    tar_target(
      name = result,
      command = model_performance_per_volume(model, sources, feat = target_feature) %>%
        dplyr::mutate(id = id,
                      decade = decades),
      pattern = map(model, sources, decades),
      packages = c("quanteda"),
      resources = tar_resources(future = tar_resources_future(
        plan = future::tweak(future.batchtools::batchtools_slurm,
                             resources = evaluation_model_resources),
        resources = evaluation_model_resources)),
      storage = "worker",
      retrieval = "worker"
    )
  ),

# PPMI sims and WVs --------------------------------------------------------

  tar_eval(
    values = ppmi_word_vectors_df,
    tar_target(
      name = result,
      command = funs(sources, target_feature) %>%
        dplyr::filter(word != stringr::str_to_upper(names(target_feature))) %>%
        dplyr::mutate(decade = decades,
                      measure = "PPMI of 'DEMOCRACY' with other terms",
                      scaled_value = as.numeric(scale(value)),
                      pnormed_value = pnorm(scaled_value),
                      sigmoid_value = plogis(scaled_value),
                      id = id) %>%
        dplyr::arrange(desc(value)),
      pattern = map(sources, decades),
      resources = tar_resources(future = tar_resources_future(
        plan = future::tweak(future.batchtools::batchtools_slurm,
                             resources = predictive_model_resources),
        resources = predictive_model_resources)),
      storage = "worker",
      retrieval = "worker"
    )
  ),

# SVD word vector calculation ---------------------------------------------

  tar_eval(
    values = svd_word_vectors_df,
    tar_target(
      name = result,
      command = sources %>%
        quanteda::dfm_lookup(target_feature, exclusive = FALSE) %>%
        svd_word_vectors(initial_split = split, nv = dims, weight = weight),
      pattern = map(sources, split),
      iteration = "list",
      resources = tar_resources(future = tar_resources_future(
        plan = future::tweak(future.batchtools::batchtools_slurm,
                             resources = svd_word_vectors_resources),
        resources = svd_word_vectors_resources)),
      storage = "worker",
      retrieval = "worker"
    )
  ),

# Similarity calculation for word vectors ---------------------------------

  tar_eval(
    values = dplyr::bind_rows(sims_svd_word_vectors_df),
    tar_target(
      name = result,
      command = wordVectors::closest_to(model, stringr::str_to_upper(names(target_feature)), n = Inf,
                                        fancy_names = FALSE) %>%
        dplyr::filter(word != stringr::str_to_upper(names(target_feature))) %>%
        dplyr::mutate(decade = decades,
                      scaled_value = as.numeric(scale(similarity)),
                      pnormed_value = pnorm(scaled_value),
                      sigmoid_value = plogis(scaled_value),
                      id = id) %>%
        dplyr::rename(value = similarity) %>%
        tibble::as_tibble(),
      pattern = map(model, decades),
      deployment = "main"
    )
  ),

# Combined Targets ------------------------------------------------------------------------

  tar_target(
    name = predictive_model_weights,
    command = dplyr::bind_rows(!!!model_weights_df$result) %>%
      dplyr::left_join(model_weights_df %>%
                         dplyr::mutate(dfms = as.character(sources)) %>%
                         dplyr::select(-model_type, -tidyselect::where(is.list),
                                       -prefix)),
    deployment = "main"

  ),

  tar_target(
    name = svd_model_weights,
    command = dplyr::bind_rows(!!!sims_svd_word_vectors_df$result)  %>%
      dplyr::left_join(sims_svd_word_vectors_df %>%
                         dplyr::mutate(dfms = as.character(sources)) %>%
                         dplyr::select(-tidyselect::where(is.list),
                                       -prefix)),
    deployment = "main"

  ),

  tar_target(
    name = ppmi_model_weights,
    command = dplyr::bind_rows(!!!ppmi_word_vectors_df$result) %>%
      dplyr::left_join(ppmi_word_vectors_df %>%
                         dplyr::mutate(dfms = as.character(sources)) %>%
                         dplyr::select(-tidyselect::where(is.list),
                                       -prefix, -funs)),
    deployment = "main"
  ),

  tar_target(
    name = all_model_weights,
    command = dplyr::bind_rows(svd_model_weights,
                               ppmi_model_weights,
                               predictive_model_weights),
    deployment = "main"
  ),

  tar_target(
    name = combined_performance,
    command = dplyr::bind_rows(!!!model_performance_df$result) %>%
      dplyr::left_join(model_performance_df %>%
                         dplyr::select(-model_type, -tidyselect::where(is.list))),
    deployment = "main"
  ),

  # tar_target(
  #   name = combined_performance_per_volume,
  #   command = dplyr::bind_rows(!!!model_performance_per_volume_df$result),
  #   deployment = "main",
  #   garbage_collection = TRUE
  # ),

  tar_target(
    name = combined_weights,
    command = all_model_weights %>%
      dplyr::filter(word != stringr::str_to_upper(names(target_feature))) %>%
      dplyr::group_by(decade, word) %>%
      dplyr::summarise(mean_value = mean(value),
                       mean_scaled = mean(scaled_value),
                       mean_pnormed = mean(pnormed_value),
                       mean_sigmoid = mean(sigmoid_value)) %>%
      dplyr::mutate(pos = stringr::str_extract(word, "(?<=_)[NVBJnvbj]{2}")) ,
    deployment = "main"

  ),

# Graphs ------------------------------------------------------------------

  tar_eval(
    values = graphs_df,
    tar_target(name = result,
               command = graph_similarities(
                 sources %>%
                   dplyr::filter(stringr::str_detect(word, graph_pos_patterns)),
                 top_n = max_per_decade,
                 var = value,
                 max_n = max_num,
                 collapse_cased = collapse_cased),
               iteration = "list",
               memory = "transient",
               garbage_collection = TRUE,
               deployment = "main"
    )
  ),

# Basic corpus stats ------------------------------------------------------

  tar_target(
    name = democracy_word_counts,
    command = query_bookworm(c("democracy"), counttype = c("WordCount"),
                             lims = c(1700, 2020)),
    deployment = "main"
  ),

  tar_target(
    name = democracy_words_per_million,
    command = query_bookworm(c("democracy"), counttype = c("WordsPerMillion"),
                             lims = c(1700, 2020)),
    deployment = "main"
  ),

  tar_target(
    name = total_words,
    command = query_bookworm(counttype = c("TotalWords"),
                             lims = c(1700, 2020)),
    deployment = "main"
  ),

  tar_target(
    name = democracy_text_counts,
    command = query_bookworm(c("democracy"), counttype = c("TextCount"),
                             lims = c(1700, 2020)),
    deployment = "main"
  ),

  tar_target(
    name = democracy_text_percent,
    command = query_bookworm(c("democracy"), counttype = c("TextPercent"),
                             lims = c(1700, 2020)),
    deployment = "main"
  ),

  tar_target(
    name = total_texts,
    command = query_bookworm(counttype = "TotalTexts",
                             lims = c(1700, 2020)),
    deployment = "main"
  ),

  tar_target(
    name = num_ht_bib_keys,
    command = cached_hathi_catalog %>%
      dplyr::pull(ht_bib_key) %>%
      dplyr::n_distinct(),
    deployment = "main"
  ),

  tar_target(
    name = num_author_title,
    command = cached_hathi_catalog %>%
      dplyr::mutate(author_title = paste(author, title)) %>%
      dplyr::pull(author_title) %>%
      dplyr::n_distinct(),
    deployment = "main"
  ),

  tar_target(
    name = num_htids,
    command = cached_hathi_catalog %>%
      dplyr::group_by(ht_bib_key) %>%
      dplyr::summarise(num_htids = dplyr::n_distinct(htid)) %>%
      dplyr::count(num_htids),
    deployment = "main"
  ),

  tar_target(
    name = num_htids_per_author,
    command = cached_hathi_catalog %>%
      dplyr::group_by(author, title) %>%
      dplyr::summarise(num_htids = dplyr::n_distinct(htid)) %>%
      dplyr::ungroup() %>%
      dplyr::count(num_htids),
    deployment = "main"
  ),

  tar_target(
    name = num_libraries,
    command = cached_hathi_catalog %>%
      dplyr::count(source),
    deployment = "main"
  ),

  tar_target(
    name = date_info,
    command = cached_hathi_catalog %>%
      dplyr::count(rights_date_used),
    deployment = "main"
  ),

  tar_target(
    name = democracy_translations,
    command = here::here("democracy_translations.xlsx"),
    format = "file",
    deployment = "main"
  ),

  tar_target(
    name = democracy_trans,
    command = democracy_translations_freqs(democracy_translations),
    deployment = "main"
  ),

# Paper and appendixes ------------------------------------------------

  tar_target(
    name = graph_document,
    command = rmd_blocks_graph_names(graphs_df),
    format = "file",
    deployment = "main"
  ),

  tar_knit(
    name = Appendix,
    path = "Paper/Appendix.rmd",
    output = "Paper/Appendix.md",
    deployment = "main"
  )
)


