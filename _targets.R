# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tidyverse)
library(hathiTools)
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
# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
# Replace the target list below with your own:
list(

  # Decade selection --------------------------------------------------------


  tar_target(
    name = decades,
    command = seq(1700, 2010, by = 5),
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
    name = democracy_worksets,
    command = workset_builder("democracy", pub_date = decades:(decades+9)) %>%
      mutate(decade = decades),
    pattern = map(decades),
    deployment = "main"
  ),



  tar_target(
    name = cached_hathi_catalog,
    command = hathiTools::load_raw_hathifile(hathi_catalog),
    resources = tar_resources(future = tar_resources_future(
      plan = future::tweak(future.batchtools::batchtools_slurm,
                           resources = list(partition = "quicktest", memory = "20G", ncpus = 12,
                                            walltime = "1:00:00")),
      resources = list(partition = "quicktest", memory = "20G", ncpus = 12,
                       walltime = "1:00:00")))
  ),

  tar_target(
    name = democracy_worksets_meta,
    command = cached_hathi_catalog %>%
      dplyr::filter(htid %in% democracy_worksets$htid),
    # resources = tar_resources(future = tar_resources_future(
    #   plan = future::tweak(future.batchtools::batchtools_slurm,
    #                        resources = list(partition = "quicktest", memory = "20G", ncpus = 2,
    #                                         walltime = "1:00:00")),
    #   resources = list(partition = "quicktest", memory = "20G", ncpus = 12,
    #                    walltime = "1:00:00"))),
    deployment = "main"
  ),

  tar_target(
    name = democracy_usable_htids,
    command = democracy_worksets_meta %>%
      dplyr::left_join(democracy_worksets) %>%
      dplyr::filter(rights_date_used >= decade, rights_date_used < decade+10,
                    decade == decades) %>%
      dplyr::mutate(rights_date_used2 = stringr::str_extract(imprint, "[0-9]{4}") %>%
                      as.double()) %>%
      dplyr::filter(rights_date_used2 <= rights_date_used,
                    lang == "eng"),
    pattern = map(decades),
    # resources = tar_resources(future = tar_resources_future(
    #   plan = future::tweak(future.batchtools::batchtools_slurm,
    #                        resources = list(partition = "quicktest", memory = "4G", ncpus = 4,
    #                                         walltime = "0:20:00")),
    #   resources = list(partition = "quicktest", memory = "4G", ncpus = 4,
    #                    walltime = "0:20:00"))),
    deployment = "main"
    ),

  tar_target(
    name = democracy_samples,
    command = democracy_usable_htids %>%
      dplyr::sample_n(min(500, dplyr::n()), weight = n),
    pattern = map(democracy_usable_htids),
    deployment = "main"
  ),

  tar_target(
    name = democracy_files,
    command = hathiTools::cache_htids(democracy_samples, attempt_rsync = TRUE,
                                      cache_format = "rds"),
    pattern = map(democracy_samples),
    resources = tar_resources(future = tar_resources_future(
      plan = future::tweak(future.batchtools::batchtools_slurm,
                           resources = list(partition = "quicktest", memory = "6G", ncpus = 2,
                                            walltime = "2:00:00")),
      resources = list(partition = "quicktest", memory = "6G", ncpus = 2,
                       walltime = "2:00:00")))
  ),

  tar_target(
    name = decade_dfm,
    command = compute_dfm(democracy_files, cache_format = "rds"),
    pattern = map(democracy_files),
    resources = tar_resources(future = tar_resources_future(
      plan = future::tweak(future.batchtools::batchtools_slurm,
                           resources = list(partition = "quicktest", memory = "25G", ncpus = 2,
                                            walltime = "0:40:00")),
      resources = list(partition = "quicktest", memory = "25G", ncpus = 2,
                       walltime = "0:40:00"))),
    iteration = "list"
  ),

  tar_target(
    name = democracy_feature,
    command = quanteda::dictionary(list(democracy = c("democracy_nn", "democracy_nnp", "democracy_nns")), tolower = FALSE),
    deployment = "main"
  ),

  tar_target(
      name = splits_decade_dfm,
      command = train_test_splits(decade_dfm, democracy_feature),
      pattern = map(decade_dfm),
      # resources = tar_resources(future = tar_resources_future(
      #   plan = future::tweak(future.batchtools::batchtools_slurm,
      #                        resources = list(partition = "quicktest", memory = "2G", ncpus = 2,
      #                                         walltime = "0:05:00")),
      #   resources = list(partition = "quicktest", memory = "2G", ncpus = 2,
      #                    walltime = "0:05:00"))),
      iteration = "list",
      deployment = "main"
    ),


# Predictive models -------------------------------------------------------

  tar_eval(
    values = tidyr::expand_grid(sources = c("decade_dfm"),
                                engine = c("LiblineaR", "xgboost"),
                                model_type = c("regression", "classification")) %>%
      dplyr::mutate(splits = paste("splits", sources, sep = "_"),
                    results = paste("predictive", model_type, engine, sources, sep = "_"),
                    dplyr::across(dplyr::all_of(c("sources", "results", "splits")),
                                  rlang::syms)),
    tar_target(
      name = results,
      command = predictive_model(dfm = sources,
                                 initial_split = splits,
                                 feat = democracy_feature,
                                 engine = engine,
                                 model_type = model_type),
      pattern = map(sources, splits),
      packages = c("quanteda"),
      resources = tar_resources(future = tar_resources_future(
        plan = future::tweak(future.batchtools::batchtools_slurm,
                             resources = list(partition = "quicktest", memory = "10G", ncpus = 10,
                                              walltime = "0:10:00")),
        resources = list(partition = "quicktest", memory = "10G", ncpus = 10,
                         walltime = "0:10:00"))),
      iteration = "list"
    )
  ),

# Predictive model weight extraction ---------------------------------

  tar_eval(
    values = tidyr::crossing("predictive",
                             c("regression", "classification"),
                             c("LiblineaR", "xgboost"),
                             c("decade_dfm")) %>%
      tidyr::unite("sources") %>%
      dplyr::mutate(results = paste("weights", sources, sep = "_"),
                    source_names = sources,
                    dplyr::across(dplyr::all_of(c("sources", "results")),
                                  rlang::syms)),
    tar_target(
      name = results,
      command = model_weights(sources) %>%
        dplyr::mutate(source = source_names,
                      decade = decades,
                      measure = "Model Weights"),
      pattern = map(sources, decades),
      deployment = "main",
      packages = "quanteda"
    )
  ),

# Predictive model evaluation ---------------------------------------------

  tar_eval(
    values = tidyr::crossing(prefix = "predictive",
                             model_type = c("regression", "classification"),
                             engine = c("LiblineaR", "xgboost"),
                             dfms = c("decade_dfm"),
                             use = c("testing", "training")) %>%
      dplyr::mutate(splits = paste("splits", dfms, sep = "_")) %>%
      tidyr::unite(col = "sources", prefix, model_type, engine, dfms, remove = FALSE) %>%
      dplyr::mutate(results = paste("performance", sources, use, sep = "_"),
                    source_names = sources,
                    dplyr::across(dplyr::all_of(c("sources", "results", "splits", "dfms")),
                                  rlang::syms)),
    tar_target(
      name = results,
      command = model_performance(sources, dfms, splits, feat = democracy_feature, use = use) %>%
        dplyr::mutate(source = source_names,
                      decade = decades,
                      model_type = model_type,
                      sample = use),
      pattern = map(sources, dfms, splits, decades),
      # resources = tar_resources(future = tar_resources_future(
      #   plan = future::tweak(future.batchtools::batchtools_slurm,
      #                        resources = list(partition = "quicktest", memory = "10G", ncpus = 10,
      #                                         walltime = "0:10:00")),
      #   resources = list(partition = "quicktest", memory = "10G", ncpus = 10,
      #                    walltime = "0:10:00"))),
      packages = c("quanteda"),
      deployment = "main"
    )
  ),

  tar_target(
    name = predictive_model_weights,
    command = dplyr::bind_rows(weights_predictive_classification_LiblineaR_decade_dfm = weights_predictive_classification_LiblineaR_decade_dfm,
                               weights_predictive_regression_LiblineaR_decade_dfm = weights_predictive_regression_LiblineaR_decade_dfm,
                               weights_predictive_classification_xgboost_decade_dfm = weights_predictive_classification_xgboost_decade_dfm,
                               weights_predictive_regression_xgboost_decade_dfm = weights_predictive_regression_xgboost_decade_dfm,
                               .id = "id"),
    deployment = "main"

  ),

  tar_target(
    name = svd_model_weights,
    command = dplyr::bind_rows(sims_svd_word_vectors_decade_dfm = sims_svd_word_vectors_decade_dfm,
                               .id = "id"),
    deployment = "main"

  ),

  tar_target(
    name = ppmi_model_weights,
    command = dplyr::bind_rows(ppmi_single_decade_dfm = ppmi_single_decade_dfm,
                               .id = "id"),
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
    command = dplyr::bind_rows(regression_LiblineaR_decade_dfm_testing = performance_predictive_regression_LiblineaR_decade_dfm_testing,
                               regression_LiblineaR_decade_dfm_training = performance_predictive_regression_LiblineaR_decade_dfm_training,
                               regression_xgboost_decade_dfm_testing = performance_predictive_regression_xgboost_decade_dfm_testing,
                               regression_xgboost_decade_dfm_training = performance_predictive_regression_xgboost_decade_dfm_training,
                               classification_xgboost_decade_dfm_testing = performance_predictive_classification_xgboost_decade_dfm_testing,
                               classification_xgboost_decade_dfm_trainig = performance_predictive_classification_xgboost_decade_dfm_training,
                               classification_LiblineaR_decade_dfm_testing = performance_predictive_classification_LiblineaR_decade_dfm_testing,
                               classification_LiblineaR_decade_dfm_training = performance_predictive_classification_LiblineaR_decade_dfm_training),
    deployment = "main"

  ),

  tar_target(
    name = combined_weights,
    command = all_model_weights %>%
      dplyr::filter(word != "DEMOCRACY") %>%
      dplyr::group_by(id, decade) %>%
      dplyr::mutate(value = scale(value)) %>%
      dplyr::group_by(decade, word) %>%
      dplyr::summarise(mean = list(as_tibble_row(Hmisc::smean.cl.normal(value)))) %>%
      tidyr::unnest(mean) %>%
      dplyr::arrange(desc(Mean), .by_group = TRUE) %>%
      dplyr::rename(value = Mean, value_upper = Upper, value_lower = Lower) %>%
      dplyr::mutate(pos = stringr::str_extract(word, "(?<=_)[NVBJnvbj]{2}")) ,
    deployment = "main"

  ),

# Graphs ------------------------------------------------------------------

  tar_target(
    name = pos_patterns,
    command = c(".","_nn","_vb","_jj", "ism_"),
    deployment = "main"
  ),

  tar_target(name = max_per_decade,
             command = 8,
             deployment = "main"
             ),

  tar_target(name = max_num,
             command = 60,
             deployment = "main"
             ),

  tar_target(
    name = graph_combined_weights,
    command = graph_similarities(combined_weights %>%
                                   dplyr::filter(stringr::str_detect(word, pos_patterns)),
                                 top_n = max_per_decade,
                                 var = value,
                                 max_n = max_num),
    pattern = map(pos_patterns),
    iteration = "list",
    deployment = "main"
  ),

  tar_knit(
    name = graph_document,
    path = "graph_document.rmd",
    deployment = "main"
  ),

# PPMI single feature calculation --------------------------------------------------------

  tar_eval(
    values = list(sources = rlang::syms("decade_dfm"),
                  results = rlang::syms("ppmi_single_decade_dfm"),
                  source_names = "decade_dfm"),
    tar_target(
      name = results,
      command = feature_ppmi(sources, democracy_feature) %>%
        dplyr::mutate(decade = decades,
                      measure = "PPMI of 'DEMOCRACY' with other terms",
                      source = source_names) %>%
        dplyr::arrange(desc(value)),
      pattern = map(sources, decades),
      # resources = tar_resources(future = tar_resources_future(
      #   plan = future::tweak(future.batchtools::batchtools_slurm,
      #                        resources = list(partition = "quicktest", memory = "20G", ncpus = 2,
      #                                         walltime = "0:15:00")),
      #   resources = list(partition = "quicktest", memory = "20G", ncpus = 2,
      #                    walltime = "0:15:00"))),
      deployment = "main"

    )
  ),

# SVD word vector calculation ---------------------------------------------

  tar_eval(
    values = list(sources = rlang::syms("decade_dfm"),
                  results = rlang::syms("svd_word_vectors_decade_dfm")),
    tar_target(
      name = results,
      command = sources %>%
        quanteda::dfm_lookup(democracy_feature, exclusive = FALSE) %>%
        svd_word_vectors(nv = 50, weight = "ppmi"),
      pattern = map(sources),
      iteration = "list",
      resources = tar_resources(future = tar_resources_future(
        plan = future::tweak(future.batchtools::batchtools_slurm,
                             resources = list(partition = "quicktest", memory = "10G", ncpus = 10,
                                              walltime = "0:10:00")),
        resources = list(partition = "quicktest", memory = "10G", ncpus = 10,
                         walltime = "0:10:00")))

    )
  ),

# Similarity calculation for word vectors ---------------------------------

  tar_eval(
    values = tidyr::expand_grid(vectors = c("svd_word_vectors"),
                                sources = c("decade_dfm")) %>%
      tidyr::unite("sources", dplyr::everything()) %>%
      dplyr::mutate(results = paste("sims", sources, sep = "_"),
                    source_names = sources,
                    dplyr::across(c(sources, results), rlang::syms)),
    tar_target(
      name = results,
      command = wordVectors::closest_to(sources, "DEMOCRACY", n = Inf,
                                        fancy_names = FALSE) %>%
        dplyr::mutate(decade = decades,
                      dimensions = ncol(sources),
                      measure = "Cosine similarity to 'DEMOCRACY'",
                      source = source_names) %>%
        dplyr::rename(value = similarity) %>%
        tibble::as_tibble(),
      pattern = map(sources, decades),
      deployment = "main"
    )
  ),

  tar_eval(
    tar_target(name = graphs,
               command = graph_similarities(sources %>%
                                              dplyr::filter(word != "DEMOCRACY") %>%
                                              dplyr::group_by(decade) %>%
                                              dplyr::mutate(value = scale(value)) %>%
                                              dplyr::ungroup() %>%
                                              dplyr::filter(stringr::str_detect(word, pos_patterns)),
                                            top_n = max_per_decade,
                                            var = value,
                                            max_n = max_num),
               pattern = map(pos_patterns),
               iteration = "list",
               deployment = "main"),
    values = tibble::tibble(sources = c("ppmi_single_decade_dfm",
                                        "sims_svd_word_vectors_decade_dfm",
                                        "weights_predictive_regression_LiblineaR_decade_dfm",
                                        "weights_predictive_classification_LiblineaR_decade_dfm",
                                        "weights_predictive_regression_xgboost_decade_dfm",
                                        "weights_predictive_classification_xgboost_decade_dfm"),
                            graphs = paste("graph", sources, sep = "_")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), rlang::syms))
  )




)
