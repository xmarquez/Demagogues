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
    name = demagogue_worksets,
    command = workset_builder("demagogue", pub_date = decades:(decades+9)) %>%
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
    name = demagogue_worksets_meta,
    command = cached_hathi_catalog %>%
      dplyr::filter(htid %in% demagogue_worksets$htid),
    resources = tar_resources(future = tar_resources_future(
      plan = future::tweak(future.batchtools::batchtools_slurm,
                           resources = list(partition = "quicktest", memory = "20G", ncpus = 12,
                                            walltime = "1:00:00")),
      resources = list(partition = "quicktest", memory = "20G", ncpus = 12,
                       walltime = "1:00:00")))
  ),

  tar_target(
    name = demagogue_usable_htids,
    command = demagogue_worksets_meta %>%
      dplyr::left_join(demagogue_worksets) %>%
      dplyr::filter(rights_date_used >= decade, rights_date_used < decade+10,
                    decade == decades) %>%
      dplyr::mutate(rights_date_used2 = stringr::str_extract(imprint, "[0-9]{4}") %>%
                      as.double()) %>%
      dplyr::filter(rights_date_used2 <= rights_date_used,
                    lang == "eng"),
    pattern = map(decades),
    resources = tar_resources(future = tar_resources_future(
      plan = future::tweak(future.batchtools::batchtools_slurm,
                           resources = list(partition = "quicktest", memory = "4G", ncpus = 4,
                                            walltime = "0:20:00")),
      resources = list(partition = "quicktest", memory = "4G", ncpus = 4,
                       walltime = "0:20:00")))
    ),

  tar_target(
    name = demagogue_samples,
    command = demagogue_usable_htids %>%
      dplyr::sample_n(min(200, dplyr::n()), weight = n),
    pattern = map(demagogue_usable_htids),
    deployment = "main"
  ),

  tar_target(
    name = demagogue_files,
    command = hathiTools::cache_htids(demagogue_samples, attempt_rsync = TRUE,
                                      cache_format = "rds"),
    pattern = map(demagogue_samples),
    resources = tar_resources(future = tar_resources_future(
      plan = future::tweak(future.batchtools::batchtools_slurm,
                           resources = list(partition = "quicktest", memory = "4G", ncpus = 4,
                                            walltime = "0:40:00")),
      resources = list(partition = "quicktest", memory = "4G", ncpus = 4,
                       walltime = "0:40:00")))
  ),

  tar_target(
    name = decade_dfm,
    command = compute_dfm(demagogue_files, cache_format = "rds"),
    pattern = map(demagogue_files),
    resources = tar_resources(future = tar_resources_future(
      plan = future::tweak(future.batchtools::batchtools_slurm,
                           resources = list(partition = "quicktest", memory = "8G", ncpus = 4,
                                            walltime = "0:20:00")),
      resources = list(partition = "quicktest", memory = "8G", ncpus = 4,
                       walltime = "0:20:00"))),
    iteration = "list"
  ),

  tar_target(
      name = splits_decade_dfm,
      command = train_test_splits(decade_dfm, "demagogue_nn"),
      pattern = map(decade_dfm),
      resources = tar_resources(future = tar_resources_future(
        plan = future::tweak(future.batchtools::batchtools_slurm,
                             resources = list(partition = "quicktest", memory = "2G", ncpus = 2,
                                              walltime = "0:05:00")),
        resources = list(partition = "quicktest", memory = "2G", ncpus = 2,
                         walltime = "0:05:00"))),
      iteration = "list"
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
                                 feat = "demagogue_nn",
                                 engine = engine,
                                 model_type = model_type),
      pattern = map(sources, splits),
      packages = c("quanteda"),
      resources = tar_resources(future = tar_resources_future(
        plan = future::tweak(future.batchtools::batchtools_slurm,
                             resources = list(partition = "quicktest", memory = "10G", ncpus = 10,
                                              walltime = "0:20:00")),
        resources = list(partition = "quicktest", memory = "10G", ncpus = 10,
                         walltime = "0:20:00"))),
      iteration = "list"
    )
  ),

# Predictive model coefficient extraction ---------------------------------

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
      resources = tar_resources(future = tar_resources_future(
        plan = future::tweak(future.batchtools::batchtools_slurm,
                             resources = list(partition = "quicktest", memory = "1G", ncpus = 2,
                                              walltime = "0:05:00")),
        resources = list(partition = "quicktest", memory = "1G", ncpus = 2,
                         walltime = "0:05:00"))),
      packages = c("quanteda")
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
      command = model_performance(sources, dfms, splits, feat = "demagogue_nn", use = use) %>%
        dplyr::mutate(source = source_names,
                      decade = decades,
                      model_type = model_type,
                      sample = use),
      pattern = map(sources, dfms, splits, decades),
      resources = tar_resources(future = tar_resources_future(
        plan = future::tweak(future.batchtools::batchtools_slurm,
                             resources = list(partition = "quicktest", memory = "10G", ncpus = 10,
                                              walltime = "0:20:00")),
        resources = list(partition = "quicktest", memory = "10G", ncpus = 10,
                         walltime = "0:20:00"))),
      packages = c("quanteda")
    )
  ),

  tar_target(
    name = predictive_model_weights,
    command = dplyr::bind_rows(weights_predictive_classification_LiblineaR_decade_dfm = weights_predictive_classification_LiblineaR_decade_dfm,
                               weights_predictive_classification_LiblineaR_sampled_decade_dfm = weights_predictive_classification_LiblineaR_sampled_decade_dfm,
                               weights_predictive_regression_LiblineaR_decade_dfm = weights_predictive_regression_LiblineaR_decade_dfm,
                               weights_predictive_regression_LiblineaR_sampled_decade_dfm = weights_predictive_regression_LiblineaR_sampled_decade_dfm,
                               weights_predictive_classification_xgboost_decade_dfm = weights_predictive_classification_xgboost_decade_dfm,
                               weights_predictive_classification_xgboost_sampled_decade_dfm = weights_predictive_classification_xgboost_sampled_decade_dfm,
                               weights_predictive_regression_xgboost_decade_dfm = weights_predictive_regression_xgboost_decade_dfm,
                               weights_predictive_regression_xgboost_sampled_decade_dfm = weights_predictive_regression_xgboost_sampled_decade_dfm,
                               .id = "id"),
    deployment = "main"

  )

)
