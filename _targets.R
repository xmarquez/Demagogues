# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tidyverse)
library(hathiTools)
library(tarchetypes) # Load other packages as needed. # nolint

# Resources for cluster ---------------------------------------------------------------------

democracy_files_resources <- list(partition = "parallel", memory = "10G", ncpus = 2,
                                  walltime = "2:00:00")
dfm_resources <- list(partition = "parallel", memory = "30G", ncpus = 2,
                      walltime = "0:40:00")
predictive_model_resources <- list(partition = "parallel", memory = "15G",
                                   ncpus = 10, walltime = "0:40:00")
evaluation_model_resources <- list(partition = "parallel", memory = "15G",
                                   ncpus = 2, walltime = "0:02:00")
svd_word_vectors_resources <- list(partition = "parallel", memory = "25G", ncpus = 10,
                                   walltime = "0:10:00")

# Object parameters ----------------------------------------------------------------------------------

dfms_df <- tibble::tibble(result = "decade_dfm") %>%
  dplyr::mutate(across(dplyr::any_of(c("result", "sources", "split")), rlang::syms))

splits <- tidyr::nesting(prefix = "splits",
                         sources = dfms_df$result,
                         downsample = c(FALSE, TRUE, TRUE),
                         type = c(NA, "random", "similarity")) %>%
  tidyr::unite(col = "result", prefix, sources, type, remove = FALSE, na.rm = TRUE) %>%
  dplyr::mutate(across(dplyr::any_of(c("result", "sources", "split")), rlang::syms))

models_df <- tidyr::nesting(prefix = "predictive",
                         model_type = "classification",
                         sources = dfms_df$result,
                         engine = c("LiblineaR", "xgboost", "glmnet")) %>%
  tidyr::expand_grid(tidyr::nesting(split = splits$result,
                                    split_type = splits$type)) %>%
  tidyr::unite(col = "result", prefix, model_type, sources, engine, split_type, remove = FALSE, na.rm = TRUE) %>%
  dplyr::mutate(across(dplyr::any_of(c("result", "sources", "split")), rlang::syms))

model_weights_df <- tidyr::nesting(prefix = "weights",
                                sources = models_df$result,
                                source_names = as.character(sources))  %>%
  tidyr::unite(col = "result", prefix, sources, remove = FALSE, na.rm = TRUE) %>%
  dplyr::mutate(across(dplyr::any_of(c("result", "sources", "split")), rlang::syms))

model_performance_df <- tidyr::nesting(prefix = "performance",
                                    sources = models_df$result,
                                    source_names = as.character(sources),
                                    dfms = models_df$sources,
                                    split = models_df$split) %>%
  tidyr::expand_grid(use = c("testing", "training")) %>%
  tidyr::unite(col = "result", prefix, sources, use, remove = FALSE, na.rm = TRUE) %>%
  dplyr::mutate(across(dplyr::any_of(c("result", "sources", "split")), rlang::syms))

svd_word_vectors_df <- tidyr::nesting(prefix = "svd_word_vectors",
                                   sources = dfms_df$result,
                                   dims = 50) %>%
  tidyr::unite(col = "result", prefix, sources, dims, remove = FALSE, na.rm = TRUE) %>%
  dplyr::mutate(across(dplyr::any_of(c("result", "sources", "split")), rlang::syms))

sims_svd_word_vectors_df <- tidyr::nesting(prefix = "sims",
                                        sources = svd_word_vectors_df$result,
                                        source_names = as.character(sources)) %>%
  tidyr::unite(col = "result", prefix, sources, remove = FALSE, na.rm = TRUE) %>%
  dplyr::mutate(across(dplyr::any_of(c("result", "sources", "split")), rlang::syms))

ppmi_word_vectors_df <- tidyr::nesting(prefix = c("ppmi_single", "sims_ppmi"),
                                    sources = dfms_df$result,
                                    funs = c("feature_ppmi", "ppmi_similarities"),
                                    source_names = as.character(sources)) %>%
  tidyr::unite(col = "result", prefix, sources, remove = FALSE, na.rm = TRUE) %>%
  dplyr::mutate(across(dplyr::any_of(c("result", "sources", "split", "funs")), rlang::syms))

graphs_df <- dplyr::bind_rows(sims_svd_word_vectors_df,
                           model_weights_df,
                           ppmi_word_vectors_df) %>%
  dplyr::mutate(prefix = "graph",
                sources = .$result) %>%
  tidyr::unite(col = "result", prefix, sources, remove = FALSE, na.rm = TRUE) %>%
  dplyr::mutate(across(dplyr::any_of(c("result", "sources", "split")), rlang::syms))

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
    deployment = "main"
    ),

  tar_target(
    name = democracy_samples,
    command = democracy_usable_htids %>%
      dplyr::sample_n(min(500, dplyr::n()), weight = n),
    pattern = map(democracy_usable_htids),
    deployment = "main"
  ),

# File caching -------------------------------------------------------------

  tar_target(
    name = democracy_files,
    command = hathiTools::cache_htids(democracy_samples, attempt_rsync = TRUE,
                                      cache_format = "rds"),
    pattern = map(democracy_samples),
    resources = tar_resources(future = tar_resources_future(
      plan = future::tweak(future.batchtools::batchtools_slurm,
                           resources = democracy_files_resources),
      resources = democracy_files_resources)),
    storage = "worker",
    retrieval = "worker"

  ),

# DFM creation -------------------------------------------------------------

  tar_target(
    name = decade_dfm,
    command = compute_dfm(democracy_files, cache_format = "rds"),
    pattern = map(democracy_files),
    resources = tar_resources(future = tar_resources_future(
      plan = future::tweak(future.batchtools::batchtools_slurm,
                           resources = dfm_resources),
      resources = dfm_resources)),
    storage = "worker",
    retrieval = "worker",
    iteration = "list"
  ),

  tar_target(
    name = democracy_feature,
    command = quanteda::dictionary(list(democracy = c("democracy_nn", "democracy_nnp", "democracy_nns")),
                                   tolower = FALSE),
    deployment = "main"
  ),

# Compute test-train splits -----------------------------------------

  tar_eval(
    values = splits,
    tar_target(
      name = result,
      command = train_test_splits(sources,
                                  feat = democracy_feature,
                                  downsample = downsample,
                                  type = type),
      pattern = map(sources),
      packages = c("quanteda"),
      iteration = "list",
      deployment = "main"
    )
  ),

# Predictive models -------------------------------------------------------

  tar_eval(
    values = models_df,
    tar_target(
      name = result,
      command = predictive_model(dfm = sources,
                                 initial_split = split,
                                 feat = democracy_feature,
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
      iteration = "list"
    )
  ),

# Predictive model weight extraction ---------------------------------

  tar_eval(
    values = model_weights_df,
    tar_target(
      name = result,
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
    values = model_performance_df,
    tar_target(
      name = result,
      command = model_performance(sources, dfms, split, feat = democracy_feature, use = use) %>%
        dplyr::mutate(source = source_names,
                      decade = decades,
                      model_type = model_type,
                      sample = use),
      pattern = map(sources, dfms, split, decades),
      packages = c("quanteda"),
      resources = tar_resources(future = tar_resources_future(
        plan = future::tweak(future.batchtools::batchtools_slurm,
                             resources = evaluation_model_resources),
        resources = evaluation_model_resources)),
      storage = "worker",
      retrieval = "worker"
    )
  ),


# Graphs ------------------------------------------------------------------

  tar_target(
    name = pos_patterns,
    command = c(".","(_nn|_NN)","(_vb|_VB)","(_jj|_JJ)", "ism_", "^[A-Z].+NN"),
    deployment = "main"
  ),

  tar_target(
    name = lowercase,
    command = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
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
                                 max_n = max_num,
                                 lowercase = lowercase),
    pattern = map(pos_patterns, lowercase),
    iteration = "list",
    deployment = "main"
  ),

  tar_knit(
    name = graph_document,
    path = "graph_document.rmd",
    deployment = "main"
  ),

  tar_eval(
    values = graphs_df,
    tar_target(name = result,
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
               deployment = "main"
    )
  ),

# PPMI sims and WVs --------------------------------------------------------

  tar_eval(
    values = ppmi_word_vectors_df,
    tar_target(
      name = result,
      command = funs(sources, democracy_feature) %>%
        dplyr::mutate(decade = decades,
                      measure = "PPMI of 'DEMOCRACY' with other terms",
                      source = source_names) %>%
        dplyr::arrange(desc(value)),
      pattern = map(sources, decades),
      deployment = "main"
    )
  ),

# SVD word vector calculation ---------------------------------------------

  tar_eval(
    values = svd_word_vectors_df,
    tar_target(
      name = result,
      command = sources %>%
        quanteda::dfm_lookup(democracy_feature, exclusive = FALSE) %>%
        svd_word_vectors(nv = dims, weight = "ppmi"),
      pattern = map(sources),
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
    values = sims_svd_word_vectors_df,
    tar_target(
      name = result,
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
  # Combined Targets ------------------------------------------------------------------------

  tar_target(
    name = predictive_model_weights,
    command = dplyr::bind_rows(!!!model_weights_df$result, .id = "id") %>%
      dplyr::mutate(id = as.numeric(id),
                    id = as.character(model_weights_df$result[id])),
    deployment = "main"

  ),

  tar_target(
    name = svd_model_weights,
    command = dplyr::bind_rows(!!!sims_svd_word_vectors_df$result, .id = "id") %>%
      dplyr::mutate(id = as.numeric(id),
                    id = as.character(model_weights_df$result[id])),
    deployment = "main"

  ),

  tar_target(
    name = ppmi_model_weights,
    command = dplyr::bind_rows(!!!ppmi_word_vectors_df$result, .id = "id") %>%
      dplyr::mutate(id = as.numeric(id),
                    id = as.character(model_weights_df$result[id])),
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
    command = dplyr::bind_rows(!!!model_performance_df$result),
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

  )
)


