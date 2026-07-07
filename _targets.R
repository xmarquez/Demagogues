# Created by use_targets().
# Updated to use {crew} and profile-aware parameter tables.

library(targets)
library(tarchetypes)
library(crew)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)
library(quanteda)
library(hathiTools)
library(here)

research_data_root <- normalizePath(
  Sys.getenv("RESEARCH_DATA_ROOT", "D:/ResearchData/corpora"),
  winslash = "/",
  mustWork = FALSE
)
hathi_data_root <- file.path(research_data_root, "hathi")

options(
  hathiTools.ef.dir = file.path(hathi_data_root, "hathi-ef")
)

# Cluster resource definitions (defines build_demagogues_controller() and the
# crew controller group used below; selects Slurm vs local via TARGETS_BACKEND).
source("cluster_resources.R")

tar_option_set(
  packages = c(
    "tibble", "magrittr", "dplyr", "stringr", "purrr", "tidyr",
    "quanteda", "hathiTools", "here", "rlang"
  ),
  format = "rds",
  # Controller group with controllers named "std" and "bigmem" (both backends).
  controller = build_demagogues_controller(),
  # Default routing: any crew-deployed target with no explicit controller runs
  # on "std". Heavy DFM builds override this to "bigmem" (see below).
  resources = tar_resources(
    crew = tar_resources_crew(controller = "std")
  )
)

# Custom functions in R/ are required by targets.
tar_source()

# Parameter tables and target names (run/profile-aware).
source("object_parameters.R")
# Parameter tables (feature_df, dfm_df, predictive_model_df, etc.) are created
# by object_parameters.R from the split config files and are available directly.

# Ingest and sampling ---------------------------------------------------------
ingest_targets <- list(
  tar_eval(
    values = feature_df,
    tar_target(
      name = feature_object,
      command = feature_list |>
        set_names(feature_name) |>
        quanteda::dictionary(tolower = FALSE),
      deployment = "main",
      description = "Ingest: Quanteda dictionary for the target feature."
    )
  ),
  tar_eval(
    values = period_params,
    tar_target(
      name = period_object,
      command = seq(
        sample_min_year,
        sample_max_year,
        by = sample_slice_size
      ),
      deployment = "main",
      description = "Ingest: Sequence of period boundaries for slicing the corpus."
    )
  ),
  tar_target(
    name = hathi_catalog,
    command = file.path(
      hathi_data_root,
      "raw-hathifiles",
      "hathi_full_20230101.txt.gz"
    ),
    format = "file",
    deployment = "main",
    description = "Ingest: Path to the compressed HathiTrust catalog snapshot."
  ),
  tar_eval(
    values = workset_df,
    tar_target(
      name = workset_object,
      command = workset_builder_resilient(
        feature_search_terms,
        token_join = feature_search_join,
        pub_date = period_object:(period_object + sample_slice_size - 1),
        method = workset_method
      ) %>%
        mutate(period = period_object),
      pattern = map(period_object),
      deployment = "main",
      description = "Ingest: Workset of HTIDs for the feature and period slice."
    )
  ),
  tar_target(
    name = cached_hathi_catalog,
    command = hathiTools::load_raw_hathifile(hathi_catalog),
    deployment = "main",
    description = "Ingest: Cached tibble of the raw Hathi catalog for downstream filtering."
  ),
  tar_eval(
    values = workset_meta_df,
    tar_target(
      name = workset_meta_object,
      command = cached_hathi_catalog %>%
        {
          if (identical(corpus_id, "corpus_hathi_marxism")) {
            dplyr::filter(
              .,
              stringr::str_detect(author, "Marx|Engels|Lenin|Mao|Kautsky|Trotsky|Stalin") |
                stringr::str_detect(title, "Marx|marx")
            )
          } else {
            .
          }
        } %>%
        filter(!!corpus_filter) %>%
        filter(htid %in% workset_object$htid) %>%
        left_join(workset_object, by = "htid", suffix = c("", "_workset")) %>%
        filter(
          rights_date_used >= period,
          rights_date_used < period + sample_slice_size
        ) %>%
        mutate(
          rights_date_used2 = str_extract(imprint, "[0-9]{4}") %>%
            as.double()
        ) %>%
        # Volumes whose `imprint` lacks a parseable 4-digit year yield NA in
        # `rights_date_used2` and are intentionally excluded from the corpus.
        filter(
          !is.na(rights_date_used2),
          rights_date_used2 <= rights_date_used,
          lang == "eng"
        ),
      deployment = "main",
      description = "Ingest: Workset metadata joined and filtered for all period slices."
    )
  ),
  tar_eval(
    values = workset_count_df,
    tar_target(
      name = workset_volume_count_object,
      command = nrow(workset_object),
      deployment = "main",
      description = "Ingest: Total number of volumes in the feature workset (all periods)."
    )
  ),
  tar_eval(
    values = workset_meta_count_df,
    tar_target(
      name = workset_meta_volume_count_object,
      command = workset_meta_object |>
        dplyr::filter(lang == "eng") |>
        dplyr::distinct(htid) |>
        nrow(),
      deployment = "main",
      description = "Ingest: Distinct English-language volumes in the feature workset metadata."
    )
  ),
  tar_eval(
    values = sample_df,
    tar_target(
      name = sample_object,
      command = workset_meta_object %>%
        {
          # Dedupe within period only: reprints/editions in *other* decades still
          # count, preserving the cross-time popularity signal.
          if (identical(sample_dedupe, "bib_key")) {
            dplyr::slice_sample(dplyr::group_by(., period, ht_bib_key), n = 1) %>%
              dplyr::ungroup()
          } else if (identical(sample_dedupe, "author_title")) {
            dplyr::slice_sample(
              dplyr::group_by(., period, .author_title = paste(author, title)),
              n = 1
            ) %>%
              dplyr::ungroup() %>%
              dplyr::select(-.author_title)
          } else {
            .
          }
        } %>%
        dplyr::group_by(period) %>%
        dplyr::sample_n(min(sample_max_vols, dplyr::n())),
      deployment = "main",
      description = "Ingest: Sample of volumes per period respecting the max volume cap (optional dedupe)."
    )
  ),
  tar_target(
    name = info_sample,
    command = sample_df,
    deployment = "main",
    description = "Ingest: Parameter grid for sampling volumes across corpora and periods."
  ),
  tar_target(
    name = sampled_htids,
    # Reproducibility export (P4): the exact HTIDs drawn by every sample target,
    # one row per volume x sample, so the corpus sample can be published as
    # supplementary material. The !!! splice makes every sample_object a tracked
    # dependency; sample_df supplies the identifying metadata at run time.
    command = purrr::map(
      rlang::set_names(list(!!!sample_df$sample_object), sample_df$sample_id),
      ~ if (is.null(.x)) NULL else dplyr::ungroup(.x) %>% dplyr::select(htid, period)
    ) %>%
      dplyr::bind_rows(.id = "sample_id") %>%
      dplyr::left_join(
        sample_df %>%
          dplyr::select(sample_id, sample_rep, sample_type, sample_max_vols, feature_name),
        by = "sample_id"
      ) %>%
      dplyr::select(htid, period, sample_id, sample_rep, sample_type, sample_max_vols, feature_name),
    deployment = "main",
    description = "Ingest: HTIDs drawn by every sample target (reproducibility export)."
  ),
  tar_eval(
    values = files_df,
    tar_target(
      name = files_object,
      command = cache_ef_files(
        sample_object %>%
          filter(period == period_object)
      ),
      pattern = map(period_object),
      # Default crew controller ("std"). NOTE: EF downloads need outbound network
      # and the shared hathi-ef cache; on the cluster, pre-stage the cache to
      # scratch (see HPC.md) so workers only read local files.
      storage = "worker",
      retrieval = "worker",
      format = "file",
      # A failed branch/run is recorded in tar_meta() and becomes NULL rather
      # than aborting the whole pipeline.
      error = "null",
      description = "Ingest: Download and cache EF JSON files for the sampled volumes."
    )
  )
)

# DFM and splits -------------------------------------------------------------
dfm_targets <- list(
  tar_eval(
    values = dfm_df,
    tar_target(
      name = dfm_object,
      command = restricted_dfm_from_json(
        files_object,
        vocab_size = dfm_vocab_size,
        pos_pattern = dfm_feature_pos_pattern,
        include_pattern = dfm_feature_include_pattern,
        min_length = dfm_feature_min_length,
        page_language = dfm_page_language,
        pages_contain = dfm_pages_contain,
        min_sentence_count = dfm_page_min_sentence_count,
        to_lower = dfm_to_lower,
        multiplier = sample_multiplier
      ),
      pattern = map(files_object),
      packages = c("quanteda"),
      resources = tar_resources(crew = tar_resources_crew(controller = "bigmem")),
      iteration = "list",
      memory = "transient",
      garbage_collection = TRUE,
      # One empty/failed branch must not abort a multi-day run; errors are
      # recorded in tar_meta() and the branch value becomes NULL.
      error = "null",
      description = "DFM: Build restricted DFMs per sample with POS and regex filters."
    )
  ),
  tar_eval(
    values = reduced_dfm_df,
    tar_target(
      name = reduced_dfm_object,
      command = reduce_dfm(dfm_object, weight_pred_object, feature_object, reduced_dfm_vocab_per_period),
      packages = c("quanteda"),
      resources = tar_resources(crew = tar_resources_crew(controller = "bigmem")),
      memory = "transient",
      garbage_collection = TRUE,
      # One empty/failed branch must not abort a multi-day run; errors are
      # recorded in tar_meta() and the branch value becomes NULL.
      error = "null",
      description = "DFM: Reduce DFMs to top features per period for topic modeling."
    )
  ),
  tar_eval(
    values = split_df,
    tar_target(
      name = split_object,
      command = train_test_splits(
        dfm_object,
        feat = feature_object,
        downsampled = split_downsample,
        type = split_downsample_type
      ),
      pattern = map(dfm_object),
      packages = c("quanteda"),
      iteration = "list",
      memory = "transient",
      garbage_collection = TRUE,
      deployment = "main",
      # One empty/failed branch must not abort a multi-day run; errors are
      # recorded in tar_meta() and the branch value becomes NULL.
      error = "null",
      description = "DFM: Train/test splits (with optional downsampling) for each DFM."
    )
  )
)

# Models and downstream weights ----------------------------------------------
model_targets <- list(
  tar_eval(
    values = predictive_model_df,
    tar_target(
      name = predictive_model_object,
      command = predictive_model(
        dfm = dfm_object,
        weight = predictive_model_dfm_weight,
        initial_split = split_object,
        feat = feature_object,
        engine = predictive_model_engine,
        model_type = predictive_model_task,
        params = predictive_model_params
      ),
      pattern = map(dfm_object, split_object),
      packages = c("quanteda"),
      iteration = "list",
      # One empty/failed branch must not abort a multi-day run; errors are
      # recorded in tar_meta() and the branch value becomes NULL.
      error = "null",
      description = "Model: Fit predictive models per DFM with specified engine and weighting."
    )
  ),
  tar_eval(
    values = topic_model_df,
    tar_target(
      name = topic_model_object,
      command = stm::stm(reduced_dfm_object, K = topic_model_K),
      packages = c("quanteda"),
      resources = tar_resources(crew = tar_resources_crew(controller = "std")),
      storage = "worker",
      retrieval = "worker",
      garbage_collection = TRUE,
      # One empty/failed branch must not abort a multi-day run; errors are
      # recorded in tar_meta() and the branch value becomes NULL.
      error = "null",
      description = "Model: Fit STM topic models on reduced DFMs."
    )
  ),
  tar_target(
    name = info_topic_models,
    command = topic_model_df,
    deployment = "main",
    description = "Model Info: Parameter grid for topic model runs."
  ),
  tar_eval(
    values = predictive_model_weights_df,
    tar_target(
      name = weight_pred_object,
      command = model_weights(predictive_model_object, method = weight_normalization_method) %>%
        dplyr::mutate(weight_id = weight_pred_id, period = period_object),
      pattern = map(predictive_model_object, period_object),
      deployment = "main",
      packages = "quanteda",
      # One empty/failed branch must not abort a multi-day run; errors are
      # recorded in tar_meta() and the branch value becomes NULL.
      error = "null",
      description = "Model Weights: Extract model weights for predictive models."
    )
  ),
  tar_target(
    name = info_predictive_model_weights,
    command = predictive_model_weights_df |>
      mutate(
        workset_name = case_when(
          str_detect(workset_meta_id, "fiction") ~ "Fiction sample",
          str_detect(workset_meta_id, "marx") ~ "Marxist texts sample",
          TRUE ~ "All volumes"
        ),
        sample_strategy = case_when(
          sample_type == "restricted" ~ "Balanced (majority category undersampled)",
          TRUE ~ "Unbalanced"
        )
      ),
    deployment = "main",
    description = "Model Info: Metadata describing each predictive model weight run."
  ),
  tar_eval(
    values = performance_standard_df,
    tar_target(
      name = performance_object,
      command = model_performance(
        predictive_model_object,
        dfm_object,
        split_object,
        feat = feature_object,
        weight = predictive_model_dfm_weight,
        use = performance_split
      ) %>%
        dplyr::mutate(performance_id = performance_id, period = period_object),
      pattern = map(predictive_model_object, dfm_object, split_object, period_object),
      packages = c("quanteda"),
      resources = tar_resources(crew = tar_resources_crew(controller = "std")),
      storage = "worker",
      retrieval = "worker",
      error = "null",
      description = "Model Performance: In-sample performance metrics (training/testing split)."
    )
  ),
  tar_eval(
    values = performance_wild_sample_df,
    tar_target(
      name = performance_object,
      command = model_performance(
        predictive_model_object,
        testing_dfm_object,
        split_object,
        feat = feature_object,
        weight = predictive_model_dfm_weight,
        use = "testing - OOD",
        reference_dfm = get_training_sample(dfm_object, split_object)
      ) %>%
        dplyr::mutate(performance_id = performance_id, period = period_object),
      pattern = map(predictive_model_object, testing_dfm_object, dfm_object, split_object, period_object),
      packages = c("quanteda"),
      # OOD evaluates on the unrestricted DFM (up to ~150k pages x 30k features
      # for recent decades); route to bigmem so the largest evaluations have
      # headroom. (Resources are not part of a target's hash, so this does not
      # invalidate already-built OOD targets - only errored ones retry here.)
      resources = tar_resources(crew = tar_resources_crew(controller = "bigmem")),
      storage = "worker",
      retrieval = "worker",
      error = "null",
      description = "Model Performance: Out-of-distribution performance on the unrestricted DFM."
    )
  ),
  tar_eval(
    values = ppmi_wv_df,
    tar_target(
      name = weight_ppmi_object,
      command = ppmi_fun(dfm_object, feature_object) %>%
        dplyr::filter(word != stringr::str_to_upper(feature_name)) %>%
        dplyr::mutate(
          period = period_object,
          scaled_value = as.numeric(scale(value)),
          normalized_value = normalize_weights(value, method = weight_normalization_method),
          weight_id = weight_ppmi_id
        ) %>%
        dplyr::arrange(desc(value)),
      pattern = map(dfm_object, period_object),
      resources = tar_resources(crew = tar_resources_crew(controller = "std")),
      storage = "worker",
      retrieval = "worker",
      # One empty/failed branch must not abort a multi-day run; errors are
      # recorded in tar_meta() and the branch value becomes NULL.
      error = "null",
      description = "Model Weights: PPMI-based similarities for each feature and period."
    )
  ),
  tar_target(
    name = info_ppmi_weights,
    command = ppmi_wv_df |>
      mutate(
        workset_name = case_when(
          str_detect(workset_meta_id, "fiction") ~ "Fiction sample",
          str_detect(workset_meta_id, "marx") ~ "Marxist texts sample",
          TRUE ~ "All volumes"
        ),
        sample_strategy = case_when(
          sample_type == "restricted" ~ "Balanced (majority category undersampled)",
          TRUE ~ "Unbalanced"
        ),
        predictive_model_engine = ppmi_fun |> str_remove("_")
      ),
    deployment = "main",
    description = "Model Info: Metadata for PPMI similarity runs."
  ),
  tar_eval(
    values = svd_df,
    tar_target(
      name = svd_object,
      command = dfm_object %>%
        quanteda::dfm_lookup(feature_object, exclusive = FALSE) %>%
        svd_word_vectors(
          initial_split = split_object,
          nv = svd_dims,
          weight = svd_weight
        ),
      pattern = map(dfm_object, split_object),
      iteration = "list",
      resources = tar_resources(crew = tar_resources_crew(controller = "std")),
      storage = "worker",
      retrieval = "worker",
      # One empty/failed branch must not abort a multi-day run; errors are
      # recorded in tar_meta() and the branch value becomes NULL.
      error = "null",
      description = "Model: Compute SVD word vectors for reduced feature lookups."
    )
  ),
  tar_eval(
    values = sims_svd_df,
    tar_target(
      name = weight_svd_object,
      command = wordVectors::closest_to(
        svd_object,
        stringr::str_to_upper(feature_name),
        n = Inf,
        fancy_names = FALSE
      ) %>%
        dplyr::filter(word != stringr::str_to_upper(feature_name)) %>%
        dplyr::mutate(
          period = period_object,
          scaled_value = as.numeric(scale(similarity)),
          normalized_value = normalize_weights(similarity, method = weight_normalization_method),
          weight_id = weight_svd_id
        ) %>%
        dplyr::rename(value = similarity) %>%
        tibble::as_tibble(),
      pattern = map(svd_object, period_object),
      deployment = "main",
      # One empty/failed branch must not abort a multi-day run; errors are
      # recorded in tar_meta() and the branch value becomes NULL.
      error = "null",
      description = "Model Weights: Nearest-neighbor similarities derived from SVD word vectors."
    )
  ),
  tar_target(
    name = info_svd_weights,
    command = sims_svd_df |>
      mutate(
        workset_name = case_when(
          str_detect(workset_meta_id, "fiction") ~ "Fiction sample",
          str_detect(workset_meta_id, "marx") ~ "Marxist texts sample",
          TRUE ~ "All volumes"
        ),
        sample_strategy = case_when(
          sample_type == "restricted" ~ "Balanced (majority category undersampled)",
          TRUE ~ "Unbalanced"
        ),
        predictive_model_engine = "SVD word vectors"
      ),
    deployment = "main",
    description = "Model Info: Metadata for SVD similarity runs."
  ),
  tar_eval(
    values = kl_df,
    tar_target(
      name = kl_object,
      command = kl_simple(weight_object, "period", common_vocab_only = kl_common_vocab) %>%
        kl_matrix_to_df() %>%
        dplyr::mutate(kl_id = kl_id),
      deployment = "main",
      # A failed branch/run is recorded in tar_meta() and becomes NULL rather
      # than aborting the whole pipeline.
      error = "null",
      description = "Model: KL divergences of weight distributions across periods."
    )
  ),
  tar_eval(
    values = entropy_df,
    tar_target(
      name = entropy_object,
      command = entropy(weight_object) |>
        dplyr::mutate(entropy_id = entropy_id),
      deployment = "main",
      # A failed branch/run is recorded in tar_meta() and becomes NULL rather
      # than aborting the whole pipeline.
      error = "null",
      description = "Model Weights: Entropy of weight distributions across periods."
    )
  ),
  tar_target(
    name = predictive_model_weights,
    command = dplyr::bind_rows(!!!predictive_model_weights_df$weight_pred_object),
    deployment = "main",
    description = "Model Weights: All predictive model weights combined into one table."
  ),
  tar_target(
    name = svd_model_weights,
    command = dplyr::bind_rows(!!!sims_svd_df$weight_svd_object),
    deployment = "main",
    description = "Model Weights: All SVD similarity weights combined."
  ),
  tar_target(
    name = ppmi_model_weights,
    command = dplyr::bind_rows(!!!ppmi_wv_df$weight_ppmi_object),
    deployment = "main",
    description = "Model Weights: All PPMI similarity weights combined."
  ),
  tar_target(
    name = all_model_weights,
    command = dplyr::bind_rows(
      svd_model_weights,
      ppmi_model_weights,
      predictive_model_weights
    ),
    deployment = "main",
    description = "Model Weights: Unified table of all model weight outputs."
  ),
  tar_target(
    name = combined_performance,
    command = dplyr::bind_rows(!!!performance_predictive_model_df$performance_object),
    deployment = "main",
    description = "Model Performance: All performance metrics combined."
  ),
  tar_target(
    name = combined_kl,
    command = dplyr::bind_rows(!!!kl_df$kl_object) %>%
      left_join(kl_df),
    deployment = "main",
    description = "Model: KL results merged with run metadata."
  ),
  tar_target(
    name = combined_entropy,
    command = dplyr::bind_rows(!!!entropy_df$entropy_object) %>%
      left_join(entropy_df),
    deployment = "main",
    description = "Model: Entropy results merged with run metadata."
  ),
  tar_target(
    name = info_performance,
    command = performance_predictive_model_df |>
      mutate(
        workset_name = case_when(
          str_detect(workset_meta_id, "fiction") ~ "Fiction sample",
          str_detect(workset_meta_id, "marx") ~ "Marxist texts sample",
          TRUE ~ "All volumes"
        ),
        sample_strategy = case_when(
          sample_type == "restricted" ~ "Balanced (majority category undersampled)",
          TRUE ~ "Unbalanced"
        )
      ),
    deployment = "main",
    description = "Model Info: Metadata describing each performance evaluation."
  ),
  tar_target(
    name = info_all_model_weights,
    command = dplyr::bind_rows(
      info_predictive_model_weights,
      info_ppmi_weights,
      info_svd_weights
    ),
    deployment = "main",
    description = "Model Info: Metadata for all weight runs across engines."
  ),
  tar_target(
    name = sample_sizes,
    command = combined_performance |>
      distinct(period, performance_id, conf_mat) |>
      rowwise() |>
      mutate(
        sample_size = sum(conf_mat$table),
        negative = sum(conf_mat$table[, 1]),
        positive = sum(conf_mat$table[, 2]),
        ratio = positive / negative
      ) |>
      ungroup() |>
      select(-conf_mat) |>
      left_join(info_performance),
    deployment = "main",
    description = "Model: Sample size summaries derived from confusion matrices."
  ),
  tar_target(
    name = weight_correlations,
    command = correlate_weights_by_period(
      all_model_weights,
      info_all_model_weights
    ),
    deployment = "main",
    description = "Model: Correlate weights across periods and models."
  ),
  tar_target(
    name = rank_agreements,
    command = rank_agreement_by_period(
      all_model_weights,
      info_all_model_weights
    ),
    deployment = "main",
    description = "Model: Rank-based cross-model agreement (Spearman + top-n overlap) by period."
  ),
  tar_target(
    name = jsd_distances,
    command = jsd_weights_by_period(
      all_model_weights,
      info_all_model_weights
    ),
    deployment = "main",
    description = "Model: Jensen-Shannon distances between weight distributions across periods."
  )
)

# Graphs ---------------------------------------------------------------------
graph_targets <- list(
  tar_eval(
    values = graph_df,
    tar_target(
      name = graph_object,
      command = graph_similarities(
        weight_object %>%
          dplyr::filter(stringr::str_detect(word, graph_pos_patterns)),
        top_n = graph_max_per_slice,
        var = value,
        max_n = graph_max_terms,
        collapse_cased = graph_collapse_cased
      ) +
        ggplot2::labs(title = stringr::str_wrap(graph_title)),
      iteration = "list",
      memory = "transient",
      garbage_collection = TRUE,
      deployment = "main",
      # A failed branch/run is recorded in tar_meta() and becomes NULL rather
      # than aborting the whole pipeline.
      error = "null",
      description = "Graph: Graph objects visualizing nearest neighbors by part of speech."
    )
  ),
  tar_target(
    name = info_graph,
    command = graph_df |>
      mutate(
        workset_name = case_when(
          str_detect(workset_meta_id, "fiction") ~ "Fiction sample",
          str_detect(workset_meta_id, "marx") ~ "Marxist texts sample",
          TRUE ~ "All volumes"
        ),
        sample_strategy = case_when(
          sample_type == "restricted" ~ "Balanced (majority category undersampled)",
          TRUE ~ "Unbalanced"
        )
      ),
    deployment = "main",
    description = "Graph: Metadata for graph runs."
  ),
  tar_target(
    name = run_graph_qmd,
    command = write_run_graph_qmd(
      graph_df,
      run_id = cfg$run_id,
      run_description = cfg$run_description,
      tracked_terms = run_tracked_terms,
      tracked_top_n = run_tracked_top_n
    ),
    format = "file",
    deployment = "main",
    description = "Graph: Run-level Quarto document listing all graph targets."
  ),
  tar_target(
    name = run_graph_html,
    command = {
      graph_dependencies <- list(!!!graph_df$graph_object)
      performance_dependencies <- list(
        combined_performance,
        info_performance,
        sample_sizes
      )
      weight_dependencies <- list(
        predictive_model_weights,
        info_predictive_model_weights
      )
      render_quarto_file(run_graph_qmd)
    },
    format = "file",
    packages = c("quarto"),
    deployment = "main",
    description = "Graph: Rendered HTML for the run-level graph document."
  )
)

# Basic corpus stats ---------------------------------------------------------
# Feature-dependent bookworm summaries are generated per run feature via
# tar_eval() over corpus_stats_df (see object_parameters.R), emitting stable
# `<feature>_word_counts` / `_words_per_million` / `_text_counts` / `_text_percent`
# target names. The feature-independent totals/catalogue counts stay static.
corpus_stats_feature_targets <- list(
  tar_eval(
    values = corpus_stats_df,
    tar_target(
      name = word_counts_object,
      command = query_bookworm(
        bookworm_terms,
        counttype = c("WordCount"),
        lims = corpus_stats_lims
      ),
      deployment = "main",
      description = "Corpus Stats: Raw word counts for the feature over time."
    )
  ),
  tar_eval(
    values = corpus_stats_df,
    tar_target(
      name = words_per_million_object,
      command = query_bookworm(
        bookworm_terms,
        counttype = c("WordsPerMillion"),
        lims = corpus_stats_lims
      ),
      deployment = "main",
      description = "Corpus Stats: Words-per-million time series for the feature."
    )
  ),
  tar_eval(
    values = corpus_stats_df,
    tar_target(
      name = text_counts_object,
      command = query_bookworm(
        bookworm_terms,
        counttype = c("TextCount"),
        lims = corpus_stats_lims
      ),
      deployment = "main",
      description = "Corpus Stats: Number of texts containing the feature."
    )
  ),
  tar_eval(
    values = corpus_stats_df,
    tar_target(
      name = text_percent_object,
      command = query_bookworm(
        bookworm_terms,
        counttype = c("TextPercent"),
        lims = corpus_stats_lims
      ),
      deployment = "main",
      description = "Corpus Stats: Percentage of texts containing the feature."
    )
  )
)

# Per-feature translations sources (only for features declaring translations_file).
corpus_stats_translations_targets <- if (nrow(corpus_stats_translations_df) > 0) {
  list(
    tar_eval(
      values = corpus_stats_translations_df,
      tar_target(
        name = translations_object,
        command = here::here(translations_file),
        format = "file",
        deployment = "main",
        description = "Corpus Stats: Source Excel file listing feature translations."
      )
    ),
    tar_eval(
      values = corpus_stats_translations_df,
      tar_target(
        name = trans_object,
        command = democracy_translations_freqs(translations_object),
        deployment = "main",
        description = "Corpus Stats: Frequency table of feature translations."
      )
    )
  )
} else {
  list()
}

corpus_stats_targets <- c(
  corpus_stats_feature_targets,
  list(
  tar_target(
    name = total_words,
    command = query_bookworm(
      counttype = c("TotalWords"),
      lims = c(1700, 2020)
    ),
    deployment = "main",
    description = "Corpus Stats: Total word counts in the corpus over time."
  ),
  tar_target(
    name = total_texts,
    command = query_bookworm(
      counttype = "TotalTexts",
      lims = c(1700, 2020)
    ),
    deployment = "main",
    description = "Corpus Stats: Total number of texts in the corpus over time."
  ),
  tar_target(
    name = num_ht_bib_keys,
    command = cached_hathi_catalog %>%
      dplyr::pull(ht_bib_key) %>%
      dplyr::n_distinct(),
    deployment = "main",
    description = "Corpus Stats: Distinct HT bibliographic keys."
  ),
  tar_target(
    name = num_author_title,
    command = cached_hathi_catalog %>%
      dplyr::mutate(author_title = paste(author, title)) %>%
      dplyr::pull(author_title) %>%
      dplyr::n_distinct(),
    deployment = "main",
    description = "Corpus Stats: Distinct author-title combinations."
  ),
  tar_target(
    name = num_htids,
    command = cached_hathi_catalog %>%
      dplyr::group_by(ht_bib_key) %>%
      dplyr::summarise(num_htids = dplyr::n_distinct(htid)) %>%
      dplyr::count(num_htids),
    deployment = "main",
    description = "Corpus Stats: Distribution of HTIDs per bibliographic key."
  ),
  tar_target(
    name = num_htids_per_author,
    command = cached_hathi_catalog %>%
      dplyr::group_by(author, title) %>%
      dplyr::summarise(num_htids = dplyr::n_distinct(htid)) %>%
      dplyr::ungroup() %>%
      dplyr::count(num_htids),
    deployment = "main",
    description = "Corpus Stats: Distribution of HTIDs per author-title pair."
  ),
  tar_target(
    name = num_libraries,
    command = cached_hathi_catalog %>%
      dplyr::count(source),
    deployment = "main",
    description = "Corpus Stats: Counts of contributing libraries."
  ),
  tar_target(
    name = date_info,
    command = cached_hathi_catalog %>%
      dplyr::count(rights_date_used),
    deployment = "main",
    description = "Corpus Stats: Counts of volumes by rights_date_used."
  )
  ),
  corpus_stats_translations_targets
)

# Paper and appendixes -------------------------------------------------------
document_targets <- list(
  tar_target(
    name = graph_document,
    command = rmd_blocks_graph_names(graph_df),
    format = "file",
    deployment = "main",
    description = "Document: Markdown block with graph names for reporting."
  ),
  # error = "null" on the quarto targets: these read the store via
  # tar_read_raw(<string>), which tarchetypes cannot register as a dependency,
  # so they are not ordered after their inputs and a render failure must never
  # abort the (multi-day) pipeline. The `document` output group is omitted from
  # full_democracy.yml for the same reason; render documents locally until the
  # targets gain explicit dependencies (Phase 4 / W1).
  tar_quarto(
    name = Appendix,
    path = "Paper/Appendix.qmd",
    output_file = "Appendix.md",
    deployment = "main",
    error = "null",
    description = "Document: Render the Appendix markdown."
  ),
    tar_quarto(
    name = Paper,
    path = "Paper/The_Vector_Space_of_Democracy.qmd",
    output_file = "The_Vector_Space_of_Democracy.md",
    deployment = "main",
    error = "null",
    description = "Document: Render the Paper."
  ),
  tar_quarto(
    name = Appendix_Methods,
    path = "Paper/Appendix_Methods.qmd",
    output_file = "Appendix_Methods.md",
    deployment = "main",
    error = "null",
    description = "Document: Render Appendix B (methodological choices)."
  )
)

marxism_output_groups <- c(
  "marxism_ingest", "marxism_segments", "marxism_audit", "marxism_dfm",
  "marxism_model", "marxism_graph"
)

marxism_targets <- list()
if (length(intersect(cfg$outputs %||% character(0), marxism_output_groups))) {
  marxism_run <- Sys.getenv("MARXISM_RUN", cfg$run_id %||% "authority_marxism_glmnet_100")
  marxism_cfg <- read_marxism_pipeline_config(run = marxism_run)

  marxism_targets <- list(
    marxism_ingest = list(
      tar_target(
        name = marxism_config,
        command = marxism_cfg,
        deployment = "main",
        description = "Marxism ingest: merged Marx/Engels run configuration."
      ),
      tar_target(
        name = marxism_feature_object,
        command = quanteda::dictionary(marxism_config$feature$dictionary, tolower = FALSE),
        deployment = "main",
        description = "Marxism ingest: Quanteda dictionary for the target feature."
      ),
      tar_target(
        name = marxism_metadata,
        command = build_marx_engels_metadata(marxism_config),
        deployment = "main",
        description = "Marxism ingest: annotated Marx/Engels text metadata."
      )
    ),
    marxism_segments = list(
      tar_target(
        name = marxism_segment_counts,
        command = build_marxism_segment_counts(marxism_metadata, marxism_config),
        memory = "transient",
        garbage_collection = TRUE,
        description = "Marxism segments: POS-tagged segment-term counts from CoreNLP annotations."
      )
    ),
    marxism_audit = list(
      tar_target(
        name = marxism_feature_audit,
        command = audit_marxism_feature_support(
          marxism_segment_counts,
          marxism_metadata,
          marxism_config
        ),
        deployment = "main",
        description = "Marxism audit: feature support by decade before modeling."
      )
    ),
    marxism_dfm = list(
      tar_target(
        name = marxism_sampled_segments,
        command = sample_marxism_segment_counts(marxism_segment_counts, marxism_config),
        memory = "transient",
        garbage_collection = TRUE,
        description = "Marxism DFM: balanced positive/negative authority segment sample."
      ),
      tar_target(
        name = marxism_dfm,
        command = marxism_counts_to_dfm(marxism_sampled_segments, marxism_config),
        packages = c("Matrix", "quanteda"),
        memory = "transient",
        garbage_collection = TRUE,
        description = "Marxism DFM: sampled segment-feature matrix."
      ),
      tar_target(
        name = marxism_period_dfms,
        command = split_marxism_dfm_by_period(marxism_dfm, marxism_config),
        packages = c("quanteda"),
        memory = "transient",
        garbage_collection = TRUE,
        description = "Marxism DFM: period-specific DFMs for model fitting."
      )
    ),
    marxism_model = list(
      tar_target(
        name = marxism_splits,
        command = make_marxism_splits(marxism_period_dfms, marxism_feature_object),
        packages = c("quanteda", "rsample"),
        deployment = "main",
        description = "Marxism model: train/test splits by period."
      ),
      tar_target(
        name = marxism_predictive_models,
        command = fit_marxism_predictive_models(
          marxism_period_dfms,
          marxism_splits,
          marxism_feature_object,
          marxism_config
        ),
        packages = c("quanteda"),
        memory = "transient",
        garbage_collection = TRUE,
        description = "Marxism model: configured predictive models by decade."
      ),
      tar_target(
        name = marxism_model_errors,
        command = marxism_predictive_models |>
          dplyr::filter(!is.na(error)),
        deployment = "main",
        description = "Marxism model: captured model fit errors, if any."
      ),
      tar_target(
        name = marxism_predictive_model_weights,
        command = extract_marxism_model_weights(marxism_predictive_models),
        deployment = "main",
        description = "Marxism model: combined model weights by decade."
      ),
      tar_target(
        name = marxism_combined_performance,
        command = write_marxism_predictive_performance(
          marxism_predictive_models,
          marxism_period_dfms,
          marxism_splits,
          marxism_feature_object,
          marxism_config,
          performance_split = "testing"
        ),
        packages = c("magrittr", "quanteda", "yardstick"),
        format = "file",
        deployment = "main",
        description = "Marxism model: RDS file of testing performance metrics by period and model."
      ),
      tar_target(
        name = marxism_sample_sizes,
        command = summarize_marxism_sample_sizes(readRDS(marxism_combined_performance)),
        deployment = "main",
        description = "Marxism model: testing split sample sizes by period and model."
      )
    ),
    marxism_graph = list(
      tar_target(
        name = marxism_graphs,
        command = build_marxism_graphs(marxism_predictive_model_weights, marxism_config),
        packages = c("ggplot2"),
        deployment = "main",
        description = "Marxism graph: graph objects visualizing authority-associated terms."
      ),
      tar_target(
        name = marxism_graph_qmd,
        command = write_marxism_graph_qmd(marxism_config),
        format = "file",
        deployment = "main",
        description = "Marxism graph: generated Quarto report for authority graphs."
      ),
      tar_target(
        name = marxism_graph_html,
        command = {
          graph_dependencies <- list(
            marxism_graphs,
            marxism_feature_audit,
            marxism_predictive_model_weights,
            marxism_combined_performance,
            marxism_sample_sizes
          )
          render_quarto_file(marxism_graph_qmd)
        },
        format = "file",
        packages = c("quarto"),
        deployment = "main",
        description = "Marxism graph: rendered HTML report for authority graphs."
      )
    )
  )
}

jstor_output_groups <- c("jstor_ingest", "jstor_audit", "jstor_dfm", "jstor_model", "jstor_graph")

jstor_targets <- list()
if (length(intersect(cfg$outputs %||% character(0), jstor_output_groups))) {
  jstor_run <- Sys.getenv("JSTOR_RUN", cfg$run_id %||% "authority_jstor_dfr_audit")
  jstor_cfg <- read_jstor_pipeline_config(run = jstor_run)
  jstor_target_df <- tibble::tibble(
    jstor_config_object = list(rlang::sym(slugify("jstor_config", jstor_cfg$run_id))),
    jstor_feature_object_object = list(rlang::sym(slugify("jstor_feature_object", jstor_cfg$run_id))),
    jstor_source_inventory_object = list(rlang::sym(slugify("jstor_source_inventory", jstor_cfg$run_id))),
    jstor_feature_audit_object = list(rlang::sym(slugify("jstor_feature_audit", jstor_cfg$run_id))),
    jstor_sampling_audit_object = list(rlang::sym(slugify("jstor_sampling_audit", jstor_cfg$run_id))),
    jstor_sampled_texts_object = list(rlang::sym(slugify("jstor_sampled_texts", jstor_cfg$run_id))),
    jstor_document_counts_object = list(rlang::sym(slugify("jstor_document_counts", jstor_cfg$run_id))),
    jstor_dfm_object = list(rlang::sym(slugify("jstor_dfm", jstor_cfg$run_id))),
    jstor_period_dfms_object = list(rlang::sym(slugify("jstor_period_dfms", jstor_cfg$run_id))),
    jstor_splits_object = list(rlang::sym(slugify("jstor_splits", jstor_cfg$run_id))),
    jstor_predictive_models_object = list(rlang::sym(slugify("jstor_predictive_models", jstor_cfg$run_id))),
    jstor_model_errors_object = list(rlang::sym(slugify("jstor_model_errors", jstor_cfg$run_id))),
    jstor_predictive_model_weights_object = list(rlang::sym(slugify("jstor_predictive_model_weights", jstor_cfg$run_id))),
    jstor_combined_performance_object = list(rlang::sym(slugify("jstor_combined_performance", jstor_cfg$run_id))),
    jstor_sample_sizes_object = list(rlang::sym(slugify("jstor_sample_sizes", jstor_cfg$run_id))),
    jstor_graphs_object = list(rlang::sym(slugify("jstor_graphs", jstor_cfg$run_id))),
    jstor_graph_qmd_object = list(rlang::sym(slugify("jstor_graph_qmd", jstor_cfg$run_id))),
    jstor_graph_html_object = list(rlang::sym(slugify("jstor_graph_html", jstor_cfg$run_id)))
  )

  jstor_targets <- list(
    jstor_ingest = list(
      tar_eval(
        values = jstor_target_df,
        tar_target(
          name = jstor_config_object,
          command = jstor_cfg,
          deployment = "main",
          description = "JSTOR ingest: merged JSTOR run configuration."
        )
      ),
      tar_eval(
        values = jstor_target_df,
        tar_target(
          name = jstor_feature_object_object,
          command = build_jstor_feature_dictionary(jstor_config_object),
          deployment = "main",
          description = "JSTOR ingest: untagged Quanteda dictionary for the target feature."
        )
      ),
      tar_eval(
        values = jstor_target_df,
        tar_target(
          name = jstor_source_inventory_object,
          command = inventory_jstor_source(jstor_config_object),
          packages = c("readr"),
          deployment = "main",
          description = "JSTOR ingest: source inventory for DfR zips or JSONL sample."
        )
      )
    ),
    jstor_audit = list(
      tar_eval(
        values = jstor_target_df,
        tar_target(
          name = jstor_feature_audit_object,
          command = audit_jstor_feature_support(jstor_source_inventory_object, jstor_config_object),
          packages = c("jsonlite", "readr"),
          memory = "transient",
          garbage_collection = TRUE,
          description = "JSTOR audit: feature support by decade before modeling."
        )
      )
    ),
    jstor_dfm = list(
      tar_eval(
        values = jstor_target_df,
        tar_target(
          name = jstor_sampling_audit_object,
          command = load_jstor_sampling_audit(jstor_source_inventory_object, jstor_config_object),
          packages = c("targets"),
          deployment = "main",
          description = "JSTOR DFM: exact feature audit used as the modeling sampling frame."
        )
      ),
      tar_eval(
        values = jstor_target_df,
        tar_target(
          name = jstor_sampled_texts_object,
          command = sample_jstor_texts(jstor_sampling_audit_object, jstor_config_object),
          deployment = "main",
          description = "JSTOR DFM: selected top positive and matched negative texts."
        )
      ),
      tar_eval(
        values = jstor_target_df,
        tar_target(
          name = jstor_document_counts_object,
          command = build_jstor_document_counts(jstor_sampled_texts_object, jstor_config_object),
          packages = c("quanteda"),
          memory = "transient",
          garbage_collection = TRUE,
          description = "JSTOR DFM: tidy NGRAMS1 counts for selected texts."
        )
      ),
      tar_eval(
        values = jstor_target_df,
        tar_target(
          name = jstor_dfm_object,
          command = jstor_counts_to_dfm(jstor_document_counts_object, jstor_sampled_texts_object, jstor_config_object),
          packages = c("Matrix", "quanteda"),
          memory = "transient",
          garbage_collection = TRUE,
          description = "JSTOR DFM: sampled article-feature matrix."
        )
      ),
      tar_eval(
        values = jstor_target_df,
        tar_target(
          name = jstor_period_dfms_object,
          command = split_jstor_dfm_by_period(jstor_dfm_object, jstor_config_object),
          packages = c("quanteda"),
          memory = "transient",
          garbage_collection = TRUE,
          description = "JSTOR DFM: period-specific DFMs for model fitting."
        )
      )
    ),
    jstor_model = list(
      tar_eval(
        values = jstor_target_df,
        tar_target(
          name = jstor_splits_object,
          command = make_jstor_splits(jstor_period_dfms_object, jstor_feature_object_object),
          packages = c("quanteda", "rsample"),
          deployment = "main",
          description = "JSTOR model: train/test splits by period."
        )
      ),
      tar_eval(
        values = jstor_target_df,
        tar_target(
          name = jstor_predictive_models_object,
          command = fit_jstor_predictive_models(
            jstor_period_dfms_object,
            jstor_splits_object,
            jstor_feature_object_object,
            jstor_config_object
          ),
          packages = c("quanteda"),
          memory = "transient",
          garbage_collection = TRUE,
          description = "JSTOR model: configured predictive models by decade."
        )
      ),
      tar_eval(
        values = jstor_target_df,
        tar_target(
          name = jstor_model_errors_object,
          command = jstor_predictive_models_object |>
            dplyr::filter(!is.na(error)),
          deployment = "main",
          description = "JSTOR model: captured model fit errors, if any."
        )
      ),
      tar_eval(
        values = jstor_target_df,
        tar_target(
          name = jstor_predictive_model_weights_object,
          command = extract_jstor_model_weights(jstor_predictive_models_object),
          deployment = "main",
          description = "JSTOR model: combined model weights by decade."
        )
      ),
      tar_eval(
        values = jstor_target_df,
        tar_target(
          name = jstor_combined_performance_object,
          command = write_jstor_predictive_performance(
            jstor_predictive_models_object,
            jstor_period_dfms_object,
            jstor_splits_object,
            jstor_feature_object_object,
            jstor_config_object,
            performance_split = "testing"
          ),
          packages = c("magrittr", "quanteda", "yardstick"),
          format = "file",
          deployment = "main",
          description = "JSTOR model: RDS file of testing performance metrics by period and model."
        )
      ),
      tar_eval(
        values = jstor_target_df,
        tar_target(
          name = jstor_sample_sizes_object,
          command = summarize_jstor_sample_sizes(readRDS(jstor_combined_performance_object)),
          deployment = "main",
          description = "JSTOR model: testing split sample sizes by period and model."
        )
      )
    ),
    jstor_graph = list(
      tar_eval(
        values = jstor_target_df,
        tar_target(
          name = jstor_graphs_object,
          command = build_jstor_graphs(jstor_predictive_model_weights_object, jstor_config_object),
          packages = c("ggplot2"),
          deployment = "main",
          description = "JSTOR graph: graph objects visualizing authority-associated terms."
        )
      ),
      tar_eval(
        values = jstor_target_df,
        tar_target(
          name = jstor_graph_qmd_object,
          command = write_jstor_graph_qmd(jstor_config_object),
          format = "file",
          packages = c("readr"),
          deployment = "main",
          description = "JSTOR graph: generated Quarto report for authority graphs."
        )
      ),
      tar_eval(
        values = jstor_target_df,
        tar_target(
          name = jstor_graph_html_object,
          command = {
            graph_dependencies <- list(
              jstor_graphs_object,
              jstor_sampling_audit_object,
              jstor_predictive_model_weights_object,
              jstor_combined_performance_object,
              jstor_sample_sizes_object
            )
            render_quarto_file(jstor_graph_qmd_object)
          },
          format = "file",
          packages = c("quarto"),
          deployment = "main",
          description = "JSTOR graph: rendered HTML report for authority graphs."
        )
      )
    )
  )
}

target_groups <- list(
  ingest = ingest_targets,
  dfm = dfm_targets,
  model = model_targets,
  graph = graph_targets,
  corpus_stats = corpus_stats_targets,
  document = document_targets
) |>
  c(marxism_targets) |>
  c(jstor_targets)

enabled_outputs <- cfg$outputs %||% names(target_groups)
unknown_outputs <- setdiff(enabled_outputs, names(target_groups))
if (length(unknown_outputs)) {
  stop("Unknown output group(s): ", paste(unknown_outputs, collapse = ", "))
}

target_groups[enabled_outputs]
