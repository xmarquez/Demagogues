#' Generate a graph appendix Rmd from targets graph metadata
#'
#' Creates an R Markdown file (`Paper/graph_appendix.rmd`) containing one code
#' chunk per graph target. Each chunk reads the graph object via `tar_read()` and
#' prints it with a figure caption describing the model/weighting configuration.
#'
#' @param graph_df A data frame/tibble of graph metadata produced by the targets
#'   pipeline (must include `graph_id`, `feature_name`, and fields used to build
#'   the description such as `predictive_model_engine`, `svd_dims`, `ppmi_fun`,
#'   `svd_weight`, `sample_max_vols`, and `dfm_to_lower`).
#'
#' @return The path to the generated file, as returned by
#'   `here::here("Paper", "graph_appendix.rmd")`.
rmd_blocks_graph_names <- function(graph_df) {

  graph_names <- graph_df %>%
    dplyr::mutate(description = case_when(
      !is.na(predictive_model_engine) ~ paste(
        "Weights of", predictive_model_engine,
        sample_max_vols, "vols. sample",
        ifelse(dfm_to_lower, "lowercased",
               "not lowercased")),
      !is.na(svd_dims) ~paste(
        "Weights of svd decomposition of doc-term matrix, weighted by",
        svd_weight,
        sample_max_vols, "vols. sample",
        ifelse(dfm_to_lower, "lowercased",
               "not lowercased")),
      !is.na(ppmi_fun) ~paste(
        "PPMI with target feature",
        ppmi_fun,
        sample_max_vols, "vols. sample",
        ifelse(dfm_to_lower, "lowercased",
               "not lowercased")))
      ) %>%
    dplyr::select(description, graph_id, feature_name)

  blocks <- graph_names %>%
    purrr::pmap(function(description, graph_id, feature_name) paste0("\n```{r fig-",
                    graph_id, "}", "\n",
                    "#| fig-cap: ",
                    description,
                    ". Terms at the top are more closely associated with '",
                    feature_name,
                    "' in later years. ",
                    "Label indicates the peak density of association.", "\n",
                    "#| fig-height: 9\n",
                    "graph <- tar_read(",
                    graph_id, ")",
                    "\n\n", "graph", "\n\n",
                    "```\n\n"
    ))

  blocks <- unlist(blocks) %>%
    paste(sep = "\n")

  readr::write_lines(blocks, here::here("Paper", "graph_appendix.rmd"))
  here::here("Paper", "graph_appendix.rmd")
}

#' Generate a run-level Quarto document for graph targets
#'
#' Creates a Quarto document containing one plot chunk per graph target in the
#' active run. The generated document reads precomputed graph objects from the
#' targets store, so rendering it does not rerun the graph computations.
#'
#' @param graph_df A data frame/tibble of graph metadata produced by the targets
#'   pipeline. Must include `graph_object`, `feature_name`,
#'   `graph_pos_patterns`, `sample_max_vols`, `sample_type`, `dfm_to_lower`,
#'   and the model/weight columns used to identify graph types.
#' @param run_id Identifier for the active run.
#' @param run_description Optional human-readable description of the run.
#' @param tracked_terms Character vector of specific terms to plot over time.
#' @param tracked_top_n Number of top terms per predictive model/POS group to plot.
#' @param output_dir Directory where the `.qmd` file should be written.
#'
#' @return The path to the generated `.qmd` file.
write_run_graph_qmd <- function(graph_df,
                                run_id,
                                run_description = "",
                                tracked_terms = character(0),
                                tracked_top_n = 10,
                                output_dir = here::here("Paper")) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  tracked_top_n <- as.integer(tracked_top_n)
  tracked_top_n <- tracked_top_n[!is.na(tracked_top_n)]
  tracked_top_n <- if (length(tracked_top_n)) tracked_top_n[[1]] else 10L
  if (tracked_top_n < 1L) {
    tracked_top_n <- 10L
  }
  if (is.null(run_description)) {
    run_description <- ""
  }
  run_description <- as.character(run_description)
  run_description <- run_description[!is.na(run_description)]
  run_description <- if (length(run_description)) run_description[[1]] else ""

  yaml_quote <- function(x) {
    paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
  }

  chunk_quote <- function(x) {
    x <- gsub("\\", "\\\\", x, fixed = TRUE)
    x <- gsub("\"", "\\\"", x, fixed = TRUE)
    paste0("\"", x, "\"")
  }

  r_character_vector <- function(x) {
    x <- x[!is.na(x) & nzchar(x)]
    if (!length(x)) {
      return("character(0)")
    }
    paste0("c(", paste(purrr::map_chr(x, chunk_quote), collapse = ", "), ")")
  }

  slug <- stringr::str_replace_all(run_id, "[^A-Za-z0-9]+", "_") |>
    stringr::str_replace_all("_+", "_") |>
    stringr::str_remove("^_") |>
    stringr::str_remove("_$")

  output_path <- file.path(output_dir, paste0("graphs_", slug, ".qmd"))

  graph_info <- graph_df %>%
    dplyr::mutate(
      graph_target = purrr::map_chr(graph_object, rlang::as_name),
      graph_type = dplyr::case_when(
        !is.na(predictive_model_engine) & predictive_model_engine == "xgboost" ~
          paste("Predictive model importance", predictive_model_engine),
        !is.na(predictive_model_engine) ~ paste("Predictive model weights", predictive_model_engine),
        !is.na(svd_dims) ~ paste("SVD word vectors", svd_weight, svd_dims, "dimensions"),
        !is.na(ppmi_fun) ~ paste("PPMI", ppmi_fun),
        TRUE ~ "Graph"
      ),
      pos_label = dplyr::case_when(
        graph_pos_patterns == "." ~ "All terms",
        stringr::str_detect(graph_pos_patterns, "_nn|_NN") ~ "Nouns",
        stringr::str_detect(graph_pos_patterns, "_vb|_VB") ~ "Verbs",
        stringr::str_detect(graph_pos_patterns, "_jj|_JJ") ~ "Adjectives",
        graph_pos_patterns == "ism_" ~ "Ism terms",
        graph_pos_patterns == "^[A-Z].+NN" ~ "Uppercase nouns",
        TRUE ~ graph_pos_patterns
      ),
      section_title = paste(graph_type, pos_label, sep = " - "),
      caption = dplyr::case_when(
        predictive_model_engine == "xgboost" ~ paste0(
          graph_type, "; ", pos_label, "; ",
          sample_max_vols, " ", sample_type, " volumes per period; ",
          ifelse(dfm_to_lower, "lowercased", "not lowercased"),
          ". Terms at the top contribute more to the xgboost classifier in later years. ",
          "The label marks the peak density of importance."
        ),
        TRUE ~ paste0(
          graph_type, "; ", pos_label, "; ",
          sample_max_vols, " ", sample_type, " volumes per period; ",
          ifelse(dfm_to_lower, "lowercased", "not lowercased"),
          ". Terms at the top are more closely associated with '",
          feature_name,
          "' in later years. The label marks the peak density of association."
        )
      ),
      chunk_label = paste0(
        "fig-",
        stringr::str_replace_all(graph_target, "[^A-Za-z0-9]+", "-")
      )
    )

  model_info <- graph_info %>%
    dplyr::filter(!is.na(predictive_model_engine)) %>%
    dplyr::distinct(
      predictive_model_engine,
      predictive_model_dfm_weight
    ) %>%
    dplyr::mutate(
      engine_order = match(
        predictive_model_engine,
        c("glmnet", "LiblineaR", "naivebayes", "xgboost")
      ),
      engine_order = dplyr::coalesce(engine_order, 999L),
      model_label = paste(
        predictive_model_engine,
        predictive_model_dfm_weight,
        "weights"
      ),
      model_slug = stringr::str_replace_all(
        tolower(model_label),
        "[^a-z0-9]+",
        "-"
      ) |>
        stringr::str_remove("^-") |>
        stringr::str_remove("-$")
    ) %>%
    dplyr::arrange(engine_order, model_label)

  tracked_pos_info <- tibble::tribble(
    ~pos_group, ~pos_slug, ~pos_term_label,
    "Adjectives", "adjectives", "adjective terms",
    "Verbs", "verbs", "verb terms",
    "Nouns", "nouns", "noun terms"
  )

  top_term_fig_height <- max(7, 2 + 0.8 * tracked_top_n)

  features <- paste(unique(graph_info$feature_name), collapse = ", ")

  header <- c(
    "---",
    paste0("title: ", yaml_quote(paste("Graphs for", run_id))),
    "format:",
    "  html:",
    "    toc: true",
    "    toc-depth: 3",
    "    toc-location: right",
    "    include-in-header:",
    "      text: |",
    "        <style>",
    "        #TOC ul.collapse {",
    "          display: block !important;",
    "        }",
    "        #TOC ul.collapse li {",
    "          margin-left: 0.75rem;",
    "        }",
    "        </style>",
    "execute-dir: project",
    "execute:",
    "  echo: false",
    "  message: false",
    "  warning: false",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "library(targets)",
    "library(dplyr)",
    "library(tidyr)",
    "library(ggplot2)",
    "library(here)",
    "library(knitr)",
    "targets::tar_config_set(store = here::here(\"_targets\"), config = \"custom.yaml\")",
    "ggplot2::theme_set(ggplot2::theme_bw())",
    "```",
    "",
    paste0("Run: `", run_id, "`."),
    paste0("Feature(s): ", features, "."),
    if (nzchar(run_description)) run_description else NULL,
    "",
    "This document reads precomputed graph objects from the local targets store."
  )

  performance_blocks <- c(
    "",
    "## Predictive Performance",
    "",
    paste(
      "F1 is the harmonic mean of precision and recall for pages containing",
      "the target feature. These diagnostics are internal to the configured",
      "sample and train/test split; they do not estimate prevalence in the",
      "full HathiTrust corpus."
    ),
    "",
    "```{r}",
    "#| label: fig-performance-f1",
    "#| fig-cap: \"Testing F1 by model and period.\"",
    "#| fig-width: 8",
    "#| fig-height: 4",
    "performance <- targets::tar_read_raw(\"combined_performance\") |>",
    "  dplyr::left_join(",
    "    targets::tar_read_raw(\"info_performance\") |>",
    "      dplyr::distinct(",
    "        performance_id,",
    "        performance_split,",
    "        predictive_model_engine,",
    "        predictive_model_dfm_weight,",
    "        sample_type,",
    "        sample_max_vols",
    "      ),",
    "    by = \"performance_id\"",
    "  ) |>",
    "  dplyr::mutate(",
    "    model = paste(",
    "      predictive_model_engine,",
    "      predictive_model_dfm_weight,",
    "      \"weights\"",
    "    )",
    "  )",
    "",
    "sample_sizes <- targets::tar_read_raw(\"sample_sizes\") |>",
    "  dplyr::select(",
    "    performance_id,",
    "    period,",
    "    performance_split,",
    "    sample_size,",
    "    negative,",
    "    positive",
    "  )",
    "",
    "performance_wide <- performance |>",
    "  dplyr::filter(",
    "    performance_split == \"testing\",",
    "    .metric %in% c(\"f_meas\", \"roc_auc\", \"accuracy\", \"kap\")",
    "  ) |>",
    "  dplyr::select(period, performance_id, model, .metric, .estimate) |>",
    "  tidyr::pivot_wider(names_from = .metric, values_from = .estimate) |>",
    "  dplyr::left_join(",
    "    sample_sizes |>",
    "      dplyr::filter(performance_split == \"testing\"),",
    "    by = c(\"period\", \"performance_id\")",
    "  ) |>",
    "  dplyr::arrange(model, period)",
    "",
    "ggplot2::ggplot(",
    "  performance_wide,",
    "  ggplot2::aes(x = period, y = f_meas, color = model, group = model)",
    ") +",
    "  ggplot2::geom_line(linewidth = 0.6) +",
    "  ggplot2::geom_point(size = 2) +",
    "  ggplot2::scale_y_continuous(limits = c(0, 1)) +",
    "  ggplot2::labs(x = \"Period\", y = \"Testing F1\", color = \"Model\")",
    "```",
    "",
    "```{r}",
    "#| label: tbl-performance-testing",
    "performance_wide |>",
    "  dplyr::transmute(",
    "    model,",
    "    period,",
    "    f1 = round(f_meas, 3),",
    "    roc_auc = round(roc_auc, 3),",
    "    accuracy = round(accuracy, 3),",
    "    kappa = round(kap, 3),",
    "    sample_size,",
    "    positive,",
    "    negative",
    "  ) |>",
    "  knitr::kable(",
    "    caption = \"Testing split predictive performance by model and period.\"",
    "  )",
    "```",
    "",
    "## Term Weight Trajectories",
    "",
    paste(
      "These plots use raw predictive-model weights recovered from the targets",
      "store. Xgboost values are feature-importance scores, not signed",
      "coefficients. Facets use free y scales because model outputs are not",
      "directly comparable in magnitude. The automatic top-term plots select",
      "adjectives, verbs, and nouns separately within each model using a cumulative log-scaled",
      "positive-association score: `sum(log1p(pmax(weight, 0)))` across",
      "periods. This favors terms with persistent positive association while",
      "dampening one-period spikes."
    ),
    "",
    "```{r}",
    "#| label: top-term-data",
    "#| include: false",
    paste0("tracked_top_n <- ", tracked_top_n),
    "tracked_pos_levels <- c(\"Adjectives\", \"Verbs\", \"Nouns\")",
    "",
    "model_weights <- targets::tar_read_raw(\"predictive_model_weights\") |> ",
    "  dplyr::left_join(",
    "    targets::tar_read_raw(\"info_predictive_model_weights\") |> ",
    "      dplyr::distinct(",
    "        weight_id = weight_pred_id,",
    "        predictive_model_engine,",
    "        predictive_model_dfm_weight",
    "      ),",
    "    by = \"weight_id\"",
    "  ) |> ",
    "  dplyr::mutate(",
    "    model = paste(",
    "      predictive_model_engine,",
    "      predictive_model_dfm_weight,",
    "      \"weights\"",
    "    )",
    "  )",
    "",
    "top_terms <- model_weights |> ",
    "  dplyr::mutate(",
    "    pos_group = dplyr::case_when(",
    "      stringr::str_detect(",
    "        word,",
    "        stringr::regex(\"_jj[a-z]*$\", ignore_case = TRUE)",
    "      ) ~ \"Adjectives\",",
    "      stringr::str_detect(",
    "        word,",
    "        stringr::regex(\"_vb[a-z]*$\", ignore_case = TRUE)",
    "      ) ~ \"Verbs\",",
    "      stringr::str_detect(",
    "        word,",
    "        stringr::regex(\"_nn[a-z]*$\", ignore_case = TRUE)",
    "      ) ~ \"Nouns\",",
    "      TRUE ~ NA_character_",
    "    ),",
    "    pos_group = factor(pos_group, levels = tracked_pos_levels),",
    "    positive_value = pmax(value, 0),",
    "    selection_score = log1p(positive_value)",
    "  ) |> ",
    "  dplyr::filter(!is.na(pos_group)) |> ",
    "  dplyr::group_by(model, pos_group, word) |> ",
    "  dplyr::summarise(",
    "    association_score = sum(selection_score, na.rm = TRUE),",
    "    periods_positive = sum(positive_value > 0, na.rm = TRUE),",
    "    max_positive_weight = max(positive_value, na.rm = TRUE),",
    "    .groups = \"drop\"",
    "  ) |> ",
    "  dplyr::filter(association_score > 0) |> ",
    "  dplyr::group_by(model, pos_group) |> ",
    "  dplyr::arrange(",
    "    dplyr::desc(association_score),",
    "    dplyr::desc(periods_positive),",
    "    dplyr::desc(max_positive_weight),",
    "    word,",
    "    .by_group = TRUE",
    "  ) |> ",
    "  dplyr::slice_head(n = tracked_top_n) |> ",
    "  dplyr::ungroup()",
    "```",
    "",
    if (nrow(model_info)) {
      purrr::pmap_chr(
        model_info,
        function(predictive_model_engine,
                 predictive_model_dfm_weight,
                 engine_order,
                 model_label,
                 model_slug,
                 ...) {
          purrr::pmap_chr(
            tracked_pos_info,
            function(pos_group, pos_slug, pos_term_label) {
              paste0(
                "### ", model_label, " - ", pos_group, "\n\n",
                "```{r}\n",
                "#| label: fig-top-term-weights-", model_slug, "-", pos_slug, "\n",
                "#| fig-cap: \"Top persistent positive-association ",
                pos_term_label,
                " for ",
                model_label,
                ".\"\n",
                "#| fig-width: 9\n",
                "#| fig-height: ", top_term_fig_height, "\n",
                "model_name <- ",
                chunk_quote(model_label),
                "\n",
                "pos_name <- ",
                chunk_quote(pos_group),
                "\n\n",
                "model_top_terms <- top_terms |> \n",
                "  dplyr::filter(model == model_name, pos_group == pos_name) |> \n",
                "  dplyr::arrange(\n",
                "    dplyr::desc(association_score),\n",
                "    dplyr::desc(periods_positive),\n",
                "    dplyr::desc(max_positive_weight),\n",
                "    word\n",
                "  )\n\n",
                "if (nrow(model_top_terms)) {\n",
                "  model_top_weights <- tidyr::expand_grid(\n",
                "    model = model_name,\n",
                "    period = sort(unique(model_weights$period[model_weights$model == model_name])),\n",
                "    word = model_top_terms$word\n",
                "  ) |> \n",
                "    dplyr::left_join(\n",
                "      model_weights |> \n",
                "        dplyr::filter(model == model_name) |> \n",
                "        dplyr::select(model, period, word, value),\n",
                "      by = c(\"model\", \"period\", \"word\")\n",
                "    ) |> \n",
                "    dplyr::mutate(\n",
                "      value = tidyr::replace_na(value, 0),\n",
                "      word = factor(word, levels = model_top_terms$word)\n",
                "    )\n\n",
                "  ggplot2::ggplot(\n",
                "    model_top_weights,\n",
                "    ggplot2::aes(x = period, y = value, group = word)\n",
                "  ) +\n",
                "    ggplot2::geom_hline(yintercept = 0, color = \"grey75\", linewidth = 0.3) +\n",
                "    ggplot2::geom_line(color = \"#2F5D62\", linewidth = 0.65) +\n",
                "    ggplot2::geom_point(color = \"#2F5D62\", size = 1.3) +\n",
                "    ggplot2::facet_wrap(ggplot2::vars(word), ncol = 1, scales = \"free_y\") +\n",
                "    ggplot2::labs(\n",
                "      x = \"Period\",\n",
                "      y = \"Raw weight / importance\",\n",
                "      subtitle = paste(\n",
                "        \"Terms ordered by cumulative log-scaled positive association within\",\n",
                "        tolower(pos_name)\n",
                "      )\n",
                "    ) +\n",
                "    ggplot2::theme(\n",
                "      legend.position = \"none\",\n",
                "      strip.text = ggplot2::element_text(face = \"bold\")\n",
                "    )\n",
                "} else {\n",
                "  cat(\"No positive ",
                pos_term_label,
                " available for this model.\")\n",
                "}\n",
                "```\n"
              )
            }
          ) |>
            paste(collapse = "\n")
        }
      )
    } else {
      "No predictive model weights are configured for this run."
    },
    "",
    "### Selected Configured Terms",
    "",
    "```{r}",
    "#| label: fig-selected-term-weights",
    "#| fig-cap: \"Selected term weights or importance by model over time.\"",
    "#| fig-width: 9",
    "#| fig-height: 5",
    paste0("tracked_terms <- ", r_character_vector(tracked_terms)),
    "",
    "if (length(tracked_terms)) {",
    "  selected_weights <- model_weights |> ",
    "    dplyr::distinct(model, period) |> ",
    "    tidyr::expand_grid(word = tracked_terms) |> ",
    "    dplyr::left_join(",
    "      model_weights |> ",
    "        dplyr::filter(word %in% tracked_terms) |> ",
    "        dplyr::select(model, period, word, value),",
    "      by = c(\"model\", \"period\", \"word\")",
    "    ) |> ",
    "    dplyr::mutate(",
    "      value = tidyr::replace_na(value, 0),",
    "      word = factor(word, levels = tracked_terms)",
    "    )",
    "",
    "  ggplot2::ggplot(",
    "    selected_weights,",
    "    ggplot2::aes(x = period, y = value, color = word, group = word)",
    "  ) +",
    "    ggplot2::geom_hline(yintercept = 0, color = \"grey70\", linewidth = 0.3) +",
    "    ggplot2::geom_line(linewidth = 0.6) +",
    "    ggplot2::geom_point(size = 1.5) +",
    "    ggplot2::facet_wrap(ggplot2::vars(model), scales = \"free_y\") +",
    "    ggplot2::labs(x = \"Period\", y = \"Raw weight / importance\", color = \"Term\")",
    "} else {",
    "  cat(\"No tracked terms configured.\")",
    "}",
    "```",
    "",
    "## Graphs"
  )

  if (!nrow(graph_info)) {
    blocks <- "No graph targets are configured for this run."
  } else {
    blocks <- purrr::pmap_chr(
      graph_info,
      function(section_title, caption, chunk_label, graph_target, ...) {
        paste0(
          "\n\n### ", section_title, "\n\n",
          "```{r}\n",
          "#| label: ", chunk_label, "\n",
          "#| fig-cap: ", chunk_quote(caption), "\n",
          "#| fig-width: 11\n",
          "#| fig-height: 9\n",
          "graph <- targets::tar_read_raw(\"", graph_target, "\")\n",
          "graph\n",
          "```"
        )
      }
    )
  }

  readr::write_lines(c(header, performance_blocks, blocks), output_path)
  output_path
}

#' Render a Quarto document and return its output paths
#'
#' Renders a single `.qmd` file to HTML and returns the generated HTML path plus
#' the support files directory when Quarto creates one.
#'
#' @param qmd_path Path to a Quarto source document.
#' @param output_format Quarto output format to render.
#' @param quiet Logical passed to `quarto::quarto_render()`.
#'
#' @return A character vector of file/directory paths produced by rendering.
render_quarto_file <- function(qmd_path,
                               output_format = "html",
                               quiet = TRUE) {
  quarto::quarto_render(
    input = qmd_path,
    output_format = output_format,
    execute_dir = getwd(),
    quiet = quiet
  )

  html_path <- paste0(tools::file_path_sans_ext(qmd_path), ".html")
  if (!file.exists(html_path)) {
    stop("Expected rendered Quarto output not found: ", html_path, call. = FALSE)
  }

  support_dir <- paste0(tools::file_path_sans_ext(qmd_path), "_files")
  output_paths <- html_path
  if (dir.exists(support_dir)) {
    output_paths <- c(output_paths, support_dir)
  }

  output_paths
}
