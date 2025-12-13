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
