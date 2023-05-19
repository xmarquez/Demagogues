rmd_blocks_graph_names <- function(graphs_df) {

  graph_names <- graphs_df %>%
    dplyr::select(description, id, target_feature)

  blocks <- graph_names %>%
    purrr::pmap(function(description, id, target_feature) paste0("\n```{r fig-",
                    id, "}", "\n",
                    "#| fig-cap: ",
                    description,
                    ". Terms at the top are more closely associated with '",
                    target_feature,
                    "' in later years. ",
                    "Label indicates the peak density of association.", "\n",
                    "#| fig-height: 9\n",
                    "graph <- tar_read(",
                    id, ")",
                    "\n\n", "graph", "\n\n",
                    "```\n\n"
    ))

  blocks <- unlist(blocks) %>%
    paste(sep = "\n")

  readr::write_lines(blocks, here::here("Paper", "graph_appendix.rmd"))
  here::here("Paper", "graph_appendix.rmd")
}
