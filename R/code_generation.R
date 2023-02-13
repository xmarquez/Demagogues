rmd_blocks_graph_names <- function(graph_names) {

  noun_blocks <- graph_names %>%
    purrr::map(function(x) paste0("\n```{r fig-",
                    x, "_nn}", "\n",
                    "#| fig-cap: ",
                    stringr::str_remove(x, "graph_") %>% stringr::str_replace_all("_", " "),
                    ", nouns only. Terms at the top are more closely associated with 'democracy' in later years. Label indicates the peak density of association.", "\n",
                    "#| fig-height: 9\n",
                    "graph <- tar_read(",
                    x, ")",
                    "\n\n", "graph[[2]]", "\n\n",
                    "```\n\n"
    ))

  verb_blocks <- graph_names %>%
    purrr::map(function(x) paste0("\n```{r fig-",
                    x, "_vb}", "\n",
                    "#| fig-cap: ",
                    stringr::str_remove(x, "graph_") %>% stringr::str_replace_all("_", " "),
                    ", verbs only. Terms at the top are more closely associated with 'democracy' in later years. Label indicates the peak density of association.", "\n",
                    "#| fig-height: 9\n",
                    "\n\n", "graph[[3]]", "\n\n",
                    "```\n\n"
    ))

  adjective_blocks <- graph_names %>%
    purrr::map(function(x) paste0("\n```{r fig-",
                    x, "_jj}", "\n",
                    "#| fig-cap: ",
                    stringr::str_remove(x, "graph_") %>% stringr::str_replace_all("_", " "),
                    ", adjectives only. Terms at the top are more closely associated with 'democracy' in later years. Label indicates the peak density of association.", "\n",
                    "#| fig-height: 9\n",
                    "\n\n", "graph[[4]]", "\n\n",
                    "```\n\n"
    ))

  ism_blocks <- graph_names %>%
    purrr::map(function(x) paste0("\n```{r fig-",
                    x, "_ism}", "\n",
                    "#| fig-cap: ",
                    stringr::str_remove(x, "graph_") %>% stringr::str_replace_all("_", " "),
                    ", -ism terms only. Terms at the top are more closely associated with 'democracy' in later years. Label indicates the peak density of association.", "\n",
                    "#| fig-height: 9\n",
                    "\n\n", "graph[[5]]", "\n\n",
                    "```\n\n"
    ))

  uppercase_blocks <- graph_names %>%
    purrr::map(function(x) paste0("\n```{r fig-",
                                  x, "_uppercase_nouns}", "\n",
                                  "#| fig-cap: ",
                                  stringr::str_remove(x, "graph_") %>% stringr::str_replace_all("_", " "),
                                  ", nouns starting with uppercase only. Terms at the top are more closely associated with 'democracy' in later years. Label indicates the peak density of association.", "\n",
                                  "#| fig-height: 9\n",
                                  "\n\n", "graph[[6]]", "\n\n",
                                  "```\n\n"
    ))

  list(noun_blocks, verb_blocks, adjective_blocks, ism_blocks, uppercase_blocks) %>%
    purrr::pmap(paste) %>%
    purrr::walk(cat)
}
