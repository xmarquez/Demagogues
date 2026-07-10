# graph_doc run-config parsing (resolve_graph_doc_config) and chunk-list
# filtering (filter_graph_doc) for the run-level graph browser document (W4).
# These select which graph chunks the generated qmd emits; they must NOT alter
# which graph_object targets are built.

# A small graph grid resembling the real graph_df columns: 5 replicates x
# (predictive glmnet/xgboost, svd, ppmi) x (adjectives, ism, nouns) feature types.
make_graph_df <- function() {
  weight_variants <- tibble::tribble(
    ~predictive_model_engine, ~predictive_model_dfm_weight, ~svd_dims, ~svd_weight, ~ppmi_fun,
    "glmnet",                 "ppmi",                       NA,        NA,          NA,
    "xgboost",                "ppmi",                       NA,        NA,          NA,
    NA,                       NA,                           50L,       "ppmi",      NA,
    NA,                       NA,                           NA,        NA,          "feature_ppmi"
  )
  pos_variants <- c("(_jj|_JJ)", "ism_", "(_nn|_NN)")

  tidyr::expand_grid(
    sample_rep = 1:5,
    weight_variants,
    graph_pos_patterns = pos_variants
  )
}

test_that("resolve_graph_doc_config defaults to rep-1 headline, no other filters", {
  cfg <- resolve_graph_doc_config(NULL)
  expect_equal(cfg$reps, 1L)
  expect_null(cfg$pos)
  expect_null(cfg$weight_types)
  expect_null(cfg$engines)
  expect_null(cfg$period_min)
  expect_null(cfg$period_max)
  expect_false(cfg$render_all)
})

test_that("graph_doc: all renders everything (all axes NULL, render_all TRUE)", {
  cfg <- resolve_graph_doc_config("all")
  expect_true(cfg$render_all)
  expect_null(cfg$reps)
  expect_null(cfg$pos)
  expect_null(cfg$weight_types)
  expect_null(cfg$engines)
})

test_that("per-axis `all` opts out of that filter only", {
  cfg <- resolve_graph_doc_config(list(reps = "all", pos = "all"))
  expect_null(cfg$reps)
  expect_null(cfg$pos)
  expect_false(cfg$render_all)
})

test_that("reps, pos, weight_types, engines parse and normalize", {
  cfg <- resolve_graph_doc_config(list(
    reps = c(1, 3),
    pos = c("adjectives", "ism"),
    weight_types = "predictive",
    engines = c("glmnet", "xgboost")
  ))
  expect_equal(cfg$reps, c(1L, 3L))
  expect_setequal(cfg$pos, c("adjectives", "ism"))
  expect_equal(cfg$weight_types, "predictive")
  expect_setequal(cfg$engines, c("glmnet", "xgboost"))
})

test_that("pos and weight-family selectors accept aliases", {
  cfg <- resolve_graph_doc_config(list(
    pos = c("adj", "isms", "nn"),
    weight_types = c("predictive_model_weights", "word_vectors")
  ))
  expect_setequal(cfg$pos, c("adjectives", "ism", "nouns"))
  expect_setequal(cfg$weight_types, c("predictive", "svd"))
})

test_that("periods parse to a min/max range; `all` clears it", {
  cfg <- resolve_graph_doc_config(list(periods = c(1800, 1950)))
  expect_equal(cfg$period_min, 1800)
  expect_equal(cfg$period_max, 1950)

  cfg_all <- resolve_graph_doc_config(list(periods = "all"))
  expect_null(cfg_all$period_min)
  expect_null(cfg_all$period_max)
})

test_that("graph_doc_pos_slug maps regex patterns to canonical slugs", {
  expect_equal(
    graph_doc_pos_slug(c(".", "(_nn|_NN)", "(_vb|_VB)", "(_jj|_JJ)", "ism_", "^[A-Z].+NN")),
    c("all_terms", "nouns", "verbs", "adjectives", "ism", "uppercase_nouns")
  )
})

test_that("graph_doc_weight_family classifies predictive / svd / ppmi rows", {
  df <- make_graph_df()
  fam <- graph_doc_weight_family(df)
  expect_setequal(unique(fam), c("predictive", "svd", "ppmi"))
  expect_true(all(fam[!is.na(df$predictive_model_engine)] == "predictive"))
  expect_true(all(fam[!is.na(df$svd_dims)] == "svd"))
  expect_true(all(fam[!is.na(df$ppmi_fun)] == "ppmi"))
})

test_that("filter_graph_doc default keeps only rep 1", {
  df <- make_graph_df()
  out <- filter_graph_doc(df, resolve_graph_doc_config(NULL))
  expect_equal(sort(unique(out$sample_rep)), 1L)
  # 4 weight variants x 3 pos = 12 chunks for rep 1.
  expect_equal(nrow(out), 12L)
})

test_that("filter_graph_doc render_all keeps every chunk", {
  df <- make_graph_df()
  out <- filter_graph_doc(df, resolve_graph_doc_config("all"))
  expect_equal(nrow(out), nrow(df))
})

test_that("filter_graph_doc applies pos, weight-family and engine filters", {
  df <- make_graph_df()
  cfg <- resolve_graph_doc_config(list(
    reps = 1,
    pos = c("adjectives", "ism"),
    weight_types = "predictive",
    engines = "glmnet"
  ))
  out <- filter_graph_doc(df, cfg)
  expect_equal(sort(unique(out$sample_rep)), 1L)
  expect_setequal(graph_doc_pos_slug(out$graph_pos_patterns), c("adjectives", "ism"))
  expect_setequal(unique(out$predictive_model_engine), "glmnet")
  # rep 1 x {glmnet} x {adjectives, ism} = 2 chunks.
  expect_equal(nrow(out), 2L)
})

test_that("engine filter only narrows predictive rows, keeps svd/ppmi", {
  df <- make_graph_df()
  cfg <- resolve_graph_doc_config(list(reps = "all", engines = "glmnet"))
  out <- filter_graph_doc(df, cfg)
  # xgboost predictive rows dropped; svd and ppmi rows (NA engine) retained.
  expect_false("xgboost" %in% out$predictive_model_engine)
  expect_true(any(is.na(out$predictive_model_engine)))
  expect_setequal(graph_doc_weight_family(out), c("predictive", "svd", "ppmi"))
})

test_that("weight_types = predictive drops svd/ppmi rows entirely", {
  df <- make_graph_df()
  cfg <- resolve_graph_doc_config(list(reps = "all", weight_types = "predictive"))
  out <- filter_graph_doc(df, cfg)
  expect_setequal(graph_doc_weight_family(out), "predictive")
})

# --- store resolution in the generated document (W4 repair) -------------------
# Quarto executes chunks with the document directory (Paper/) as cwd for
# standalone renders, so the emitted setup block must resolve the store without
# here::here()/tar_config_set(). These tests eval the emitted lines directly.

eval_store_lines <- function(envir = new.env(parent = baseenv())) {
  eval(parse(text = graph_doc_store_setup_lines()), envir = envir)
  envir$targets_store
}

test_that("emitted store block resolves ../_targets when cwd is Paper/", {
  root <- tempfile("storeres")
  dir.create(file.path(root, "_targets"), recursive = TRUE)
  dir.create(file.path(root, "Paper"))
  old_wd <- setwd(file.path(root, "Paper"))
  on.exit(setwd(old_wd), add = TRUE)

  expect_equal(
    eval_store_lines(),
    normalizePath(file.path(root, "_targets"), winslash = "/")
  )
})

test_that("emitted store block resolves _targets when cwd is the project root", {
  root <- tempfile("storeres")
  dir.create(file.path(root, "_targets"), recursive = TRUE)
  old_wd <- setwd(root)
  on.exit(setwd(old_wd), add = TRUE)

  expect_equal(
    eval_store_lines(),
    normalizePath(file.path(root, "_targets"), winslash = "/")
  )
})

test_that("emitted store block honors a non-empty TARGETS_STORE env var", {
  root <- tempfile("storeres")
  store <- file.path(root, "elsewhere", "_targets")
  dir.create(store, recursive = TRUE)
  Sys.setenv(TARGETS_STORE = store)
  on.exit(Sys.unsetenv("TARGETS_STORE"), add = TRUE)

  expect_equal(eval_store_lines(), normalizePath(store, winslash = "/"))
})

test_that("emitted store block fails loudly when no store exists", {
  # Nested dir with no `_targets` in itself or its parent.
  bottom <- file.path(tempfile("storeres"), "a", "b")
  dir.create(bottom, recursive = TRUE)
  old_wd <- setwd(bottom)
  on.exit(setwd(old_wd), add = TRUE)

  expect_error(eval_store_lines(), "targets data store not found")
})

test_that("generated qmd carries explicit store args, no tar_config_set / execute-dir", {
  df <- tibble::tibble(
    sample_rep = 1L,
    feature_name = "democracy",
    graph_pos_patterns = "(_jj|_JJ)",
    sample_max_vols = 500,
    sample_type = "restricted",
    dfm_to_lower = TRUE,
    predictive_model_engine = "glmnet",
    predictive_model_dfm_weight = "ppmi",
    svd_dims = NA_integer_,
    svd_weight = NA_character_,
    ppmi_fun = NA_character_,
    graph_object = list(rlang::sym("graph_weight_test123"))
  )
  out_dir <- tempfile("qmdout")
  path <- write_run_graph_qmd(df, run_id = "test_run", output_dir = out_dir)
  qmd <- readLines(path)

  # No config indirection, no stray sidecar yaml, no misleading front matter.
  # (Comment lines excluded: the setup chunk's explanatory comment may name
  # tar_config_set when describing why it is avoided.)
  code_lines <- qmd[!grepl("^\\s*#", qmd)]
  expect_false(any(grepl("tar_config_set", code_lines)))
  expect_false(any(grepl("custom.yaml", code_lines, fixed = TRUE)))
  expect_false(any(grepl("execute-dir", qmd)))

  # Fallback resolution logic present, with the loud existence assertion.
  expect_true(any(grepl("TARGETS_STORE", qmd)))
  expect_true(any(grepl('file.path("..", "_targets")', qmd, fixed = TRUE)))
  expect_true(any(grepl("targets data store not found", qmd)))

  # Every emitted tar_read_raw() call passes the resolved store explicitly.
  raw_calls <- grep("tar_read_raw\\(", qmd, value = TRUE)
  expect_gt(length(raw_calls), 0)
  expect_true(all(grepl("store = targets_store", raw_calls)))
  expect_true(any(grepl("graph_weight_test123", raw_calls)))
})
