# Sampling repeats for uncertainty quantification (Phase 3, P3) and the
# sampled_htids reproducibility export (P4).
#
# Grid-level tests only: object_parameters.R is sourced under temp split
# configs (a copy of config/ plus a test run YAML) so we can vary
# samples.n_repeats / samples.repeats_scope without touching the repo config.
# Covered: (1) rep 1 keeps the empty label so default target names are
# byte-identical; (2) repeats_scope gates which sample rows replicate
# (headline = restricted at the max volume cap only, all = every row); (3)
# kl/entropy target names carry the rep label for reps >= 2 (no collisions);
# (4) the OOD performance join pairs every rep's models with the rep-1
# unrestricted testing DFM.

suppressWarnings(suppressMessages({
  library(dplyr)
  library(stringr)
}))

# Source object_parameters.R into a fresh environment under a temp config dir
# whose runs/test_repeats.yml sets the requested sampling-repeat overrides.
source_params_with_config <- function(n_repeats = NULL, repeats_scope = NULL) {
  root <- here::here()
  tmp <- file.path(tempdir(), paste0("repcfg_", n_repeats %||% "d", "_", repeats_scope %||% "d"))
  if (!dir.exists(tmp)) {
    dir.create(tmp, recursive = TRUE)
    file.copy(file.path(root, "config"), tmp, recursive = TRUE)
  }
  samples <- list()
  if (!is.null(n_repeats)) samples$n_repeats <- n_repeats
  if (!is.null(repeats_scope)) samples$repeats_scope <- repeats_scope
  run_cfg <- list(
    id = "test_repeats",
    description = "P3 repeat tests",
    features = list("democracy"),
    outputs = list("ingest", "dfm", "model")
  )
  if (length(samples)) run_cfg$samples <- samples
  yaml::write_yaml(run_cfg, file.path(tmp, "config", "runs", "test_repeats.yml"))

  e <- new.env(parent = globalenv())
  withr::with_envvar(
    c(TARGET_RUN = "test_repeats", TARGET_PROFILE = "full", TARGET_CONFIG = ""),
    withr::with_dir(tmp, source(file.path(root, "object_parameters.R"), local = e))
  )
  e
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# Shared fixtures (sourcing the grids is the slow part; do it once per config).
params_default <- source_params_with_config()                # corpus default: n_repeats 1
params_rep3 <- source_params_with_config(3, "headline")
params_rep3_all <- source_params_with_config(3, "all")

test_that("n_repeats = 1 leaves the sample grid rep-free with empty labels", {
  s <- params_default$sample_df
  expect_true(all(s$sample_rep == 1L))
  expect_true(all(s$sample_rep_label == ""))
  expect_false(any(str_detect(s$sample_id, "rep\\d")))
})

test_that("rep-1 target names are byte-identical to the unreplicated config", {
  rep1 <- params_rep3$sample_df %>% filter(sample_rep == 1L)
  expect_identical(sort(rep1$sample_id), sort(params_default$sample_df$sample_id))
  # And the rep-1 kl/entropy names are unchanged too (no label leakage).
  kl1 <- params_rep3$kl_df %>% filter(sample_rep == 1L)
  expect_identical(sort(kl1$kl_id), sort(params_default$kl_df$kl_id))
  ent1 <- params_rep3$entropy_df %>% filter(sample_rep == 1L)
  expect_identical(sort(ent1$entropy_id), sort(params_default$entropy_df$entropy_id))
})

test_that("headline scope replicates only the restricted chain at the max volume cap", {
  s <- params_rep3$sample_df
  max_cap <- max(s$sample_max_vols)
  reps <- s %>% filter(sample_rep >= 2L)
  expect_true(all(reps$sample_type == "restricted"))
  expect_true(all(reps$sample_max_vols == max_cap))
  # One eligible row per workset gains (n_repeats - 1) extra rows.
  eligible <- params_default$sample_df %>%
    filter(sample_type == "restricted", sample_max_vols == max_cap)
  expect_equal(nrow(s), nrow(params_default$sample_df) + 2L * nrow(eligible))
  expect_equal(sort(unique(reps$sample_rep_label)), c("rep2", "rep3"))
  expect_true(all(str_detect(reps$sample_id, "rep[23]")))
})

test_that("all scope replicates every sample row", {
  s <- params_rep3_all$sample_df
  expect_equal(nrow(s), 3L * nrow(params_default$sample_df))
  expect_equal(
    s %>% count(sample_rep) %>% pull(n),
    rep(nrow(params_default$sample_df), 3L)
  )
})

test_that("kl and entropy names carry the rep label for reps >= 2 and never collide", {
  kl <- params_rep3$kl_df
  ent <- params_rep3$entropy_df
  expect_false(any(duplicated(kl$kl_id)))
  expect_false(any(duplicated(ent$entropy_id)))
  kl2 <- kl %>% filter(sample_rep >= 2L)
  ent2 <- ent %>% filter(sample_rep >= 2L)
  expect_gt(nrow(kl2), 0L)
  expect_gt(nrow(ent2), 0L)
  expect_true(all(str_detect(kl2$kl_id, "rep[23]$")))
  expect_true(all(str_detect(ent2$entropy_id, "rep[23]$")))
  expect_false(any(str_detect(filter(kl, sample_rep == 1L)$kl_id, "rep\\d")))
  expect_false(any(str_detect(filter(ent, sample_rep == 1L)$entropy_id, "rep\\d")))
})

test_that("OOD performance pairs every rep's models with the rep-1 testing DFM", {
  ood <- params_rep3$performance_wild_sample_df
  expect_setequal(unique(ood$sample_rep), 1:3)
  # The testing side is rep-1 only: no rep fragment in any testing DFM id, and
  # reps >= 2 use exactly the testing DFMs rep 1 uses.
  expect_false(any(str_detect(ood$testing_dfm_id, "rep\\d")))
  rep1_ids <- unique(ood$testing_dfm_id[ood$sample_rep == 1L])
  rep2plus_ids <- unique(ood$testing_dfm_id[ood$sample_rep >= 2L])
  expect_true(all(rep2plus_ids %in% rep1_ids))
})

test_that("sampled_htids is a headline bundle target (P4)", {
  expect_true("sampled_htids" %in% headline_target_names())
})
