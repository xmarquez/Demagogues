# make_results_bundle() / headline_target_names() -----------------------------

make_fake_store <- function(root, objects) {
  dir.create(file.path(root, "_targets", "meta"), recursive = TRUE)
  dir.create(file.path(root, "_targets", "objects"), recursive = TRUE)
  writeLines("fake targets metadata", file.path(root, "_targets", "meta", "meta"))
  for (obj in objects) {
    saveRDS(obj, file.path(root, "_targets", "objects", obj))
  }
  invisible(root)
}

test_that("headline_target_names returns unique names incl. feature counts", {
  nms <- headline_target_names()
  expect_type(nms, "character")
  expect_false(any(duplicated(nms)))
  expect_true("combined_performance" %in% nms)
  expect_true("rank_agreements" %in% nms)
  expect_true("workset_volume_count_democracy" %in% nms)
  expect_true("workset_meta_volume_count_democracy" %in% nms)

  auth <- headline_target_names(features = "authority")
  expect_true("workset_volume_count_authority" %in% auth)
  expect_false("workset_volume_count_democracy" %in% auth)
})

test_that("make_results_bundle tars meta plus existing objects, warns on missing", {
  root <- file.path(tempdir(), paste0("fake_store_", as.integer(stats::runif(1, 1, 1e8))))
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  present <- c("combined_performance", "sample_sizes")
  make_fake_store(root, present)

  wanted <- c(present, "all_model_weights")  # one missing on purpose
  expect_warning(
    bundle <- make_results_bundle(
      dir = "exports",
      targets = wanted,
      root = root,
      run = "test_run"
    ),
    regexp = "all_model_weights"
  )

  expect_true(file.exists(bundle))
  expect_match(basename(bundle), "^results_test_run_\\d{8}-\\d{6}\\.tar\\.gz$")

  contents <- utils::untar(bundle, list = TRUE)
  contents <- gsub("\\\\", "/", contents)
  expect_true("_targets/meta/meta" %in% contents)
  expect_true(all(file.path("_targets", "objects", present) %in% contents))
  expect_false("_targets/objects/all_model_weights" %in% contents)
})

test_that("extracting the bundle into another project root restores the files", {
  root <- file.path(tempdir(), paste0("fake_store2_", as.integer(stats::runif(1, 1, 1e8))))
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  make_fake_store(root, "combined_kl")

  bundle <- make_results_bundle(
    dir = "exports",
    targets = "combined_kl",
    root = root,
    run = "roundtrip"
  )

  dest <- file.path(tempdir(), paste0("dest_", as.integer(stats::runif(1, 1, 1e8))))
  dir.create(dest)
  on.exit(unlink(dest, recursive = TRUE), add = TRUE)
  utils::untar(bundle, exdir = dest)

  expect_true(file.exists(file.path(dest, "_targets", "meta", "meta")))
  restored <- readRDS(file.path(dest, "_targets", "objects", "combined_kl"))
  expect_identical(restored, "combined_kl")
})

test_that("make_results_bundle errors when the store metadata is absent", {
  root <- file.path(tempdir(), paste0("empty_store_", as.integer(stats::runif(1, 1, 1e8))))
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  expect_error(
    make_results_bundle(root = root, run = "nope"),
    regexp = "metadata"
  )
})
