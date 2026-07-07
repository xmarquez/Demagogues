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

# rendered_document_files() ---------------------------------------------------

make_fake_paper <- function(root) {
  paper <- file.path(root, "Paper")
  dir.create(paper, recursive = TRUE)
  writeLines("paper", file.path(paper, "The_Paper.md"))
  writeLines("<html></html>", file.path(paper, "The_Paper.html"))
  writeLines("%PDF", file.path(paper, "The_Paper.pdf"))
  fig_dir <- file.path(paper, "The_Paper_files", "figure-html")
  dir.create(fig_dir, recursive = TRUE)
  writeLines("png", file.path(fig_dir, "plot-1.png"))
  writeLines("notes", file.path(paper, "notes.txt"))  # unrelated, must be skipped
  invisible(root)
}

test_that("rendered_document_files finds md/html/pdf plus _files contents, relative", {
  root <- file.path(tempdir(), paste0("paper_", as.integer(stats::runif(1, 1, 1e8))))
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  make_fake_paper(root)

  rel <- rendered_document_files(root)
  expect_true("Paper/The_Paper.md" %in% rel)
  expect_true("Paper/The_Paper.html" %in% rel)
  expect_true("Paper/The_Paper.pdf" %in% rel)
  expect_true("Paper/The_Paper_files/figure-html/plot-1.png" %in% rel)
  expect_false(any(grepl("notes.txt", rel)))
  expect_false(any(grepl("^([A-Za-z]:)?/", rel)))  # all paths are relative
})

test_that("rendered_document_files returns character(0) when Paper is absent", {
  root <- file.path(tempdir(), paste0("nopaper_", as.integer(stats::runif(1, 1, 1e8))))
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  expect_identical(rendered_document_files(root), character(0))
})

test_that("make_results_bundle includes rendered documents when present", {
  root <- file.path(tempdir(), paste0("doc_store_", as.integer(stats::runif(1, 1, 1e8))))
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  make_fake_store(root, "combined_performance")
  make_fake_paper(root)

  bundle <- make_results_bundle(
    dir = "exports",
    targets = "combined_performance",
    root = root,
    run = "with_docs"
  )
  contents <- gsub("\\\\", "/", utils::untar(bundle, list = TRUE))
  expect_true("Paper/The_Paper.md" %in% contents)
  expect_true("Paper/The_Paper.html" %in% contents)
  expect_true("Paper/The_Paper.pdf" %in% contents)
  expect_true("Paper/The_Paper_files/figure-html/plot-1.png" %in% contents)
  expect_false(any(grepl("notes.txt", contents)))
})
