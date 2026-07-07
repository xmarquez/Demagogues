# EF rsync path correction (hathiTools 0.2.0 encoding bug workaround).
#
# hathiTools builds remote paths without the `.` -> `,` substitution the HTRC
# rsync tree uses, so dotted local ids (miun.*, miua.*) 404. ef_remote_path()
# reproduces hathiTools' stubbytree layout but with the correct comma encoding;
# for htids WITHOUT dots in the local id the two must agree exactly.
source(here::here("R", "fast_dfm_functions.R"))

test_that("ef_remote_path matches the live-tree layout for dotted Michigan ids", {
  # Ground truth confirmed against data.analytics.hathitrust.org on 2026-07-07.
  expect_identical(
    ef_remote_path("miun.abj7655.0004.001"),
    "miun/a750,1/miun.abj7655,0004,001.json.bz2"
  )
})

test_that("ef_remote_path agrees with hathiTools for htids without dotted local ids", {
  skip_if_not_installed("hathiTools")
  undotted <- c("mdp.39015012345678", "uc1.b3342759", "coo.31924013523232")
  expect_identical(
    ef_remote_path(undotted),
    unname(vapply(undotted, hathiTools:::stubby_url_to_rsync, character(1)))
  )
})

test_that("ef_remote_path applies the full pairtree character encoding", {
  # ark-style ids carry ':' and '/' which encode to '+' and '='.
  p <- ef_remote_path("coo1.ark:/13960/t2z32f26q")
  expect_match(p, "^coo1/", fixed = FALSE)
  expect_match(p, "coo1\\.ark\\+=13960=t2z32f26q\\.json\\.bz2$")
  expect_false(grepl(":", p) || grepl("=json", p))
})

test_that("ef_remote_path is vectorized and preserves order", {
  ids <- c("miun.abj7655.0004.001", "mdp.39015012345678")
  out <- ef_remote_path(ids)
  expect_length(out, 2L)
  expect_match(out[1], ",")
  expect_false(grepl(",", out[2]))
})
