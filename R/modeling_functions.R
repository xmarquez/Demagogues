train_test_splits <-function(dfm, feat) {
  UseMethod("train_test_splits", feat)
}

train_test_splits.dictionary2 <- function(dfm, feat) {
  df <- dfm |>
    quanteda::dfm_lookup(feat) |>
    quanteda::dfm_weight("boolean") |>
    quanteda::convert(to = "data.frame")

  rsample::initial_split(df, strata = names(feat))
}

train_test_splits.character <- function(dfm, feat) {
  df <- dfm |>
    quanteda::dfm_select(feat) |>
    quanteda::dfm_weight("boolean") |>
    quanteda::convert(to = "data.frame")

  rsample::initial_split(df, strata = feat)
}
