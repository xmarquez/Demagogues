# Source the plain R/ scripts needed by the tests. This project is a targets
# pipeline (not a package), so there is no namespace to load; we source the
# relevant files directly, resolving paths from the project root via here::here().
suppressPackageStartupMessages(library(dplyr))
source(here::here("R", "model_weights.R"))
source(here::here("R", "dfm_functions.R"))
source(here::here("R", "correlation_functions.R"))
source(here::here("R", "export_functions.R"))
source(here::here("R", "code_generation.R"))
