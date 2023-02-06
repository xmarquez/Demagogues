# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tidyverse)
library(hathiTools)
# library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("tibble", "magrittr"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multiprocess")

# tar_make_future() configuration (okay to leave alone):
future::plan(future.batchtools::batchtools_slurm, template = here::here("slurm.tmpl"),
             resources = list(partition = "quicktest", ncpus = 2,
                              memory = "2G",
                              walltime = "1:00:00"))

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
# Replace the target list below with your own:
list(

  # Decade selection --------------------------------------------------------


  tar_target(
    name = decades,
    command = seq(1700, 2010, by = 10),
    deployment = "main"
  ),

  # Tracking of big catalog HathiFile ---------------------------------------

  tar_target(
    name = hathi_catalog,
    command = here::here("raw-hathifiles/hathi_full_20230101.txt.gz"),
    format = "file",
    deployment = "main"
  ),

  # Workset creation and metadata addition ----------------------------------

  tar_target(
    name = demagogue_worksets,
    command = workset_builder("demagogue", pub_date = decades:(decades+9)) %>%
      mutate(decade = decades),
    pattern = map(decades),
    deployment = "main"
  ),



  tar_target(
    name = demagogue_worksets_meta,
    command = demagogue_worksets %>%
      dplyr::left_join(load_raw_hathifile(hathi_catalog)),
    resources = tar_resources(future = tar_resources_future(
      resources = list(partition = "parallel", memory = "8G")))
  ),

  tar_target(
    name = demagogue_usable_htids,
    command = demagogue_worksets_meta %>%
      dplyr::filter(rights_date_used >= decade, rights_date_used < decade+10,
                    decade == decades) %>%
      dplyr::mutate(rights_date_used2 = stringr::str_extract(imprint, "[0-9]{4}") %>%
                      as.double()) %>%
      dplyr::filter(rights_date_used2 <= rights_date_used,
                    lang == "eng"),
    pattern = map(decades),
    resources = tar_resources(future = tar_resources_future(
      resources = list(partition = "parallel", memory = "4G")))
  )
)
