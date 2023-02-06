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


  # Workset creation and metadata addition ----------------------------------

  tar_target(
    name = demagogue_worksets,
    command = workset_builder("demagogue", pub_date = decades:(decades+9)) %>%
      mutate(decade = decades),
    pattern = map(decades),
    deployment = "main"
  )
)
