
if (!interactive()) options(tidymodels.quiet = TRUE)

library(tidymodels)
library(mgcv)
library(modsculpt) 
# remotes::install_github(repo = "genentech/modsculpt", ref = "v0.1")
library(stats4phc)
# remotes::install_github(repo = "genentech/stats4phc", ref = "v0.1.1")

source(here::here("R", "functions.R"))

# output folder is needed for other outputs like figures, so keep this line
if (!dir.exists(here::here("output"))) dir.create(here::here("output"))

# You can change the storage folder to a different location
storage_folder <- here::here("output", "train_results")
if (!dir.exists(here::here("output", "train_results"))) dir.create(here::here("output", "train_results"))

