
# Perform nested cross-validation to estimate generalization error

# Setup -----------

message(Sys.info()[['nodename']])

if (interactive()) {
  cmd_args <- list(
    # model_type = "xgb_1_order",
    model_type = "log_elastic",
    dataset = "compas",
    bayesian = FALSE
  )
} else {
  command_args <- commandArgs(trailingOnly = TRUE)
  cmd_args <- list(
    model_type = command_args[1],
    dataset = command_args[2],
    bayesian = as.logical(command_args[3])
  )
}

message(paste("Running", cmd_args$model_type, "on", cmd_args$dataset))
message(paste("Bayesian modeling?", cmd_args$bayesian))


# load global setting and functions
source(here::here("R", "0_setup.R"))

# get compas data
dd <- define_data(type = cmd_args$dataset)

# set model
dm <- define_model(type = cmd_args$model_type, data_info = dd)


# Resampled estimation -----------

# run in parallel
# comment out if you get any issues
# see https://tune.tidymodels.org/articles/extras/optimizations.html
# Detect number of cores
all_cores <- parallel::detectCores(logical = FALSE)
cl <- parallel::makePSOCKcluster(all_cores)
doParallel::registerDoParallel(cl)

# tune using nested cv
tn <- tune_nested(
  data_splits = dd$ncv, 
  response = dd$response, 
  task = dd$task, 
  model_info = dm,
  bayesian_tuning = cmd_args$bayesian
)

# Close the cluster
parallel::stopCluster(cl)

# summarise metrics from nested cv (estimate generalization error)
sm <- summarise_metrics(tn)


# Storage ---------

# store nested tuned results in project folder 
# (see `storage_folder` variable in 0_setup.R)

name_base <- paste0(cmd_args$dataset, "-", cmd_args$model_type)
if (cmd_args$bayesian) {
  name_base <- paste0(name_base, "_bayes")
}

store_results(tn, paste0(name_base, "-tuned-ncv.rds"))
store_metrics(sm, paste0(name_base, "-metrics-ncv.csv"))

# read via:
# tn <- load_results("compas-rpart-tuned-ncv.rds")

message("Job done")
