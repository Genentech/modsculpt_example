
# Tuning for strong learner models

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


# Tuning ------------------

# # run in parallel
# # comment out if you get any issues
# # see https://tune.tidymodels.org/articles/extras/optimizations.html
# if (packageVersion("tune") >= "1.2.0" &
#     suppressWarnings(require("future", quietly = TRUE))) {
#   future::plan(multicore)
# }

# n_cores <- parallel::detectCores()
all_cores <- parallel::detectCores(logical = FALSE)
# cl <- parallel::makeForkCluster(n_cores)
cl <- parallel::makePSOCKcluster(all_cores)
doParallel::registerDoParallel(cl)

# tune and fit on whole train

message("")
message("Tuning..")
out_tune_all_train <- 
  tuning_step(model_info = dm, 
              data_resample = dd$cv,
              data_all = dd$data$train,
              bayesian_tuning = cmd_args$bayesian)

tg <- out_tune_all_train$tg
best_params <- out_tune_all_train$best_params
ff <- out_tune_all_train$model


# evaluate on holdout 
eh <- eval_holdout(
  holdout = dd$data$holdout, 
  response = dd$response, 
  prediction_type = if_else(dd$task == "classification", "prob", "numeric"),
  model_final = ff, 
  metrics = dm$metrics
)

# Close the cluster
parallel::stopCluster(cl)

# Storage ---------

# store tuned results in project folder 
# (see `storage_folder` variable in 0_setup.R)

name_base <- paste0(cmd_args$dataset, "-", cmd_args$model_type)
if (cmd_args$bayesian) {
  name_base <- paste0(name_base, "_bayes")
}

store_results(tg, paste0(name_base, "-tuned-cv.rds"))
store_metrics(eh, paste0(name_base, "-metrics-holdout.csv"))
store_results(ff, paste0(name_base, "-fit_final.rds"))


message("Job done")
