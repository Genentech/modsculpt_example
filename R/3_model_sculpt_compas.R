

# setup ----------

source(here::here("R", "0_setup.R"))

# set dataset (any with discrete response)
dataset <- "compas"

# set model_type
model_type = "xgb"

# set nr of features for a polished model
top_k <- 3

# util function for storage
sname <- function(x, prefix = dataset) {
  paste0(prefix, "-", x)
}

# logit function
logit <- function(x) log(x / (1-x))
inv.logit <- function(x) 1 / (1 + exp(-x))


# load stored data -------------

# load dataset
dd <- define_data(dataset)

# set model
dm <- define_model(type = model_type, data_info = dd)


# load xgb
# model_type <- paste0(model_type, "_bayes")
fit_xgb <- load_results(sname(paste0(model_type, "-fit_final.rds")))


# model sculpting for the final model -------------

# get product marginals
pm <- sample_marginals(dd$data$train[dd$covariates$all], n = 1e4, seed = 1234)

# get rough model - on pm
rs_pm <- sculpt_rough(
  pm,
  seed = 1234,
  model_predict_fun = function(x) {
    p <- predict(fit_xgb, new_data = x, type = "prob")$.pred_1
    logit(p)
  }
)

ds_pm <- sculpt_detailed_gam(rs_pm)

ps_pm <- sculpt_polished(ds_pm, k = top_k)


# evaluate on holdout

eval_holdout_sculpts <- function(model_sculpt) {
  eval_holdout(
    holdout = dd$data$holdout, 
    response = dd$response, 
    prediction_type = if_else(dd$task == "classification", "prob", "numeric"),
    model_final = model_sculpt, 
    metrics = dm$metrics
  )
}

eh_rs <- eval_holdout_sculpts(rs_pm)
eh_ds <- eval_holdout_sculpts(ds_pm)
eh_ps <- eval_holdout_sculpts(ps_pm)

store_metrics(eh_rs, sname(paste0(model_type, "-sculpt_rough-metrics-holdout.csv")))
store_metrics(eh_ds, sname(paste0(model_type, "-sculpt_detailed-metrics-holdout.csv")))
store_metrics(eh_ps, sname(paste0(model_type, "-sculpt_polished-metrics-holdout.csv")))

store_results(rs_pm, sname(paste0(model_type, "-sculpt_rough_pm.rds")))
store_results(ds_pm, sname(paste0(model_type, "-sculpt_detailed_pm.rds")))
store_results(ps_pm, sname(paste0(model_type, "-sculpt_polished_pm.rds")))

