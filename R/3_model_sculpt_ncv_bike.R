

# setup ----------

source(here::here("R", "0_setup.R"))

# set dataset (any with discrete response)
dataset <- "bike"

# set model_type
model_type = "xgb"

# set nr of features for a polished model
top_k <- 3

# util function for storage
sname <- function(x, prefix = dataset) {
  paste0(prefix, "-", x)
}


# load stored data -------------

# load dataset
dd <- define_data(dataset)


# model sculpting for the nested cross-validation models -------------

dm <- define_model(model_type, dd)

# 1) load stored ncv
# model_type <- paste0(model_type, "_bayes")
ncv <- load_results(sname(paste0(model_type, "-tuned-ncv.rds")))
out <- vector("list", nrow(ncv))


message("Calculating metrics for resampled sculptures")
out <- map(1:nrow(ncv), function(i) {
  
  # 2) load / re-fit the model on training(ncv$splits[[i]])
  # - ncv$models has the model stored (ncv$best_params has the params if in need to re-fit)
  mod_i <- ncv$models[[i]]
  bp_i <- ncv$best_params[[i]]
  tr_i <- training(ncv$splits[[i]])
  if (!workflows::is_trained_workflow(mod_i)) {
    fw_i <- finalize_workflow(dm$workflow, bp_i)
    mod_i <- fit(fw_i, tr_i)
  }
  
  # 3) create a product marginals set from training(ncv$splits[[i]]) and sculpt the model
  pm_i <- sample_marginals(tr_i, n = 1e3, seed = 84329)
  rs_i <- sculpt_rough(
    dat = pm_i, 
    model_predict_fun = function(x) {
      predict(mod_i, new_data = x)$.pred
    },
    n_ice = 10, 
    seed = 142324
  )
  
  ds_i <- sculpt_detailed_gam(rs_i)
  ps_i <- sculpt_polished(ds_i, k = top_k)
  
  # 4) get the predictions on testing(ncv$splits[[i]])
  te_i <- testing(ncv$splits[[i]])
  obs_i <- te_i[[dd$response]]
  pred_rs_i <- predict(rs_i, te_i)
  pred_ds_i <- predict(ds_i, te_i)
  pred_ps_i <- predict(ps_i, te_i)
  cm_rs_i <- fit_calibration(obs = obs_i, preds = pred_rs_i)
  cm_ds_i <- fit_calibration(obs = obs_i, preds = pred_ds_i)
  cm_ps_i <- fit_calibration(obs = obs_i, preds = pred_ps_i)
  out[[i]] <- tibble(
    i = i,
    id = ncv$id[[i]],
    id2 = ncv$id2[[i]],
    model = rep(c(paste0(model_type, "-sculpt_rough"), 
                  paste0(model_type, "-sculpt_detailed"), 
                  paste0(model_type, "-sculpt_polished")), 
                each = length(obs_i)),
    # obs = rep(obs_i, 3),
    obs = rep(obs_i, 3),
    preds = c(pred_rs_i, pred_ds_i, pred_ps_i),
    preds_calib = c(
      as.numeric(predict(cm_rs_i, type = "response")), 
      as.numeric(predict(cm_ds_i, type = "response")),
      as.numeric(predict(cm_ps_i, type = "response"))
    )
  )
}, .progress = TRUE)
res <- bind_rows(out)


# metrics for sculptures on resampled train set
metrics_resamples_sculptures <- res %>% 
  group_by(model, id, id2) %>% 
  nest() %>% 
  mutate(df_metrics = map(data, ~calc_metrics(dat = .x, task = dd$task, metrics = dm$metrics))) %>%
  select(-data) %>%
  unnest(df_metrics) %>% 
  group_by(model, id) %>% 
  summarise(across(where(is.numeric), mean), .groups = "drop_last") %>% # mean over folds
  summarise(across(where(is.numeric), mean), .groups = "drop_last") # mean over repeats

metrics_resamples_sculptures %>% 
  filter(model == paste0(model_type, "-sculpt_rough")) %>%
  store_metrics(sname(paste0(model_type, "-sculpt_rough-metrics-ncv.csv")))
metrics_resamples_sculptures %>%
  filter(model == paste0(model_type, "-sculpt_detailed")) %>%
  store_metrics(sname(paste0(model_type, "-sculpt_detailed-metrics-ncv.csv")))
metrics_resamples_sculptures %>%
  filter(model == paste0(model_type, "-sculpt_polished")) %>%
  store_metrics(sname(paste0(model_type, "-sculpt_polished-metrics-ncv.csv")))


