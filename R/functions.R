

source(here::here("R", "define_data.R"))
source(here::here("R", "define_model.R"))


#' Tune and evaluate model using nested cv
#'
#' @param data_splits Nested CV.
#' @param response Response.
#' @param task "classification" or "regression".
#' @param model_info Model definition, result of `define_model`.
#' @param use_corel Use corel algorithm or tidymodels?
#' @param bayesian_tuning Use bayesian tuning when running tidymodels?
#' @param ... Additional parameters (currently not used).
#'
#' @return Tibble with `data_splits` and results for nested cv.
#'
tune_nested <- function(data_splits, response, task, model_info, 
                        bayesian_tuning = FALSE) {
  
  stopifnot(
    inherits(data_splits, "nested_cv")
  )
  
  first_metric <- names(attr(model_info$metrics, "metrics"))[1]
  
  message("")
  message("Running nested cross-validation")
  # tune model on inner loops
  
  options_progress <- 
    list(name = "- Outer fold",
         format = "{cli::pb_name}: {cli::pb_current}/{cli::pb_total} | ETA: {cli::pb_eta}")
  
  list_tuning <-
    map2(data_splits$inner_resamples, data_splits$splits, 
         \(resample, split) {
           tuning_step(model_info, resample, training(split), bayesian_tuning)
         }, .progress = options_progress)
  
  models <- map(list_tuning, "model")
  best_params <- map(list_tuning, "best_params")
  tgs <- map(list_tuning, "tg")
  
  # evaluate on outer test sets (i.e. holdouts)
  holdout_results <- map_dfr(
    1:nrow(data_splits), 
    \(i) eval_holdout(
      holdout = testing(data_splits$splits[[i]]),
      response = response, 
      prediction_type = ifelse(task == "classification", "prob", "numeric"),
      model_final = models[[i]], 
      metrics = model_info$metrics
    )
  )
  
  # return updated tibble
  ncv_evaluated <- tibble(
    select(data_splits, splits, id, id2),
    tune_grids = tgs,
    models = models,
    best_params = best_params,
    holdout_results
  )
  
  return(ncv_evaluated)
}




#' Perform grid and bayesian tuning on model
#' 
#' @param model_info Model definition, result of `define_model`.
#' @param data_resample Resamples for tuning.
#' @param data_all All data for final fit.
#' @param bayesian_tuning Use bayesian tuning when running tidymodels?
#' 
#' @return List with the final model, best parameters, and tuning grid.
#' 
tuning_step <- function(model_info, data_resample, data_all, bayesian_tuning = FALSE) {
  
  first_metric <- names(attr(model_info$metrics, "metrics"))[1]
  
  # tuning in nested cv for tunable models
  if (nrow(tune::tune_args(model_info$workflow)) > 0) {
    
    # tune grid using i-th inner cv
    tg <- tune_grid(
      object = model_info$workflow,
      resamples = data_resample,
      param_info = model_info$param_info,
      grid = model_info$param_grid, 
      metrics = model_info$metrics,
      control = control_grid(
        verbose = FALSE, allow_par = TRUE, parallel_over = "resamples"
      )
    )
    
    # tune bayes if requested
    if (bayesian_tuning) {
      
      message("-- Bayesian tuning")
      
      # xgb, lgb: partially finalize workflow to speed up bayesian tuning
      if (extract_spec_parsnip(model_info$workflow)$engine == "xgboost") {
        bst <- select_best(tg, metric = first_metric)
        no_tune <- c("min_n", "sample_size", "stop_iter", "alpha", "lambda")
        wf <- finalize_workflow(model_info$workflow, bst[no_tune])
        pi <- filter(model_info$param_info, !id %in% no_tune)
        
      } else if (extract_spec_parsnip(model_info$workflow)$engine == "lightgbm") {
        bst <- select_best(tg, metric = names(attr(model_info$metrics, "metrics")[1]))
        no_tune <- c("min_n", "tree_depth", "sample_size", "stop_iter", "lambda_l1", "lambda_l2")
        wf <- finalize_workflow(model_info$workflow, bst[no_tune])
        pi <- filter(model_info$param_info, !id %in% no_tune)
        
      } else {
        wf <- model_info$workflow
        pi <- model_info$param_info
        bst <- NULL
      }
      
      tg <- tune_bayes(
        object = wf,
        resamples = data_resample,
        iter = 30,
        param_info = pi,
        metrics = model_info$metrics,
        initial = tg,
        control = control_bayes(
          verbose = TRUE, allow_par = TRUE, parallel_over = "everything",
          no_improve = 10
        )
      )
    } else {
      bst <- NULL
    }
    
    # refit on whole outer train and store results
    if (is.null(bst)) {
      best_params_i <- select_best(tg, metric = first_metric)  
    } else {
      best_params_i <- bind_cols(
        select(select_best(tg, metric = first_metric), -any_of(no_tune)),
        select(bst, all_of(no_tune))
      ) %>% select(all_of(colnames(bst))) # relocate columns
    }
    workflow_final <- finalize_workflow(model_info$workflow, best_params_i)
    model <- fit(workflow_final, data = data_all)
    best_params <- best_params_i
    
  } else {
    
    # no tuning needed
    model <- fit(model_info$workflow, data = data_all)
    best_params <- data.frame()
    tg <- data.frame()  
  }
  
  out <- list(
    model = model,
    best_params = best_params,
    tg = tg
  )
  
  return(out)
}


#' Evaluate metrics on holdout data.
#'
#' @param holdout Holdout set.
#' @param response Response.
#' @param prediction_type Prediction type
#' @param model_final Re-fit final model.
#' @param metrics `metric_set` from `define_model`
#'
#' @return Tibble with metrics and predictions.
#'
eval_holdout <- function(holdout, response, prediction_type, model_final, metrics) {
  
  stopifnot(
    is.data.frame(holdout),
    is.character(response),
    is.character(prediction_type),
    inherits(model_final, "workflow") | inherits(model_final, "sculpture"),
    inherits(metrics, "metric_set")
  )
  
  # set variables based on prediction_type
  if (prediction_type == "prob") {
    task <- "classification"
    prediction_column <- ".pred_1"
  } else if (prediction_type == "class") {  
    task <- "classification"
    prediction_column <- ".pred_class"
  } else if (prediction_type == "numeric") {
    task <- "regression"
    prediction_column <- ".pred"
  } else {
    stop("Unknown prediction type")
  }
  
  if (inherits(model_final, "workflow")) {
    
    # generate predictions on holdout - workflow
    predictions <- predict(
      model_final, 
      new_data = holdout[colnames(holdout) != response], 
      type = prediction_type
    )[[prediction_column]]
    
    if (prediction_type == "class") {  
      predictions <- as.numeric(as.character(predictions))
    }
    
  } else {
    
    # generate predictions on holdout - sculpture
    predictions <- predict(
      model_final, 
      newdata = holdout[colnames(holdout) != response], 
    )
    if (task == "classification") {
      # message("Converting sculpture predictions from logloss to probability.")
      predictions <- 1 / (1 + exp(-predictions))
    }
  }
  
  # holdout data into one tibble
  observations <- holdout[[response]]
  dat <- tibble(obs = observations, preds = predictions)
  
  # fit calibration model on holdout
  calib_model <- fit_calibration(obs = dat$obs, preds = dat$preds)
  
  dat <- mutate(dat, preds_calib = as.vector(predict(calib_model, type = "response"))) 
  # as.vector: 1d array -> numeric
  
  m_holdout <- calc_metrics(dat = dat, task = task, metrics = metrics)
  
  return(m_holdout)
}


#' Calculate metrics for classification or regression
#' 
#' @param dat Data frame with obs, preds, preds_calib
#' @param task "classification" or "regression"
#' @param metrics `metric_set` from `define_model`
#' 
calc_metrics <- function(dat, 
                         task = c("classification", "regression"),
                         metrics) {
  
  stopifnot(
    is.data.frame(dat),
    all(c("obs", "preds", "preds_calib") %in% colnames(dat))
  )
  
  task <- match.arg(task)
  
  m1 <- dat %>% 
    metrics(truth = obs, "preds") %>% 
    select(-.estimator) %>% 
    pivot_wider(names_from = .metric, values_from = .estimate)
  
  if (task == "classification") {
    
    # Need to convert factor to numeric for DI, MI, R2C1
    # The first level of factor is considered as 1
    if (is.factor(dat$obs)) {
      dat <- mutate(dat, obs = 2 - as.numeric(obs)) 
    }
    
    m2 <- summarise(
      dat, 
      R2 = metrics_R2(score_fun = "score_quadratic", y = obs, y_hat = preds),
      DI = metrics_DI(score_fun = "score_quadratic", y = obs, y_hat_calib = preds_calib),
      MI = metrics_MI(score_fun = "score_quadratic", y = obs, y_hat = preds, 
                      y_hat_calib = preds_calib),
      R2C1 = DI - MI,
      r2C1 = metrics_r2(y = obs, y_hat = preds, y_hat_calib = preds_calib),
      R2_ll = metrics_R2(score_fun = "score_log_loss", y = obs, y_hat = preds),
      DI_ll = metrics_DI(score_fun = "score_log_loss", y = obs, y_hat_calib = preds_calib),
      MI_ll = metrics_MI(score_fun = "score_log_loss", y = obs, y_hat = preds, 
                         y_hat_calib = preds_calib),
    )
  } else {
    m2 <- summarise(
      dat, 
      R2 = metrics_R2(score_fun = "score_quadratic", y = obs, y_hat = preds),
      DI = metrics_DI(score_fun = "score_quadratic", y = obs, y_hat_calib = preds_calib),
      MI = metrics_MI(score_fun = "score_quadratic", y = obs, y_hat = preds, 
                      y_hat_calib = preds_calib),
      R2C1 = DI - MI,
      r2C1 = metrics_r2(y = obs, y_hat = preds, y_hat_calib = preds_calib)
    )
  }
  
  if ("rsq_trad" %in% colnames(m1)) {
    m1 <- select(m1, -all_of("rsq_trad"))
  }
  
  return(
    bind_cols(m1, m2)
  )
  }


#' Fit calibration curve using gam.
#' 
#' Binomial link for classification, otherwise gaussian.
#'
#' @param obs Observations
#' @param preds Predictions
#'
#' @return GAM model
#'
fit_calibration <- function(obs, preds) {
  if (is.factor(obs)) {
    fam <- binomial()
    obs <- factor(obs, levels=rev(levels(obs)))
  } else {
    fam <- gaussian()
  }
  tryCatch(
    gam(obs ~ s(preds, k = -1), family = fam, na.action = "na.omit"),
    error = \(e) tryCatch(
      gam(obs ~ s(preds, k = 3), family = fam, na.action = "na.omit"),
      error = \(e) gam(obs ~ preds, family = fam, na.action = "na.omit")
    )
  )
}


#' Summarise resampled (based on nested cv) metrics
#'
#' @param x Result of `tune_nested`.
#'
#' @return Tibble with metric summaries.
#'
summarise_metrics <- function(x) {
  x %>% 
    group_by(id) %>% 
    # per repeat, across folds
    summarise(across(where(is.numeric), mean)) %>% 
    # across repeats
    summarise(across(where(is.numeric), mean))
}



#' Save RDS in project folder
#'
#' @param x Object
#' @param file_path Filepath.
#'
#' @return Nothing. Saves the file as RDS.
#'
store_results <- function(x, file_path) {
  fl <- file.path(storage_folder, file_path)
  saveRDS(x, file = fl)
  message(system(paste("chmod 664", fl), intern = T))
  invisible(NULL)
}

#' Read RDS from project folder
#'
#' @param file_path Filepath.
#'
#' @return Loaded object.
#'
load_results <- function(file_path) {
  readRDS(file = file.path(storage_folder, file_path))
}

#' Save metrics as CSV in project folder
#' 
#' @param metrics data frame with metrics
#' @param file_path Filepath.
#' 
#' @return Nothing. Saves the file as CSV.
#' 
store_metrics <- function(metrics, file_path) {
  readr::write_csv(metrics, file.path(storage_folder, file_path))
}

#' Read metrics from CSV in project folder
#' 
#' @param file_path Filepath.
#' 
#' @return Loaded metrics.
#' 
load_metrics <- function(file_path) {
  readr::read_csv(file.path(storage_folder, file_path), show_col_types = FALSE)
}


#' Save output as pdf or png
#'
#' @param output Output to save
#' @param name Name of the output.
#' @param height Passed to pdf and png
#' @param width Passed to pdf and png
#' @param units Passed to png
#' @param res Passed to png
#' @param type "pdf" or "png"
#'
#' @return Nothing. Side effect: writes into pdf.
#'
store_figure <- function(output, name, height = 8, width = 10,
                         units = "in", res = 300, type = c("png", "pdf")) {
  type <- match.arg(type)
  
  if (type == "pdf") {
    pdf(here::here("output", "figure", paste0(name, ".pdf")), height = height, width = width)
  } else {
    png(here::here("output", "figure", paste0(name, ".png")), height = height, width = width,
        units = units, res = res)
  }
  
  if (inherits(output, "ggplot")) {
    print(output)
  } else if (inherits(output, "grob")) {
    grid::grid.draw(output)
  } else {
    stop("Unknown output")
  }
  dev.off()  
}

# Save table output as csv
store_table <- function(output, name) {
  # Convert all numeric columns to 3 decimal places character
  output <- output %>% 
    mutate(across(where(is.numeric), ~format(round(., 3), nsmall = 3)))
  write_csv(output, here::here("output", "table", paste0(name, ".csv")))
}



#' Predict method for corel_publication class
#'
#' @param object Object of class corel_publication
#' @param new_data New data to make predictions on
#' @param type Type of prediction (not used)
#'
#' @return Predictions
#'
predict.corel_publication <- function(object, new_data, type) {
  
  # Check if object is of class corel_publication
  if (!inherits(object, "corel_publication")) {
    stop("Object is not of class corel_publication")
  }
  
  predictions <- 
    new_data %>% 
    mutate(pred = if_else((age >= 18 & age <= 20) & sex == "Male", 1, 0),
           pred = if_else((age >= 21 & age <= 23) & (priors >= 2 & priors <= 3), 1, pred),
           pred = if_else((priors > 3), 1, pred)) %>% 
    pull(pred)
  
  return(data.frame(.pred_class = predictions))
  
}

#' Get simplified rules from rpart model fit object
#' 
#' @param obj Object of class rpart.rules
#' @param keep_pred Whether to keep prediction columns
#' 
#' @return Data frame with rules
#' 
extract_rules_rpart <-
  function(obj,
           keep_var_name = FALSE,
           keep_pred = FALSE){
    
    df_decision_sets <-
      tibble::as_tibble(obj, .name_repair = "unique")
    
    # Create an index of columns that meet the condition
    index_when <- sapply(df_decision_sets, function(col) all(col %in% c("when")))
    index_and <- sapply(df_decision_sets, function(col) all(col %in% c("&", "")))
    
    # Create a vector of new names with col_and_xx
    colname_and_seq <- paste("col_and", seq_along(index_and)[index_and], sep = "_")
    
    # Replace the column names at the identified indices
    names(df_decision_sets)[index_when] <- "col_when"
    names(df_decision_sets)[index_and] <- colname_and_seq
    
    position_vars <- which(index_and | index_when) + 1
    names(position_vars) <- NULL
    
    # Get the strings from the columns at positions and use the first non-empty one as the column name
    variable_names <- sapply(df_decision_sets[, position_vars], function(col) {
      non_empty <- col[col != ""]
      if (length(non_empty) > 0) {
        return(non_empty[1])
      } else {
        return(NULL)
      }
    })
    
    # Create a new tibble with concatenated columns
    if (keep_var_name) {
      concat_columns <- 
        sapply(position_vars, function(start) {
          end <- ifelse(is.na(position_vars[which(position_vars == start) + 1]),
                        ncol(df_decision_sets), 
                        position_vars[which(position_vars == start) + 1] - 2)
          apply(df_decision_sets[, start:end], 1, paste, collapse = " ")
        }) %>% 
        as.data.frame()
    } else {
      concat_columns <- 
        sapply(position_vars, function(start) {
          end <- ifelse(is.na(position_vars[which(position_vars == start) + 1]),
                        ncol(df_decision_sets), 
                        position_vars[which(position_vars == start) + 1] - 2)
          apply(df_decision_sets[, (start+1):end], 1, paste, collapse = " ")
        }) %>% 
        as.data.frame()
      
    }
    
    names(concat_columns) <- variable_names
    
    if (keep_pred) {
      df_decision_sets_clean <- cbind(
        df_decision_sets[, 1:(position_vars[1] - 2)], concat_columns)
    } else {
      df_decision_sets_clean <- cbind(
        df_decision_sets[, 1], concat_columns)
    }
    
    return(df_decision_sets_clean)
  }

