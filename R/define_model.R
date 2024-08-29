

#' Define model and preprocessing
#'
#' @param type Model type
#' @param data_info Output of `define_data`
#'
#' @return List with model and preprocessing definitions
#'
define_model <- function(type, data_info) {
  
  param_grid_seed <- 1234
  
  # define model, preprocessing, parameters, and workflow

  
  # rpart -----
  if (type %in% c("rpart", "rpart_regression")) {
    
    requireNamespace("rpart")
    
    model <- decision_tree(
      mode = data_info$task, engine = "rpart", 
      cost_complexity = tune(), tree_depth = tune(), min_n = tune()
    )
    
    preprocess <- recipe(
      as.formula(paste(data_info$response, "~ .")),
      data = head(data_info$data$train, 20),
    )
    
    param_info <- parameters(
      list(
        cost_complexity = cost_complexity(range = c(-6, -1)), # using log10 transf. by default
        tree_depth = tree_depth(range = c(2, 6)),
        min_n = min_n(range = c(2, 60))
      )
    )
    
    set.seed(param_grid_seed)
    param_grid <- grid_latin_hypercube(param_info, size = 20)
    
    workflow_fit <- workflow(preprocessor = preprocess, spec = model)
    
    # corel published model for COMPAS dataset -----
  } else if (type == "corel_publication") {
    
    model <- structure(list(name = "corel_publication"), 
                       class = c("corel_publication", "workflow"))
    
    preprocess <- NULL
    param_info <- NULL
    param_grid <- NULL
    workflow_fit <- model
    
    # logistic regression -----
  } else if (type == "logistic") {
    
    model <- logistic_reg(engine = "glm") # no mixture or penalty in glm
    
    preprocess <- recipe(
      as.formula(paste(data_info$response, "~ .")),
      data = head(data_info$data$train, 20),
    ) %>% 
      step_normalize(all_numeric_predictors())
    
    param_info <- param_grid <- NULL
    
    workflow_fit <- workflow(preprocessor = preprocess, spec = model)
    
    # logistic - regularized -----
  } else if (type %in% c("log_elastic", "log_lasso", "log_ridge")) {
    
    requireNamespace("glmnet")
    
    if (type == "log_elastic") {
      mixt <- 0.5
    } else if (type == "log_lasso") {
      mixt <- 1
    } else {
      mixt <- 0
    }
    
    model <- logistic_reg(engine = "glmnet", mixture = mixt, penalty = tune())
    
    preprocess <- recipe(
      as.formula(paste(data_info$response, "~ .")),
      data = head(data_info$data$train, 20),
    ) %>% 
      step_dummy(all_nominal_predictors()) %>% 
      step_normalize(all_numeric_predictors())
    
    param_info <- parameters(
      list(
        penalty = penalty(c(-5, 0))
      )
    )
    
    set.seed(param_grid_seed)
    param_grid <- grid_latin_hypercube(param_info, size = 40)
    
    workflow_fit <- workflow(preprocessor = preprocess, spec = model)
    
    # lm - regularized -----
  } else if (type %in% c("lm_elastic", "lm_lasso", "lm_ridge")) {
    
    requireNamespace("glmnet")
    
    if (type == "lm_elastic") {
      mixt <- 0.5
    } else if (type == "lm_lasso") {
      mixt <- 1
    } else {
      mixt <- 0
    }
    
    model <- linear_reg(engine = "glmnet", mixture = mixt, penalty = tune())
    
    preprocess <- recipe(
      as.formula(paste(data_info$response, "~ .")),
      data = head(data_info$data$train, 20),
    ) %>% 
      step_dummy(all_nominal_predictors()) %>% 
      step_normalize(all_numeric_predictors())
    
    param_info <- parameters(
      list(
        penalty = penalty(c(-5, 0))
      )
    )
    
    set.seed(param_grid_seed)
    param_grid <- grid_latin_hypercube(param_info, size = 40)
    
    workflow_fit <- workflow(preprocessor = preprocess, spec = model)
    
    
    # lm -----
  } else if (type == "lm") {
    
    requireNamespace("glmnet")
    
    model <- linear_reg(engine = "lm")
    
    preprocess <- recipe(
      as.formula(paste(data_info$response, "~ .")),
      data = head(data_info$data$train, 20),
    ) %>% 
      step_dummy(all_nominal_predictors()) %>% 
      step_normalize(all_numeric_predictors())
    
    param_info <- NULL
    param_grid <- NULL
    
    workflow_fit <- workflow(preprocessor = preprocess, spec = model)
    
    
    # xgb -----
  } else if (type %in% c("xgb", "xgb_monotone")) {
    
    requireNamespace("xgboost")
    
    if (type == "xgb_monotone") {
      monotone_constraints <- data_info$monotone_constraints
    } else {
      monotone_constraints <- NULL
    }
    
    model <- boost_tree(
      mode = data_info$task,
      mtry = tune(), trees = tune(), min_n = tune(),
      tree_depth = tune(), learn_rate = tune(), loss_reduction = tune(),
      sample_size = tune(), stop_iter = tune()
    ) %>%
      set_engine(
        engine = "xgboost",
        nthread = 5,
        counts = FALSE,
        alpha = tune(),
        lambda = tune(),
        monotone_constraints = monotone_constraints
      )
    
    preprocess <- recipe(
      as.formula(paste(data_info$response, "~ .")),
      data = head(data_info$data$train, 20),
    ) %>% 
      step_dummy(all_nominal_predictors(), one_hot = TRUE)
    
    param_info <- parameters(
      list(
        mtry = subsample(),
        trees = trees(c(10, 6000)),
        min_n = min_n(c(2, 60)),
        tree_depth = tree_depth(c(1, 10)),
        learn_rate = learn_rate(c(-6, -0.3)),
        loss_reduction = loss_reduction(c(-6, 1)),
        sample_size = subsample(),
        alpha = regularization(),
        lambda = regularization(),
        stop_iter = stop_iter(c(5, 20))
      )
    )
    
    set.seed(param_grid_seed)
    param_grid <- grid_latin_hypercube(param_info, size = 40)
    
    workflow_fit <- workflow(preprocessor = preprocess, spec = model)
    

    # xgb nth order -----
    # Detect if type is xgb_*_order
  } else if (grepl("xgb_[0-9]+_order", type)) {
    
    # Extract the order from the type
    # get the number between xgb_ and _order and ignore everything after _order
    nth_order <- as.numeric(sub("xgb_([0-9]+)_order.*", "\\1", type))
    
    requireNamespace("xgboost")
    
    if (grepl("monotone", type)) {
      monotone_constraints <- data_info$monotone_constraints
    } else {
      monotone_constraints <- NULL
    }
    
    model <- boost_tree(
      mode = data_info$task,
      trees = tune(), min_n = tune(),
      tree_depth = tune(), learn_rate = tune(), loss_reduction = tune(),
      sample_size = tune(), stop_iter = tune()
    ) %>%
      set_engine(
        engine = "xgboost",
        nthread = 5,
        counts = TRUE,
        alpha = tune(),
        lambda = tune(),
        monotone_constraints = monotone_constraints,
        colsample_bytree = nth_order
      )
    
    preprocess <- recipe(
      as.formula(paste(data_info$response, "~ .")),
      data = head(data_info$data$train, 20),
    ) %>% 
      step_dummy(all_nominal_predictors(), one_hot = TRUE)
    
    param_info <- parameters(
      list(
        trees = trees(c(10, 6000)),
        min_n = min_n(c(2, 60)),
        tree_depth = tree_depth(c(1, 10)),
        learn_rate = learn_rate(c(-6, -0.3)),
        loss_reduction = loss_reduction(c(-6, 1)),
        sample_size = subsample(),
        alpha = regularization(),
        lambda = regularization(),
        stop_iter = stop_iter(c(5, 20))
      )
    )
    
    set.seed(param_grid_seed)
    param_grid <- grid_latin_hypercube(param_info, size = 40)
    
    workflow_fit <- workflow(preprocessor = preprocess, spec = model)
    
    # random forest -----
  } else if (type == "rf") {
    
    requireNamespace("ranger")
    
    model <- rand_forest(
      mode = data_info$task,
      mtry = tune(),
      trees = tune(),
      min_n = tune()
    ) %>%
      set_engine(
        engine = "ranger",
        max.depth = tune(),
        sample.fraction = tune(),
        num.threads = 5
      )
    
    preprocess <- recipe(
      as.formula(paste(data_info$response, "~ .")),
      data = head(data_info$data$train, 20),
    ) %>% 
      step_dummy(all_nominal_predictors(), one_hot = TRUE)
    
    n_cols <- ncol(bake(prep(preprocess, data_info$data$train), new_data = NULL)) - 1
    
    param_info <- parameters(
      list(
        mtry = mtry(c(1, n_cols)),
        trees = trees(c(10, 6000)),
        min_n = min_n(c(2, 60)),
        max.depth = tree_depth(c(1, 10)),
        sample.fraction = subsample()
      )
    )
    
    set.seed(param_grid_seed)
    param_grid <- grid_latin_hypercube(param_info, size = 40)
    
    workflow_fit <- workflow(preprocessor = preprocess, spec = model)
    
    # lightgbm -----
  } else if (type == "lgb") {
    
    requireNamespace(c("bonsai", "lightgbm"))
    
    model <- boost_tree(
      mode = data_info$task,
      mtry = tune(),
      trees = tune(),
      min_n = tune(),
      tree_depth = tune(),
      learn_rate = tune(),
      loss_reduction = tune(),
      sample_size = tune(),
      stop_iter = tune()
    ) %>%
      set_engine(
        engine = "lightgbm",
        counts = FALSE,
        eval = ifelse(data_info$task == "regression", "rmse", "auc"),
        num_threads = 5,
        lambda_l1 = tune(),
        lambda_l2 = tune(),
        # tree_learner: https://lightgbm.readthedocs.io/en/v3.3.5/Parallel-Learning-Guide.html
        tree_learner = ifelse(nrow(data_info$data$train) > 5e3, "data", "feature"),
        # https://lightgbm.readthedocs.io/en/v3.3.5/Parameters-Tuning.html
        num_leaves = tune()
      )
    
    preprocess <- recipe(
      as.formula(paste(data_info$response, "~ .")),
      data = head(data_info$data$train, 20),
    ) 
    
    param_info <- parameters(
      list(
        mtry = subsample(),
        trees = trees(c(1, 6000)),
        min_n = min_n(c(2, 40)),
        tree_depth = tree_depth(c(1, 10)),
        learn_rate = learn_rate(c(-5, -0.3)),
        loss_reduction = loss_reduction(c(-5, 1)),
        sample_size = subsample(),
        lambda_l1 = regularization(),
        lambda_l2 = regularization(),
        num_leaves = num_leaves(c(2, 2^10)),
        stop_iter = stop_iter(c(5, 20))
      )
    )
    
    set.seed(param_grid_seed)
    param_grid <- grid_latin_hypercube(param_info, size = 40)
    
    workflow_fit <- workflow(preprocessor = preprocess, spec = model)
    
  } else {
    stop(paste("Undefined model:", type))
  }
  
  # define metrics
  if (data_info$task == "regression") {
    metrics <- metric_set(rsq_trad, rmse, rsq)  
  } else if (data_info$task == "classification") {
    metrics <- metric_set(roc_auc, mn_log_loss)
  } else {
    stop(paste("Undefined metrics for task:", data_info$task))
  }
  
  return(
    list(
      workflow = workflow_fit,
      param_grid = param_grid,
      param_info = param_info, 
      metrics = metrics
    )
  )
}

# subsample parameter for xgb
subsample <- function(range = c(0.5, 1), trans = NULL) {
  new_quant_param(
    type = "double",
    range = range,
    inclusive = c(FALSE, TRUE),
    trans = trans,
    label = c(subsample = "subsample"),
    finalize = NULL
  )
}

# regularization parameter for xgb
regularization <- function(range = c(0, 10), trans = NULL) {
  new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(regularization = "regularization"),
    finalize = NULL
  )
}

