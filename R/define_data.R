
#' Load data and set variables
#'
#' @param type Dataset type
#'
#' @return List with data, nested and standard cross-validation based on "train" set, 
#' and data variables.
#' 
define_data <- function(type) {
  
  seed_ncv <- 91346
  
  # compas data ----
  if (type %in% c("compas", "compas_regression")) {
    
    # check that data exist
    if (!file.exists(here::here("data", "processed", "compas_holdout.csv"))) {
      stop("Please run R/1_prepare_data.R first.")
    }
    
    # load preprocessed data
    train <- read.csv(here::here("data", "processed", "compas_train.csv")) %>% 
      as_tibble() %>% 
      mutate(
        sex = factor(sex, levels = c("Female", "Male")),
        current_charge_degree = factor(current_charge_degree, levels = c("Felony", "Misdemeanor")),
        recidivate_within_two_years = factor(recidivate_within_two_years, levels = c("1", "0"))
      )
    
    holdout <- read.csv(here::here("data", "processed", "compas_holdout.csv")) %>% 
      as_tibble() %>% 
      mutate(
        sex = factor(sex, levels = c("Female", "Male")),
        current_charge_degree = factor(current_charge_degree, levels = c("Felony", "Misdemeanor")),
        recidivate_within_two_years = factor(recidivate_within_two_years, levels = c("1", "0"))
      )
    
    # define variables
    response <- "recidivate_within_two_years"
    continuous_vars <- names(which(vapply(train, is.numeric, logical(1))))
    discrete_vars <- setdiff(colnames(train), c(continuous_vars, response))
    labels <- NULL
    
    if (type == "compas"){
      task <- "classification"
    } else {
      task <- "regression"
      train <- 
        train %>% 
        mutate(
          recidivate_within_two_years = as.numeric(as.character(recidivate_within_two_years))
        )
      holdout <- 
        holdout %>% 
        mutate(
          recidivate_within_two_years = as.numeric(as.character(recidivate_within_two_years))
        )
      
    }
    
    monotone_constraints <- list(
      "age" = -1,
      "priors" = 1,
      "juvenile_felonies" = 1,
      "juvenile_misdemeanors" = 1,
      "juvenile_crimes" = 1,
      # "sex" = 1, # levels: Female, Male; want higher prob. for Males
      # "current_charge_degree" = -1 # levels: Felony Misdemeanor; want higher prob for Felony
      "sex_Female" = -1,
      "sex_Male" = 1,
      "current_charge_degree_Felony" = 1,
      "current_charge_degree_Misdemeanor" = -1
    )
    
  # bike data ----
  } else if (type %in% c("bike")) {
    
    # check that data exist
    if (!file.exists(here::here("data", "processed", "bike_holdout.csv"))) {
      stop("Please run R/1_prepare_data.R first.")
    }
    
    # load preprocessed data
    train <- read.csv(here::here("data", "processed", "bike_train.csv")) %>% 
      as_tibble() %>%
      select(-dteday, -casual, -registered, -instant) %>%
      mutate(across(c(season, yr, mnth, holiday, weekday, workingday, weathersit), as.factor)) %>%
      mutate(cnt = log(cnt))
    
    holdout <- read.csv(here::here("data", "processed", "bike_holdout.csv")) %>% 
      as_tibble()  %>%
      select(-dteday, -casual, -registered, -instant) %>%
      mutate(across(c(season, yr, mnth, holiday, weekday, workingday, weathersit), as.factor)) %>%
      mutate(cnt = log(cnt))
    
    # define variables
    response <- "cnt"
    
    continuous_vars <- names(which(vapply(train, is.numeric, logical(1))))
    continuous_vars <- setdiff(continuous_vars, response)
    discrete_vars <- setdiff(colnames(train), c(continuous_vars, response))
    labels <- NULL
    
    task <- "regression"
    
    monotone_constraints <- list(
      "temp" = -1,
      "atemp" = 1,
      "hum" = 1,
      "windspeed" = 1
    )
    
  } else {
    stop(paste("Undefined dataset:", type))
  }
  
  
  # define nested cross validation - for resampled performance
  set.seed(1234)
  set.seed(seed_ncv)
  ncv <- nested_cv(
    data = train,
    outside = vfold_cv(v = 5, repeats = 3),
    inside = vfold_cv(v = 5, repeats = 1)
    # # quick cv for testing purposes only ...
    # outside = vfold_cv(v = 2, repeats = 2),
    # inside = vfold_cv(v = 2, repeats = 1)
  )
  
  # define cross validation - for final fit
  set.seed(1234)
  cv <- vfold_cv(v = 5, repeats = 1, data = train)
  
  # return object with all data, cv, and variables
  return(
    list(
      data = list(
        train = train,
        holdout = holdout
      ), 
      cv = cv,
      ncv = ncv,
      covariates = list(
        all = setdiff(colnames(train), response),
        continuous = continuous_vars,
        discrete = discrete_vars,
        labels = labels
      ),
      response = response,
      task = task, 
      monotone_constraints = monotone_constraints
    )
  )
}

