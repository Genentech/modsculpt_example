
## Important links

- modsculpt package:
  <https://github.com/Genentech/modsculpt>
- Output examples (GitHub Pages):
  <https://pages.github.roche.com/yoshidk6/modsculpt_example/>

## Table Of Contents
- [Introduction](#introduction)
- [Overview](#overview)
    - [1. Set-up](#1-set-up)
    - [2. Tuning and training models](#2-tuning-and-training-models)
    - [3. Model sculpting](#3-model-sculpting)
    - [4. Model evaluation](#4-model-evaluation)
- [Details](#details)
    - [Install the packages](#install-the-packages)
    - [Data examples](#data-examples)
    - [Model options](#model-options)
    - [Job submission](#job-submission)
- [Appendix](#appendix)
    - [Continuous endpoint example](#continuous-endpoint-example)


## Introduction

This repository provides a fully worked-out example of model sculpting,
a method to develop interpretable, trustworthy, and high-performing additive
models. The example presented here accompanies the [`modsculpt`](https://github.com/Genentech/modsculpt)
package, which provides a set of tools to perform model sculpting, and 
[the slide deck (still in development)](#placeholder) that explains the concept of model sculpting.

Here we provide an example workflow of how to develop strong learner with 
hyperparameter tuning, to perform proper performance evaluation using nested 
cross-validation, to sculpt the strong learner into an interpretable model, 
and to evaluate and interpret the sculpted model.

Simpler examples are provided in the original 
[`modsculpt` repository](https://genentech.github.io/modsculpt/main/articles/modsculpt.html).


## Overview

### 1. Set-up

1. Clone the repository
1. Open the R project file `modsculpt_example.Rproj` in RStudio
1. [Install the necessary packages](#install-the-packages)
1. Open the R script `R/0_setup.R` and modify the storage folder as necessary
    - By default it is set at `output/sculpt_results` folder, but you can specify
      any folder, including one outside of this repository
      (e.g. when you run this code in a cluster).

### 2. Tuning and training models

1. Run the R script `R/1_prepare_data.R` to [prepare the example data](#data-examples)
    - [You can define your own data](#data-examples) in `R/define_data.R`
1. Run the R script `R/2_train_models.R` to train the models
    - See below for [data examples](#data-examples) and [model options](#model-options) 
    - You can run `R/2_train_models.R` interactively, or execute it in a batch
      mode with the command line.  
      See [job submission](#job-submission) for details
    - The following models need to be trained on the `compas` dataset if you want
      to follow the steps below
    
      ```bash
      # Note that xgb training can take some time (~1 hr on M2 MacBook Air)
      Rscript R/2_train_models.R xgb compas FALSE 
      # xgb_1_order is a bit faster (~30 min)
      Rscript R/2_train_models.R xgb_1_order compas FALSE 
      # Following models are faster (<1 min)
      Rscript R/2_train_models.R log_elastic compas FALSE
      Rscript R/2_train_models.R log_lasso compas FALSE
      Rscript R/2_train_models.R log_ridge compas FALSE
      ```

1. *[Optional]* Run the R script `R/2_train_models_ncv.R` to perform
   nested cross-validation on the models
    - This script is necessary only if you want to perform nested cross-validation
      for model evaluation
    - You can run this script interactively, or execute it in a batch mode with
      the command line.  
      See [job submission](#job-submission) for details

### 3. Model sculpting

1. Run the R script `R/3_model_sculpt_compas.R` to perform model sculpting
  on the `compas` dataset
    - You can modify the script to evaluate the models on the `bike` dataset
1. *[Optional]* Run the R script `R/3_model_sculpt_ncv_compas.R` to perform
   model sculpting on nested cross-validation outputs for sculpted model 
   performance evaluation

### 4. Model evaluation

1. Evaluation/interpretation of sculpted models: `notebooks/2_1_modsculpt_compas.qmd`
1. Performance summary: `notebooks/3_performance_summary.qmd`
1. [Sample outputs](https://pages.github.roche.com/yoshidk6/modsculpt_example/)


## Details

### Install the packages

The following packages are required to run the example in this repository.

```r
# Install necessary packages
install_if_not_available <-
  function(pkg, from_github = FALSE, ref = NULL, min_version = NULL) {
    
    # Check whether it's installed and the version is ok
    is_installed <- suppressWarnings(suppressPackageStartupMessages(require(pkg, character.only = TRUE)))
    version_ok <- is.null(min_version)
    if (is_installed & !version_ok) version_ok <- packageVersion(pkg) >= min_version
    
    # Install if necessary
    if (!is_installed | !version_ok) {
      if (from_github) {
        remotes::install_github(pkg, ref = ref)
      } else {
        install.packages(pkg)
      }
    }
  }

install_if_not_available("dplyr")
install_if_not_available("tidyr")
install_if_not_available("readr")
install_if_not_available("purrr")
install_if_not_available("lubridate")
install_if_not_available("here")
install_if_not_available("DT")
install_if_not_available("data.table")
install_if_not_available("mgcv")
install_if_not_available("tidymodels")
install_if_not_available("glmnet")
install_if_not_available("xgboost")
# devtools::install_version('xgboost', '1.7.7.1') # Version used for generating the outputs
install_if_not_available("tune")
install_if_not_available("parallel") 
install_if_not_available("doParallel") 

# Install the following packages from GitHub
install_if_not_available("remotes")
install_if_not_available("modsculpt", from_github = TRUE, ref = "v0.1")
install_if_not_available("stats4phc", from_github = TRUE, ref = "v0.1.1")
```

### Data examples

Two example datasets were provided, `compas` and `bike`.

- `compas`: A dataset from the COMPAS recidivism risk assessment tool, used in 
  the publication ["Stop Explaining Black Box Machine Learning Models for High
  Stakes Decisions and Use Interpretable Models Instead" by Rudin et al.
  ](https://www.nature.com/articles/s42256-019-0048-x), available 
  [here](https://github.com/propublica/compas-analysis). This is an example for
  binary classification.
- `bike`: A dataset from the Capital Bikeshare program in Washington, D.C., 
  available [here](https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset).
  This is an example for regression.

If you are interested in using your own data, you can define it in
`R/define_data.R`. For binary classification task, the response variable is to 
be a factor with the the first level being "positive"; otherwise some of the codes
make a wrong conversion. 

### Model options

The following `model_types` are currently supported, made available with
`parsnip` package.
See `R/define_model.R` for the implementation details.

| model_type | task | description | packages |
|------------|------|-------------|----------|
| `xgb` | reg. or class. | XGBoost | `xgboost` |
| `xgb_monotone` | reg. or class. | XGBoost with monotone constraints as defined in `R/define_data.R` | `xgboost` |
| `xgb_*_order` | reg. or class. | XGBoost with `*` order constraints; replace `*` with the number to indicate the number of your choice, e.g. `xgb_1_order`| `xgboost` |
| `xgb_*_order_monotone` | reg. or class. | XGBoost with *_order and the monotone constraints | `xgboost` |
| `lm` / `logistic` | reg. / class. | Linear model / Logistic regression | `stats` / `glm` |
| `lm_elastic` / `log_elastic` | reg. / class. | Elastic net | `glmnet` |
| `lm_lasso` / `log_lasso` | reg. / class. | Lasso | `glmnet` |
| `lm_ridge` / `log_ridge` | reg. / class. | Ridge | `glmnet` |
| `rf` | reg. or class. | Random forest | `ranger` |
| `lgb` | reg. or class. | LightGBM | `bonsai`, `lightgbm` |
| `rpart` | class. | Recursive partitioning | `rpart` |
| `rpart_regression` | reg. | Recursive partitioning | `rpart` |


### Job submission

You can run the training script `R/2_train_models.R` and `R/2_train_models_ncv.R`
in a batch mode with the command line. 
The scripts takes three arguments: model type, dataset name, and
whether to use Bayesian optimization in addition to the grid search.

This is particularly useful when you run the code on a cluster.

On you local computer, you should be able to use any terminal; 
on Windows PC, the terminal tab in RStudio might be the easiest.

```bash
Rscript R/2_train_models.R $1 $2 $3
Rscript R/2_train_models_ncv.R $1 $2 $3
```

where

- `$1` is the model type (e.g. `xgb`, see [model options](#model-options) for
  the full list of model types)
- `$2` is the dataset name (e.g. `compas` or `bike`)
- `$3` is whether to use Bayesian optimization (e.g. `TRUE` or `FALSE`)
  in addition to the grid search
    - Bayesian optimization takes a long time to run, so it is recommended to
      set this to "FALSE" for an interactive run

For example,

```bash
Rscript R/2_train_models.R xgb compas FALSE
Rscript R/2_train_models.R lm_elastic bike FALSE
```

## Appendix

### Continuous endpoint example

Example with bike dataset

1. Run the R scripts `R/2_train_models.R` (& `R/2_train_models_ncv.R`) to train the models
    
    ```bash
    Rscript R/2_train_models.R xgb bike FALSE 
    Rscript R/2_train_models.R xgb_1_order bike FALSE 
    Rscript R/2_train_models.R log_elastic bike FALSE
    Rscript R/2_train_models.R log_lasso bike FALSE
    Rscript R/2_train_models.R log_ridge bike FALSE
    ```

1. Run the R script `R/3_model_sculpt_bike.R` (& `R/3_model_sculpt_ncv_bike.R`) 
   to perform model sculpting (and nested cross-validation performance evaluation)
   on the `bike` dataset
1. Evaluation/interpretation of sculpted models: `notebooks/2_1_modsculpt_bike.qmd`
1. Performance summary: `notebooks/3_performance_summary.qmd`
1. [Sample outputs](https://pages.github.roche.com/yoshidk6/modsculpt_example/)
