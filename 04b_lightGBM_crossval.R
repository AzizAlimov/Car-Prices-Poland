# Perform cross-validation on LGBM using the hyperparameters picked from
# Phase 2

# Load the one hot dataset
library(lightgbm)
load("data/one_hot_subset.RData")
source("functions.R")
seed=123

lgb_params = create_lgb_params()
crossValidationCont(dat_one_hot, 3, lightgbm_training, lightgbm_testing)
