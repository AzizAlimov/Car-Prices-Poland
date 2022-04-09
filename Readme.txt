01_presentation_analysis.R: file with functions used to generate plots used in the first presentation concerning each of the variables in the dataset

02_data_prepping.R: file for data filtering and wrangling

03_lightGBM.R: Initial phase 2 LGBM training to pick hyperparameters for cross validation

04a_regression_prediction.R: Perform cross-validation for OLS, WLS and transformed OLS

04b_lightGBM_crossval.R: Perform cross-validation on LGBM using the hyperparameters picked from phase 2

04c_forest_predictions.R: Perform cross-validation for quantile random forest

04d_tree_predictions.R: Perform cross-validation for decision tree


Order
Run in the order indicated by the numbers; 01_presentation_analysis.R -> 02_data_prepping.R -> 03_lightGBM.R -> 04a_regression_prediction.R, 04b_lightGBM_crossval.R, 04c_forest_predictions.R, or 04d_tree_predictions.R in any order


Required R libraries
library(ggplot2): ggplot, aes, geom_boxplot, labs, geom_histogram 
library(forcats): fct_reorder
library(dplyr): group_by, filter, ungroup, filter, mutate, select
library(corrplot): corrplot
library(magrittr): %>% operator
library(tibble): as_tibble
library(lightgbm): lgb.importance, lgb.plot.importance, lgb.Dataset, lgb.train
library(quantregForest): quantregForest
library(rpart): rpart
library(rpart.plot): rpart.plot
