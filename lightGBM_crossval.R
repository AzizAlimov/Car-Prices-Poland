# Perform cross-validation on LGBM using the hyperparameters picked from
# Phase 2


library(lightgbm)

# Load the one hot dataset
load("data/one_hot_subset.RData")
source("functions.R")
seed=123

lgb_params = list(objective="quantile", 
                  alpha=0.5, 
                  reg_alpha=1,
                  reg_lambda=1,
                  num_leaves=20,
                  subsample=0.9,
                  min_child_samples=40,
                  learning_rate=0.1,
                  seed=seed)

# Given training set and holdout, run LGBM training
lightgbm_training <- function(train, holdout) {
  train_labels = train$price
  holdout_labels = holdout$price
  
  train = subset(train, select=-c(price))
  holdout = subset(holdout, select=-c(price))
  train_matrix = as.matrix(train)
  holdout_matrix = as.matrix(holdout)
  
  lgb_train = lgb.Dataset(data=train_matrix, label=train_labels)
  models = vector(length=5, mode='list')
  alphas = c(0.5, 0.1, 0.9, 0.25, 0.75)
  for (i in 1:5) {
    lgb_params$alpha= alphas[i]
    models[[i]] = lgb.train(params=lgb_params, data=lgb_train, nrounds=100, verbose_eval= F)
  }
  models
}

# Run holdout set prediction on trained regression tree model and return
# summaries from 50% and 80% interval score evaluation.
# Plot the importance plot and residuals for that fold
lightgbm_testing <- function(models, train, holdout, fold) {
  holdout_labels = holdout$price
  holdout = subset(holdout, select=-c(price))
  holdout_matrix = as.matrix(holdout)
  holdout_preds = vector(length=5, mode='list')
  for (i in 1:5) {
    holdout_preds[i] = predict(models[i], holdout_matrix)
  }
  
  pred50 = cbind(holdout_preds[[1]], holdout_preds[[2]], holdout_preds[[3]])
  is50_results = intervalScore(pred50, holdout_labels, 0.5)
  
  pred80 = cbind(holdout_preds[[1]], holdout_preds[[4]], holdout_preds[[5]])
  is80_results = intervalScore(pred80, holdout_labels, 0.8)
  
  results = rbind(is50_results$summary, is80_results$summary)
  
  model_importance = lgb.importance(models[[1]], percentage = TRUE)
  lgb.plot.importance(model_importance, measure="Gain")
  
  resids_holdout = (holdout_labels-holdout_preds[[1]])
  plot(holdout_preds[[1]], resids_holdout, ylab = "Residuals", xlab = "Fitted Values", main = sprintf("Fold %d", fold))
  results
}

# Run cross validation
crossValidationCont(dat_one_hot, 3, lightgbm_training, lightgbm_testing, model_name="LGBM")

