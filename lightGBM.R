library(lightgbm)
library(dplyr)

load("data/one_hot_subset.RData")
source("functions.R")

run_lgb = function(Data) {

  train = Data[1:10000,]
  test = Data[10001:12000,] 
  train_labels = train$price
  test_labels = test$price
  
  lgb_models = lightgbm_training(train, test)
  results = lightgbm_testing(lgb_models, train, test)
  colnames(results)=c("level","avgleng","IS","cover")
  print(results)
}

lgb_params_list = rep(list(create_lgb_params()), 3)

lgb_params_list[[2]]$num_leaves=10
lgb_params_list[[3]]$num_leaves=3
lgb_params_list[[3]]$subsample=0.8
lgb_params_list[[3]]$min_child_samples=20
lgb_params_list[[3]]$num_leaves=3

for (i in 1:3) {
  lgb_params = lgb_params_list[[i]]
  print(run_lgb(dat_one_hot))
}
