# Initial phase 2 LGBM training to pick hyperparameters for cross validation
set.seed(123)
load("data/one_hot_subset.RData")
source("functions.R")
library(lightgbm)

# Train and test LGBM on dataframe, splitting it into 10000 training samples and 2000 test
run_lgb = function(Data) {
  train = Data[1:10000,]
  test = Data[10001:12000,] 
  train_labels = train$price
  test_labels = test$price
  
  lgb_models = lightgbm_training(train, test)
  results = lightgbm_testing(lgb_models, train, test)
  colnames(results)=c("level","avgleng","IS","cover")
  print(results)
  lgb_models
}

lgb_params_list = rep(list(create_lgb_params()), 3)

# Try different params
lgb_params_list[[2]]$num_leaves=10
lgb_params_list[[3]]$num_leaves=3
lgb_params_list[[3]]$subsample=0.8
lgb_params_list[[3]]$min_child_samples=20
lgb_params_list[[3]]$num_leaves=3

for (i in 1:3) {
  lgb_params = lgb_params_list[[i]]
  results = run_lgb(dat_one_hot)
  
  save_name = paste("out/LGBM_init_model_", i, ".RData", sep="")
  save(file=save_name, results, lgb_params)
}
