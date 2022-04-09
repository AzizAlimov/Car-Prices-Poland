library(lightgbm)
library(dplyr)

load("data/one_hot_subset.RData")

run_lgb = function(lgb_params) {

  train = dat_one_hot[1:10000,]
  test = dat_one_hot[10001:12000,] 
  
  
  train_labels = train$price
  test_labels = test$price
  
  train = subset(train, select=-c(price))
  test = subset(test, select=-c(price))
  train_matrix = as.matrix(train)
  test_matrix = as.matrix(test)
  
  lgb_train = lgb.Dataset(data=as.matrix(train), label=train_labels)
  
  
  lgb_model = lgb.train(params=lgb_params, data=lgb_train, nrounds=100)
  
  rsquare = function(y_actual,y_predict){
    cor(y_actual,y_predict)^2
  }
  
  train_pred = predict(lgb_model, as.matrix(train))
  sqrt(mean((train_pred - train_labels)^2))
  a = rsquare(train_labels, train_pred)
  
  resids = (train_labels-train_pred)
  plot(train_pred, resids)
  
  test_pred = predict(lgb_model, as.matrix(test))
  resids_test = (test_labels-test_pred)
  plot(test_pred, resids_test)
  sqrt(mean((test_pred - test_labels)^2))
  rsquare(test_labels, test_pred)
  # lgb.importance(lgb_model)
  
  
  lgb_params$alpha= 0.1
  lgb_model_10 = lgb.train(params=lgb_params, data=lgb_train, nrounds=100)
  lgb_params$alpha= 0.25
  lgb_model_25 = lgb.train(params=lgb_params, data=lgb_train, nrounds=100)
  lgb_params$alpha= 0.75
  lgb_model_75 = lgb.train(params=lgb_params, data=lgb_train, nrounds=100)
  lgb_params$alpha= 0.90
  lgb_model_90 = lgb.train(params=lgb_params, data=lgb_train, nrounds=100)
  
  train_pred25 = predict(lgb_model_25, train_matrix)
  train_pred75 = predict(lgb_model_75, train_matrix)
  train_pred10 = predict(lgb_model_10, train_matrix)
  train_pred90 = predict(lgb_model_90, train_matrix)
  
  test_pred25 = predict(lgb_model_25, test_matrix)
  test_pred75 = predict(lgb_model_75, test_matrix)
  test_pred10 = predict(lgb_model_10, test_matrix)
  test_pred90 = predict(lgb_model_90, test_matrix)
  
  intervalScore = function(predObj,actual,level)
  { n = nrow(predObj)
  alpha = 1-level
  ilow = (actual<predObj[,2]) # overestimation
  ihigh = (actual>predObj[,3]) # underestimation
  sumlength = sum(predObj[,3]-predObj[,2]) # sum of lengths of prediction intervals
  sumlow = sum(predObj[ilow,2]-actual[ilow])*2/alpha
  sumhigh = sum(actual[ihigh]-predObj[ihigh,3])*2/alpha
  avglength = sumlength/n
  IS = (sumlength+sumlow+sumhigh)/n # average length + average under/over penalties
  cover = mean(actual>= predObj[,2] & actual<=predObj[,3])
  summ = c(level,avglength,IS,cover)
  # summary with level, average length, interval score, coverage rate
  imiss = which(ilow | ihigh)
  list(summary=summ, imiss=imiss)
  }
  
  pred50 = cbind(test_pred, test_pred25, test_pred75)
  is50 = intervalScore(pred50, test_labels, 0.5)
  print(is50$summary)
  
  pred80 = cbind(test_pred, test_pred10, test_pred90)
  is80 = intervalScore(pred80, test_labels, 0.80)
  print(is80$summary)
}


lgb_params = list(objective="quantile", 
                  alpha=0.5, 
                  reg_alpha=1,
                  reg_lambda=1,
                  num_leaves=20,
                  subsample=0.9,
                  min_child_samples=40,
                  learning_rate=0.1,
                  seed=123)

run_lgb(lgb_params)
 
lgb_params = list(objective="quantile", 
                  alpha=0.5, 
                  reg_alpha=1,
                  reg_lambda=1,
                  num_leaves=10,
                  subsample=0.9,
                  min_child_samples=40,
                  learning_rate=0.1,
                  seed=123)

run_lgb(lgb_params)

lgb_params = list(objective="quantile", 
                  alpha=0.5, 
                  reg_alpha=1,
                  reg_lambda=1,
                  num_leaves=3,
                  subsample=0.8,
                  min_child_samples=20,
                  learning_rate=0.1,
                  seed=123)

run_lgb(lgb_params)
