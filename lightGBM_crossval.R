library(lightgbm)

load("data/one_hot_subset.RData")
seed=123

lgb_params_large = list(objective="quantile", 
                  alpha=0.5, 
                  reg_alpha=1,
                  reg_lambda=1,
                  num_leaves=40,
                  subsample=0.9,
                  min_child_samples=40,
                  learning_rate=0.1,
                  seed=seed)

lgb_params_small = list(objective="quantile", 
                  alpha=0.5, 
                  reg_alpha=1,
                  reg_lambda=1,
                  num_leaves=3,
                  subsample=0.8,
                  min_child_samples=20,
                  learning_rate=0.1,
                  seed=seed)

rsquare = function(y_actual,y_predict){
  cor(y_actual,y_predict)^2
}

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


crossValidationCont = function(df,K,nperfmeas=10,seed,lgb_params)
{ set.seed(seed)
  n = nrow(df)
  nhold = round(n/K) # size of holdout set 
  iperm = sample(n)
  perfmeas = matrix(0,K,nperfmeas)  
  for(k in 1:K)
  { indices = (((k-1)*nhold+1):(k*nhold))
  if( k==K ) indices = (((k-1)*nhold+1):n)
  indices = iperm[indices]
  train = df[-indices,]
  holdout = df[indices,]
  
  train_labels = train$price
  holdout_labels = holdout$price
  
  train = subset(train, select=-c(price))
  holdout = subset(holdout, select=-c(price))
  train_matrix = as.matrix(train)
  holdout_matrix = as.matrix(holdout)
  
  lgb_train = lgb.Dataset(data=train_matrix, label=train_labels)
  lgb_model = lgb.train(params=lgb_params, data=lgb_train, nrounds=100)
  
  lgb_params$alpha= 0.1
  lgb_model_10 = lgb.train(params=lgb_params, data=lgb_train, nrounds=100)
  lgb_params$alpha= 0.25
  lgb_model_25 = lgb.train(params=lgb_params, data=lgb_train, nrounds=100)
  lgb_params$alpha= 0.75
  lgb_model_75 = lgb.train(params=lgb_params, data=lgb_train, nrounds=100)
  lgb_params$alpha= 0.90
  lgb_model_90 = lgb.train(params=lgb_params, data=lgb_train, nrounds=100)
  
  holdout_pred = predict(lgb_model, holdout_matrix)
  holdout_pred25 = predict(lgb_model_25, holdout_matrix)
  holdout_pred75 = predict(lgb_model_75, holdout_matrix)
  holdout_pred10 = predict(lgb_model_10, holdout_matrix)
  holdout_pred90 = predict(lgb_model_90, holdout_matrix)
  
  rmse = sqrt(mean((holdout_pred - holdout_labels)^2))
  r2 = rsquare(holdout_labels, holdout_pred)
  
  pred50 = cbind(holdout_pred, holdout_pred25, holdout_pred75)
  is50_results = intervalScore(pred50, holdout_labels, 0.5)
  
  pred80 = cbind(holdout_pred, holdout_pred10, holdout_pred90)
  is80_results = intervalScore(pred80, holdout_labels, 0.8)
  
  results = c(is50_results$summ, is80_results$summ, r2, rmse)
  
  model_importance = lgb.importance(lgb_model, percentage = TRUE)
  lgb.plot.importance(model_importance, measure="Gain")
  
  resids_holdout = (holdout_labels-holdout_pred)
  plot(holdout_pred, resids_holdout)
  
  print(results)
  
  perfmeas[k,] = results
  }
  avgperfmeas = apply(perfmeas,2,mean)
  list(perfmeasbyfold=perfmeas, avgperfmeas=avgperfmeas)
}


crossValidationCont(dat_one_hot, 3, nperfmeas = 10, 123, lgb_params_small)
# crossValidationCont(dat_one_hot, 3, nperfmeas = 10, 123, lgb_params_large)
