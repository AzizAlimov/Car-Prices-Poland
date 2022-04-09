
# Function taken from course notes, calculates the interval score for a given
# level from predictions and actual values
intervalScore = function(predObj, actual, level) {
  n <- nrow(predObj)
  alpha <- 1 - level
  ilow <- (actual < predObj[, 2]) # overestimation
  ihigh <- (actual > predObj[, 3]) # underestimation
  sumlength <- sum(predObj[, 3] - predObj[, 2]) # sum of lengths of prediction intervals
  sumlow <- sum(predObj[ilow, 2] - actual[ilow]) * 2 / alpha
  sumhigh <- sum(actual[ihigh] - predObj[ihigh, 3]) * 2 / alpha
  avglength <- sumlength / n
  IS <- (sumlength + sumlow + sumhigh) / n # average length + average under/over penalties
  cover <- mean(actual >= predObj[, 2] & actual <= predObj[, 3])
  summ <- c(level, avglength, IS, cover)
  # summary with level, average length, interval score, coverage rate, r^2, rmse
  imiss <- which(ilow | ihigh)
  list(summary = summ, imiss = imiss)
}


#Function to 
FindUniquePos=function(values,groupValues,tolerance=1.e-5) { 
  ngroup = length(groupValues) # number of groups (nodes)
  temp = unique(groupValues)
  if(length(temp)<ngroup)
  { cat("Won't work: non-unique group values\n"); return(0); }
  npred = length(values) # number of cases to bin into a group label
  group = rep(0,npred) # initialize as group 0
  for(i in 1:ngroup)
  { # group[values==groupValues[i]]=i # better to use tolerance
    igroup = (abs(values-groupValues[i])<tolerance)
    group[igroup] = i # group label according to position in groupValues
  }
  if( any(group==0) ) cat("Warning: some values not matched to groupValues\n")
  return(group)
}


#Generic function to 
crossValidationCont = function(df,K,training_func,predict_func,nperfmeas=4) {
  set.seed(123)
  n = nrow(df)
  nhold = round(n/K)
  iperm = sample(n)
  perfmeas50 = matrix(0, K, nperfmeas)  
  perfmeas80 = matrix(0, K, nperfmeas)
  models = vector(mode="list", length=3)
  for (i in 1:K) { 
    indices = (((i-1)*nhold+1):(i*nhold))
    if( i==K ) indices = (((i-1)*nhold+1):n)
    indices = iperm[indices]
    train = df[-indices,]
    holdout = df[indices,]
    models[[i]] = training_func(train, holdout)
    print(sprintf("Fold %d", i))
    results = predict_func(models[[i]], train, holdout, i)
    
    IS50 = results[1,]
    IS80 = results[2,]
    
    colnames(results)=c("level","avgleng","IS","cover")
    print(results)
    
    perfmeas50[i,1:4] = IS50
    perfmeas80[i,1:4] = IS80
  }
  avgperfmeas50 = apply(perfmeas50,2,mean)
  avgperfmeas80 = apply(perfmeas80,2,mean)
  list(perfmeas50byfold=perfmeas50, avgperfmeas50=avgperfmeas50,
       perfmeas80byfold=perfmeas80, avgperfmeas80=avgperfmeas80)
}

create_lgb_params <- function() {
  list(objective="quantile", 
       alpha=0.5, 
       reg_alpha=1,
       reg_lambda=1,
       num_leaves=20,
       subsample=0.9,
       min_child_samples=40,
       learning_rate=0.1,
       verbosity=-1,
       seed=123)
}

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
  print(lgb_params)
  for (i in 1:5) {
    lgb_params$alpha= alphas[i]
    models[[i]] = lgb.train(params=lgb_params, data=lgb_train, nrounds=100, verbose_eval= F)
  }
  models
}

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
