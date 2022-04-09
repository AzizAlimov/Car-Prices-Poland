# Shared functions used across different models]

# Returns interval score summary of predictions
# Inputs:
# predObj: contains predictions, and lower and upper bounds of the 
# prediction intervals
# actual: the actual values for each prediction
# level: the prediction interval level
# Returns:
# summary: summary with the level, average length, interval score, and coverage rate
# imiss: predictions that were missed in the intervals
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
  imiss <- which(ilow | ihigh)
  list(summary = summ, imiss = imiss)
}

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
