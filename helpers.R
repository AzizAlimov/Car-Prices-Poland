intervalScore = function(predObj,actual,level) { 
  n = nrow(predObj)
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
  # summary with level, average length, interval score, coverage rate, r^2, rmse
  imiss = which(ilow | ihigh)
  list(summary=summ, imiss=imiss)
}

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
    results = predict_func(models[[i]], train, holdout)
    
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

