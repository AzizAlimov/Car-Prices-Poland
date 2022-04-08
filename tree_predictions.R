library(mlbench)
library(rpart)
library(rpart.plot)
library(VGAM)
library(pROC)
library(caret)
library(ggplot2)
library(quantregForest)
library(randomForest)


options(warn=-1)

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

get_quantiles <- function(training, holdout, model, bottom, upper, predictions) {
  meanpredRegTree=predictions
  meanByTNode=tapply(training$price, model$where, mean)
  QLowerByTNode=tapply(training$price, model$where,quantile,prob=bottom)
  Q50ByTNode=tapply(training$price, model$where,median)
  QUpperByTNode=tapply(training$price, model$where,quantile,prob=upper)
  TNodeGroup=FindUniquePos(meanpredRegTree,meanByTNode)
  ByTNode = cbind(meanByTNode,QLowerByTNode,Q50ByTNode,QUpperByTNode)
  QLowerpredRegTree=QLowerByTNode[TNodeGroup]
  QUpperpredRegTree=QUpperByTNode[TNodeGroup]
  predIntRegTree=cbind(meanpredRegTree,QLowerpredRegTree,QUpperpredRegTree)
  predIntRegTree
}


predict_tree <- function(model, training, holdout, prune = FALSE) {
  rpart.plot(model)
  predictions = predict(model,newdata=holdout,type="vector")
  pred50Int=get_quantiles(training, holdout, model, 0.25, 0.75, predictions)
  pred80Int=get_quantiles(training, holdout, model, 0.1, 0.9, predictions)
  
  IS50=intervalScore(pred50Int,holdout$price,0.5)
  IS80=intervalScore(pred80Int,holdout$price,0.8)
  out=rbind(IS50$summary,IS80$summary)
  out
}

train_tree <- function(training, holdout) {
  rpart(price~.,data=training)
}

predict_forest <- function(forest, training, holdout) {
  predRF = predict(forest, what=c(.1,.25,.5,.75,.9), newdata=holdout[,-1])
  IS50qRF=intervalScore(predRF[,c(3,2,4)],holdout$price, 0.5)
  IS80qRF=intervalScore(predRF[,c(3,1,5)],holdout$price, 0.8)
  outqRF=rbind(IS50qRF$summary,IS80qRF$summary)
  varImpPlot(forest)
  outqRF
}

train_forest <- function(training, holdout) {
  x = data.frame(training[,-1])
  y = unlist(training[,1], use.names=FALSE)
  quantregForest(x,y,importance=TRUE)
}

crossValidationCont = function(df,K,training_func,predict_func,nperfmeas=4) {
  set.seed(123)
  n = nrow(df)
  nhold = round(n/K)
  iperm = sample(n)
  perfmeas50 = matrix(0, K, nperfmeas)  
  perfmeas80 = matrix(0, K, nperfmeas)
  models = vector(mode="list", length=3)
  i = 1
  while (i <= K) { 
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
    i = i + 1
  }
  avgperfmeas50 = apply(perfmeas50,2,mean)
  avgperfmeas80 = apply(perfmeas80,2,mean)
  list(perfmeas50byfold=perfmeas50, avgperfmeas50=avgperfmeas50,
       perfmeas80byfold=perfmeas80, avgperfmeas80=avgperfmeas80)
}

source("helpers.R")
load("data/car_prices_subset_all.RData")
crossValidationCont(subset_selected, 3, train_tree, predict_tree)
crossValidationCont(subset_selected, 3, train_forest, predict_forest)

