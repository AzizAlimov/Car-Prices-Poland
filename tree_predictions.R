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

source("./helpers.R")

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

predict_tree <- function(model, training, holdout, fold) {
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

predict_forest <- function(forest, training, holdout, fold) {
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

load("data/car_prices_subset.RData")
crossValidationCont(subset_selected, 3, train_tree, predict_tree)
crossValidationCont(subset_selected, 3, train_forest, predict_forest)

