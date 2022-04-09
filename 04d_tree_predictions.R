# Perform cross-validation for regression tree
library(rpart)
library(rpart.plot)

options(warn=-1)

source("./functions.R")

# Returns the quantile values of the trained regression tree model for holdout values
# Inputs:
# training: the training dataframe
# holdout: the holdout dataframe
# model: the trained regression tree model
# bottom: the bottom quantile, as a value between 0 and 1
# upper: the upper quantile, as a value between 0 and 1
# predictions: the holdout set predictions
# Returns: list of predictions and upper and lower interval bounds
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

# Find group position for each element of a vector
# Inputs:
# values: vector which should values that depends on the group
# groupValues: group values in the vector (hopefully unique)
# tolerance: tolerance for matching equality
# Returns group vector with membership label for each element of 'values'
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

# Run holdout set prediction on trained regression tree model and return
# summaries from 50% and 80% interval score evaluation.
# Plot the trained decision tree and residuals
predict_tree <- function(model, training, holdout, fold) {
  rpart.plot(model)
  predictions = predict(model,newdata=holdout,type="vector")
  pred50Int=get_quantiles(training, holdout, model, 0.25, 0.75, predictions)
  pred80Int=get_quantiles(training, holdout, model, 0.1, 0.9, predictions)
  
  IS50=intervalScore(pred50Int,holdout$price,0.5)
  IS80=intervalScore(pred80Int,holdout$price,0.8)
  out=rbind(IS50$summary,IS80$summary)
  resids_holdout = (holdout$price - predictions)
  plot(predictions, resids_holdout, ylab = "Residuals", xlab = "Fitted Values", main = sprintf("Fold %d", fold))
  out
}

# Train the regression tree on the training set
train_tree <- function(training, holdout) {
  rpart(price~.,data=training)
}

# Run the cross validation
load("data/car_prices_subset.RData")
crossValidationCont(subset_selected, 3, train_tree, predict_tree, model_name="tree")
