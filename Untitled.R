# Run holdout set prediction on trained quantile random forest model and return
# summaries from 50% and 80% interval score evaluation. 
# Print the importance plot for that fold and residuals
predict_forest <- function(forest, training, holdout, fold) {
  predRF = predict(forest, what=c(.1,.25,.5,.75,.9), newdata=holdout[,-1])
  IS50qRF=intervalScore(predRF[,c(3,2,4)],holdout$price, 0.5)
  IS80qRF=intervalScore(predRF[,c(3,1,5)],holdout$price, 0.8)
  outqRF=rbind(IS50qRF$summary,IS80qRF$summary)
  varImpPlot(forest)
  
  resids_holdout = (holdout$price - predRF[,3])
  plot(predRF[,3], resids_holdout, ylab = "Residuals", xlab = "Fitted Values", main = sprintf("Fold %d", fold))
  outqRF
}

# Train quantile regression forest on training set
train_forest <- function(training, holdout) {
  x = data.frame(training[,-1])
  y = unlist(training[,1], use.names=FALSE)
  quantregForest(x,y,importance=TRUE)
}

# Run the cross validation
load("data/car_prices_subset.RData")