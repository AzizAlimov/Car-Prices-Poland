# Perform cross-validation for OLS, WLS and transformed OLS as described
# in the report

set.seed(123)
source("functions.R")
load("data/car_prices_subset.RData")

# Train OLS model on training set
train_regression <- function(training, holdout) {
  lm(price~mark_cat+year+mileage_cat+log_vol_engine+fuel+car_type,data=training)
}

# Train WLS model on training set, using the year variable as the weights
train_regression_weights <- function(training, holdout) {
  lm(price~mark_cat+year+mileage_cat+log_vol_engine+fuel+car_type,data=training, weights = 1/(training$year)^2)
}

# Train transformed OLS on log(price)
train_regression_log <- function(training,holdout) {
  lm(I(log(price))~mark_cat+year+mileage_cat+log_vol_engine+fuel+car_type,data=training)
}

# Run holdout set prediction on weighted OLS model and return
# summaries from 50% and 80% interval score evaluation. 
# Plot the transformed residuals for that fold
predict_regression_log <- function(model, training, holdout, fold) {
  pred50 = predict(model,newdata=holdout,interval="prediction",level=0.5)
  pred50 = exp(pred50)
  
  pred80 = predict(model,newdata=holdout,interval="prediction",level=0.8)
  pred80 = exp(pred80)
  
  IS50=intervalScore(pred50,holdout$price,0.5)
  IS80=intervalScore(pred80,holdout$price,0.8)
  out=rbind(IS50$summary,IS80$summary)
  
  predicted = predict(model,newdata=holdout)
  resids_holdout = (holdout$price-exp(predicted))
  plot(exp(predicted), resids_holdout, main = sprintf("Fold %d", fold), ylab = "Residuals", xlab = "Fitted Values")
  out
}

# Run holdout set prediction on WLS model and return
# summaries from 50% and 80% interval score evaluation. 
# Plot the residuals for that fold
predict_regression_weights <- function(model, training, holdout, fold) {
  pred50Int = predict(model,newdata=holdout,interval="prediction",level=0.5, weights = 1/(holdout$year)^2)
  pred80Int = predict(model,newdata=holdout,interval="prediction",level=0.8, weights = 1/(holdout$year)^2)
  
  IS50=intervalScore(pred50Int,holdout$price,0.5)
  IS80=intervalScore(pred80Int,holdout$price,0.8)
  out=rbind(IS50$summary,IS80$summary)
  
  predicted = predict(model,newdata=holdout, weights = 1/(holdout$year)^2)
  resids_holdout = (holdout$price-predicted)
  plot(predicted, resids_holdout, main = sprintf("Fold %d", fold), ylab = "Residuals", xlab = "Fitted Values")
  out
}

# Run holdout set prediction on OLS model and return
# summaries from 50% and 80% interval score evaluation. 
# Plot the residuals for that fold 
predict_regression <- function(model, training, holdout, fold) {
  plot(model$fitted.values, model$residuals, main = sprintf("Fold %d", fold), xlab = "Fitted Values", ylab = "Residuals")
  pred50Int = predict(model,newdata=holdout,interval="prediction",level=0.5)
  pred80Int = predict(model,newdata=holdout,interval="prediction",level=0.8)
  
  IS50=intervalScore(pred50Int,holdout$price,0.5)
  IS80=intervalScore(pred80Int,holdout$price,0.8)
  out=rbind(IS50$summary,IS80$summary)
  predicted = predict(model,newdata=holdout)
  resids_holdout = (holdout$price-predicted)
  plot(predicted, resids_holdout, main = sprintf("Fold %d", fold), ylab = "Residuals", xlab = "Fitted Values")
  out
}

crossValidationCont(subset_selected, 3, train_regression, predict_regression, model_name="OLS")
crossValidationCont(subset_selected, 3, train_regression_weights, predict_regression_weights, model_name="WLS")
crossValidationCont(subset_selected, 3, train_regression_log, predict_regression_log, model_name="trans_OLS")

