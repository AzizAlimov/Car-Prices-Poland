set.seed(123)
source("helpers.R")
load("data/car_prices_subset.RData")

train_regression <- function(training, holdout) {
  lm(price~mark_cat+year+mileage_cat+log_vol_engine+fuel+car_type,data=training)
}

train_regression_weights <- function(training, holdout) {
  lm(price~mark_cat+year+mileage_cat+log_vol_engine+fuel+car_type,data=training, weights = 1/(training$year)^2)
}

train_regression_log <- function(training,holdout) {
  lm(I(log(price))~mark_cat+year+mileage_cat+log_vol_engine+fuel+car_type,data=training)
}

predict_regression_log <- function(model, training, holdout, fold) {
  res = exp(model$model$`I(log(price))`)  - exp(model$fitted.values)
  print(res)
  print(model$fitted.values)
  plot(exp(model$fitted.values), res, main = sprintf("Fold %d", fold), xlab = "Fitted Values", ylab = "Residuals")
  
  pred50 = predict(model,newdata=holdout,interval="prediction",level=0.5)
  pred50 = exp(pred50)
  
  pred80 = predict(model,newdata=holdout,interval="prediction",level=0.8)
  pred80 = exp(pred80)
  
  IS50=intervalScore(pred50,holdout$price,0.5)
  IS80=intervalScore(pred80,holdout$price,0.8)
  out=rbind(IS50$summary,IS80$summary)
  out
}

predict_regression_weights <- function(model, training, holdout, fold) {
  plot(model$fitted.values, model$residuals, main = sprintf("Fold %d", fold), xlab = "Fitted Values", ylab = "Residuals")
  pred50Int = predict(model,newdata=holdout,interval="prediction",level=0.5, weights = 1/(holdout$year)^2)
  pred80Int = predict(model,newdata=holdout,interval="prediction",level=0.8, weights = 1/(holdout$year)^2)
  
  IS50=intervalScore(pred50Int,holdout$price,0.5)
  IS80=intervalScore(pred80Int,holdout$price,0.8)
  out=rbind(IS50$summary,IS80$summary)
  out
}

predict_regression <- function(model, training, holdout, fold) {
  plot(model$fitted.values, model$residuals, main = sprintf("Fold %d", fold), xlab = "Fitted Values", ylab = "Residuals")
  pred50Int = predict(model,newdata=holdout,interval="prediction",level=0.5)
  pred80Int = predict(model,newdata=holdout,interval="prediction",level=0.8)
  
  IS50=intervalScore(pred50Int,holdout$price,0.5)
  IS80=intervalScore(pred80Int,holdout$price,0.8)
  out=rbind(IS50$summary,IS80$summary)
  out
}

crossValidationCont(subset_selected, 3, train_regression, predict_regression)
crossValidationCont(subset_selected, 3, train_regression_weights, predict_regression_weights)
crossValidationCont(subset_selected, 3, train_regression_log, predict_regression_log)

