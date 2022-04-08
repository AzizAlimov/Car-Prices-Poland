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
