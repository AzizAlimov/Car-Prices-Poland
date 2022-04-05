library(mlbench)
library(rpart)
library(rpart.plot)
library(VGAM)
library(randomForest)
library(pROC)
library(caret)
library(ggplot2)

options(warn=-1)


CategoryPredInterval = function(ProbMatrix,labels) {
  ncases=nrow(ProbMatrix)
  pred50=rep(NA,ncases)
  pred80=rep(NA,ncases)
  for(i in 1:ncases) {
    p=ProbMatrix[i,]
    ip=order(p,decreasing=T)
    pOrdered=p[ip] # decreasing order
    labelsOrdered=labels[ip] # decreasing order
    G=cumsum(pOrdered) # cumulative sum from largest
    k1=min(which(G>=0.5)) # level1= 0.5
    k2=min(which(G>=0.8)) # level2= 0.8
    pred1=labelsOrdered[1:k1]
    pred2=labelsOrdered[1:k2]
    pred50[i]=paste(pred1,collapse="")
    pred80[i]=paste(pred2,collapse="")
  }
  list(pred50=pred50, pred80=pred80)
}

crossValidationCont = function(df,K,a,nperfmeas=6)
{ set.seed(123)
  n = nrow(df)
  nhold = round(n/K) # size of holdout set 
  iperm = sample(n)
  perfmeas = matrix(0,K,nperfmeas)  
  models = vector(mode="list", length=3)
  for(k in 1:K)
  { indices = (((k-1)*nhold+1):(k*nhold))
  if( k==K ) indices = (((k-1)*nhold+1):n)
  indices = iperm[indices]
  train = df[-indices,]
  holdout = df[indices,]
  models[[k]] = lm(price~mark_cat+year+mileage_cat+log_vol_engine+fuel+car_type+gdpc,data=train)
  pred = predict(models[[k]],newdata=holdout,interval="prediction",level=a)
  RSS = c(crossprod(models[[k]]$residuals))
  MSE = RSS / length(models[[k]]$residuals)
  x = intervalScore(pred, holdout$price, a)
  y= sqrt(MSE)
  z = summary(models[[k]])$r.squared
  perfmeas[k,1:4] = x$summary
  perfmeas[k,5] = y
  perfmeas[k,6] = z
  }
  avgperfmeas = apply(perfmeas,2,mean)
  list(perfmeasbyfold=perfmeas, avgperfmeas=avgperfmeas, models= models)
}

training_test_split <- function(Data) {
  training_length <- (nrow(Data)%/%3*2)
  n <- nrow(Data)
  itrain=sample(n, training_length) #
  training <- Data[itrain,]
  holdout <- Data[-itrain,]
  list(training=training, holdout=holdout)
}

perform_prediction <- function(model, type, holdout) {
  outpred=predict(model, type=type, newdata=holdout)
  t50 <- table(holdout$lettr,predint$pred50)
  t80 <- table(holdout$lettr,predint$pred80)
  
  s = sprintf("classification rate for 50 percent prediction interval: %s", round(sum(holdout == predint$pred50)/nrow(holdout), 4))
  print(s)
  s = sprintf("classification rate for 80 percent prediction interval: %s", round(sum(holdout == predint$pred80)/nrow(holdout), 4))
  print(s)
  print(t50)
  print(t80)
  for (i in 1:length(letters)) {
    class_classification_rate(letters, i, t50, 50)
  }
  for (i in 1:length(letters)) {
    class_classification_rate(letters, i, t80, 80)
  }
}

predict_tree <- function(Data, prune = FALSE) {
  split <- training_test_split(Data)
  training <- split$training
  holdout <- split$holdout
  RegTree=rpart(lettr~.,data=training, method="class")
  if (prune) {
    RegTree<-prune.rpart(RegTree,cp=.05)
  }
  
  perform_prediction(RegTree, "prob", holdout)
  RegTree
}

predict_forest <- function(Data) {
  split <- training_test_split(Data)
  training <- split$training
  holdout <- split$holdout
  forest = randomForest(lettr~.,data=training, importance=TRUE, proximity=TRUE)
  perform_prediction(forest, "prob", holdout)
  forest
}

crossValidationCont = function(df,K,a,nperfmeas=6)
{ set.seed(123)
  n = nrow(df)
  nhold = round(n/K) # size of holdout set 
  iperm = sample(n)
  perfmeas = matrix(0,K,nperfmeas)  
  models = vector(mode="list", length=3)
  for(k in 1:K)
  { indices = (((k-1)*nhold+1):(k*nhold))
  if( k==K ) indices = (((k-1)*nhold+1):n)
  indices = iperm[indices]
  train = df[-indices,]
  holdout = df[indices,]
  models[[k]] = lm(price~mark_cat+year+mileage_cat+log_vol_engine+fuel+car_type+gdpc,data=train)
  pred = predict(models[[k]],newdata=holdout,interval="prediction",level=a)
  RSS = c(crossprod(models[[k]]$residuals))
  MSE = RSS / length(models[[k]]$residuals)
  x = intervalScore(pred, holdout$price, a)
  y= sqrt(MSE)
  z = summary(models[[k]])$r.squared
  perfmeas[k,1:4] = x$summary
  perfmeas[k,5] = y
  perfmeas[k,6] = z
  }
  avgperfmeas = apply(perfmeas,2,mean)
  list(perfmeasbyfold=perfmeas, avgperfmeas=avgperfmeas, models= models)
}

logit = vglm(lettr~., multinomial(), data=Data)
print(summary(logit))