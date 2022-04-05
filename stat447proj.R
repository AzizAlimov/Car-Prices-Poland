library(mlbench)
library(rpart)
library(rpart.plot)
library(VGAM)
library(randomForest)
library(pROC)
library(caret)
library(ggplot2)
library(quantregForest)

set.seed(123)
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

training_test_split <- function(Data, letter_1, letter_2, letter_3) {
  training_length <- (nrow(Data)%/%3*2)
  n <- nrow(Data)
  itrain=sample(n, training_length)
  training <- Data[itrain,]
  holdout <- Data[-itrain,]
  
  list(training=training, holdout=holdout)
}

get_letter_data <- function(letter_1, letter_2, letter_3) {
  data(LetterRecognition)
  Data = subset(LetterRecognition, lettr == letter_1|lettr == letter_2 |lettr == letter_3)
  Data
}

class_classification_rate <- function(letters, i, confusionmatrix, percent) {
  index = which(colnames(confusionmatrix) == letters[i])
  s = sprintf("classification rate for %s percent prediction interval letter %s: %s", percent, letters[i], round(sum(confusionmatrix[i, index])/sum(confusionmatrix[i,]), 4))
  print(s)
}

perform_prediction <- function(model, type, holdout, letter_1, letter_2, letter_3) {
  outpred=predict(model, type=type, newdata=holdout)
  predint=CategoryPredInterval(outpred,labels=c(letter_1,letter_2,letter_3))
  t50 <- table(holdout$lettr,predint$pred50)
  t80 <- table(holdout$lettr,predint$pred80)
  
  s = sprintf("classification rate for 50 percent prediction interval: %s", round(sum(holdout == predint$pred50)/nrow(holdout), 4))
  print(s)
  s = sprintf("classification rate for 80 percent prediction interval: %s", round(sum(holdout == predint$pred80)/nrow(holdout), 4))
  print(s)
  letters = c(letter_1, letter_2, letter_3)
  print(t50)
  print(t80)
  for (i in 1:length(letters)) {
    class_classification_rate(letters, i, t50, 50)
  }
  for (i in 1:length(letters)) {
    class_classification_rate(letters, i, t80, 80)
  }
}

predict_letters_regression_subset <- function(letter_1, letter_2, letter_3) {
  Data = get_letter_data(letter_1, letter_2, letter_3)
  split <- training_test_split(Data, letter_1, letter_2, letter_3)
  training <- split$training
  holdout <- split$holdout
  
  logit = vglm(lettr~ y.bar + x2bar + xy2br + x.ege + xegvy + y.ege + yegvx, multinomial(), data=training)
  predictions <- perform_prediction(logit, "response", holdout, letter_1, letter_2, letter_3)
  
}

predict_letters_regression_all <- function(letter_1, letter_2, letter_3) {
  Data = get_letter_data(letter_1, letter_2, letter_3)
  split <- training_test_split(Data, letter_1, letter_2, letter_3)
  training <- split$training
  holdout <- split$holdout
  
  logit = vglm(lettr~., multinomial(), data=training)
  predictions <- perform_prediction(logit, "response", holdout, letter_1, letter_2, letter_3)
  
}

predict_letters_tree <- function(letter_1, letter_2, letter_3, prune = FALSE) {
  Data = get_letter_data(letter_1, letter_2, letter_3)
  split <- training_test_split(Data, letter_1, letter_2, letter_3)
  training <- split$training
  holdout <- split$holdout
  RegTree=rpart(lettr~.,data=training, method="class")
  if (prune) {
    RegTree<-prune.rpart(RegTree,cp=.05)
  }
  
  perform_prediction(RegTree, "prob", holdout, letter_1, letter_2, letter_3)
  RegTree
}

predict_letters_forest <- function(letter_1, letter_2, letter_3) {
  Data = get_letter_data(letter_1, letter_2, letter_3)
  split <- training_test_split(Data, letter_1, letter_2, letter_3)
  training <- split$training
  holdout <- split$holdout
  forest = randomForest(lettr~.,data=training, importance=TRUE, proximity=TRUE)
  perform_prediction(forest, "prob", holdout, letter_1, letter_2, letter_3)
  forest
}

'''
Data = get_letter_data("B", "N", "R")
logit = vglm(lettr~., multinomial(), data=Data)
print(summary(logit))

# Removed variables with high p values, must be significant to differentiate
# between both pairs of categories
logit = vglm(lettr~ y.bar + x2bar + xy2br + x.ege + xegvy + y.ege + yegvx, multinomial(), data=Data)
print(summary(logit))

# Used above subset in making this function
predict_letters_regression_subset("B", "N", "R")

# Model with all explanatory variables
predict_letters_regression_all("B", "N", "R")

regtree <- predict_letters_tree("B", "N", "R", FALSE)
rpart.plot(regtree)
print(regtree)

regtree <- predict_letters_tree("B", "N", "R", TRUE)
rpart.plot(regtree)
print(regtree)

regforest <- predict_letters_forest("B", "N", "R")

ggplot(Data, aes(x = lettr, y=y.ege)) + 
  geom_boxplot()

ggplot(Data, aes(x = lettr, y=xegvy)) + 
  geom_boxplot()
'''

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
  
  RegTree=rpart(price~.,data=train)
  meanByTNode=tapply(train$price,RegTree$where,mean)
  Q25ByTNode=tapply(train$price,RegTree$where,quantile,prob=0.25)
  Q50ByTNode=tapply(train$price,RegTree$where,median)
  Q75ByTNode=tapply(train$price,RegTree$where,quantile,prob=0.75)
  ByTNode=cbind(meanByTNode,Q25ByTNode,Q50ByTNode,Q75ByTNode)
  
  Q10ByTNode=tapply(train$price,RegTree$where,quantile,prob=0.10)
  Q90ByTNode=tapply(train$price,RegTree$where,quantile,prob=0.90)
  meanpredRegTree=predict(RegTree,newdata=holdout,type="vector")
  head(meanpredRegTree)
  TNodeGroup=FindUniquePos(meanpredRegTree,meanByTNode)
  Q25predRegTree=Q25ByTNode[TNodeGroup]; Q75predRegTree=Q75ByTNode[TNodeGroup]
  pred50IntRegTree=cbind(meanpredRegTree,Q25predRegTree,Q75predRegTree)
  Q10predRegTree=Q10ByTNode[TNodeGroup]; Q90predRegTree=Q90ByTNode[TNodeGroup]
  pred80IntRegTree=cbind(meanpredRegTree,Q10predRegTree,Q90predRegTree)
  ISTree50=intervalScore(pred50IntRegTree,holdout$price,0.5)
  ISTree80=intervalScore(pred80IntRegTree,holdout$price,0.8)
  outTree=rbind(ISTree50$summary,ISTree80$summary)
  colnames(outTree)=c("level","avgleng","IS","cover")
  print(ISTree80$summary)
  print(sqrt(mean((meanpredRegTree - holdout$price)^2)))
  perfmeas[k,1:4] = ISTree80$summary
  perfmeas[k,5] = sqrt(mean((meanpredRegTree - holdout$price)^2))
  
  }
  avgperfmeas = apply(perfmeas,2,mean)
  list(perfmeasbyfold=perfmeas, avgperfmeas=avgperfmeas)
}

FindUniquePos=function(values,groupValues,tolerance=1.e-5)
{ ngroup = length(groupValues) # number of groups (nodes)
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


library(quantregForest)
load("/Users/azizalimov/CarPricesPoland/data/car_prices_subset_all.RData")
data <- training_test_split(subset_selected)
train <- data$training
holdout <- data$holdout

x = data.frame(train[,-1])
y = unlist(train[,1], use.names=FALSE)

qRF=quantregForest(x, y)
predRF = predict(qRF, what=c(.1,.25,.5,.75,.9), newdata=holdout[,-1])
colnames(outqRF)=c("level","avgleng","IS","cover")

intervalScore = function(predObj,actual,level)
{ n = nrow(predObj)
alpha = 1- level
ilow = (actual<predObj[,2]) # overestimation
ihigh = (actual>predObj[,3]) # underestimation
sumlength = sum(predObj[,3]-predObj[,2]) # sum of lengths of prediction intervals
print(sum(predObj[,3]-predObj[,2]))
sumlow = sum(predObj[ilow,2]-actual[ilow])*2/alpha
sumhigh = sum(actual[ihigh]-predObj[ihigh,3])*2/alpha
avglength = sumlength/n
IS = (sumlength+sumlow+sumhigh)/n # average length + average under/over penalties
cover = mean(actual>= predObj[,2] & actual<=predObj[,3])
summ = c(level,avglength,IS,cover)
# summary with level, average length, interval score, coverage rate
imiss = which(ilow | ihigh)
list(summary=summ, imiss=imiss)
}
IS50qRF=intervalScore(predRF[,c(3,2,4)],holdout$price, 0.5)
IS80qRF=intervalScore(predRF[,c(3,1,5)],holdout$price, 0.8)
outqRF=rbind(IS50qRF$summary,IS80qRF$summary)
colnames(outqRF)=c("level","avgleng","IS","cover")
print(outqRF)

model_importance = lgb.importance(lgb_model, percentage = TRUE)
lgb.plot.importance(model_importance, measure="Gain")



FindUniquePos=function(values,groupValues,tolerance=1.e-5)
{ ngroup = length(groupValues) # number of groups (nodes)
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

TNodeGroup=FindUniquePos(meanpredRegTree,meanByTNode)

library(rpart); library(rpart.plot)
RegTree=rpart(price~.,data=train); print(RegTree)
rpart.plot(RegTree)
meanByTNode=tapply(train$price,RegTree$where,mean)
Q25ByTNode=tapply(train$price,RegTree$where,quantile,prob=0.25)
Q50ByTNode=tapply(train$price,RegTree$where,median)
Q75ByTNode=tapply(train$price,RegTree$where,quantile,prob=0.75)
ByTNode=cbind(meanByTNode,Q25ByTNode,Q50ByTNode,Q75ByTNode)

Q10ByTNode=tapply(train$price,RegTree$where,quantile,prob=0.10)
Q90ByTNode=tapply(train$price,RegTree$where,quantile,prob=0.90)
meanpredRegTree=predict(RegTree,newdata=holdout,type="vector")
head(meanpredRegTree)
Q25predRegTree=Q25ByTNode[TNodeGroup]; Q75predRegTree=Q75ByTNode[TNodeGroup]
pred50IntRegTree=cbind(meanpredRegTree,Q25predRegTree,Q75predRegTree)
Q10predRegTree=Q10ByTNode[TNodeGroup]; Q90predRegTree=Q90ByTNode[TNodeGroup]
pred80IntRegTree=cbind(meanpredRegTree,Q10predRegTree,Q90predRegTree)
ISTree50=intervalScore(pred50IntRegTree,holdout$price,0.5)
ISTree80=intervalScore(pred80IntRegTree,holdout$price,0.8)
outTree=rbind(ISTree50$summary,ISTree80$summary)
colnames(outTree)=c("level","avgleng","IS","cover")
print(outTree)

