library(lightgbm)
library(dplyr)

load("data/car_prices_subset.RData")

dat_one_hot <- subset_selected
dat_one_hot$mark_cat <- as.factor(dat_one_hot$mark_cat) 
dummies <- data.frame(model.matrix( ~ 0 + mark_cat, dat_one_hot))  # create one-hot dummies
dat_one_hot <- cbind(dat_one_hot %>% select(-mark_cat), dummies)


dat_one_hot$mileage_cat <- as.factor(dat_one_hot$mileage_cat) 
dummies <- data.frame(model.matrix( ~ 0 + mileage_cat, dat_one_hot))  # create one-hot dummies
dat_one_hot <- cbind(dat_one_hot %>% select(-mileage_cat), dummies)

dat_one_hot$car_type <- as.factor(dat_one_hot$car_type) 
dummies <- data.frame(model.matrix( ~ 0 + car_type, dat_one_hot))  # create one-hot dummies
dat_one_hot <- cbind(dat_one_hot %>% select(-car_type), dummies)

dat_one_hot$fuel <- as.factor(dat_one_hot$fuel) 
dummies <- data.frame(model.matrix( ~ 0 + fuel, dat_one_hot))  # create one-hot dummies
dat_one_hot <- cbind(dat_one_hot %>% select(-fuel), dummies)

save(file="data/one_hot_subset.RData", dat_one_hot)


train = dat_one_hot[1:10000,]
test = dat_one_hot[10001:12000,] 


train_labels = train$price
test_labels = test$price

train = subset(train, select=-c(price))
test = subset(test, select=-c(price))
train_matrix = as.matrix(train)
test_matrix = as.matrix(test)

lgb_train = lgb.Dataset(data=as.matrix(train), label=train_labels)
#lgb.Dataset.set.categorical(lgb_train, c(1L, 3L, 5L, 6L))


lgb_params = list(objective="quantile", 
                  alpha=0.5, 
                  reg_alpha=0.1,
                  reg_lambda=0.1,
                  num_leaves=100,
                  subsample=0.9,
                  min_child_samples=40,
                  learning_rate=0.1,
                  seed=123)

lgb_model = lgb.train(params=lgb_params, data=lgb_train, nrounds=100)

rsquare = function(y_actual,y_predict){
  cor(y_actual,y_predict)^2
}

train_pred = predict(lgb_model, as.matrix(train))
sqrt(mean((train_pred - train_labels)^2))
a = rsquare(train_labels, train_pred)

resids = (train_labels-train_pred)
plot(train_pred, resids)

test_pred = predict(lgb_model, as.matrix(test))
resids_test = (test_labels-test_pred)
plot(test_pred, resids_test)
sqrt(mean((test_pred - test_labels)^2))
rsquare(test_labels, test_pred)
lgb.importance(lgb_model)


lgb_params_25 = list(objective="quantile", 
                  alpha=0.25, 
                  reg_alpha=0.1,
                  reg_lambda=0.1,
                  num_leaves=100,
                  subsample=0.9,
                  min_child_samples=40,
                  learning_rate=0.1,
                  seed=123)
lgb_model_25 = lgb.train(params=lgb_params_25, data=lgb_train, nrounds=100)
lgb_params_75 = list(objective="quantile", 
                     alpha=0.75, 
                     reg_alpha=0.1,
                     reg_lambda=0.1,
                     num_leaves=100,
                     subsample=0.9,
                     min_child_samples=40,
                     learning_rate=0.1,
                     seed=123)
lgb_model_75 = lgb.train(params=lgb_params_75, data=lgb_train, nrounds=100)
lgb_params_10 = list(objective="quantile", 
                     alpha=0.1, 
                     reg_alpha=0.1,
                     reg_lambda=0.1,
                     num_leaves=100,
                     subsample=0.9,
                     min_child_samples=40,
                     learning_rate=0.1,
                     seed=123)
lgb_model_10 = lgb.train(params=lgb_params_10, data=lgb_train, nrounds=100)
lgb_params_90 = list(objective="quantile", 
                     alpha=0.9, 
                     reg_alpha=0.1,
                     reg_lambda=0.1,
                     num_leaves=100,
                     subsample=0.9,
                     min_child_samples=40,
                     learning_rate=0.1,
                     seed=123)
lgb_model_90 = lgb.train(params=lgb_params_90, data=lgb_train, nrounds=100)

train_pred25 = predict(lgb_model_25, train_matrix)
train_pred75 = predict(lgb_model_75, train_matrix)
train_pred10 = predict(lgb_model_10, train_matrix)
train_pred90 = predict(lgb_model_90, train_matrix)

test_pred25 = predict(lgb_model_25, test_matrix)
test_pred75 = predict(lgb_model_75, test_matrix)
test_pred10 = predict(lgb_model_10, test_matrix)
test_pred90 = predict(lgb_model_90, test_matrix)

head(test_pred90)
head(test_pred75)
head(test_pred)
head(test_pred25)
head(test_pred10)


intervalScore = function(predObj,actual,level)
{ n = nrow(predObj)
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
# summary with level, average length, interval score, coverage rate
imiss = which(ilow | ihigh)
list(summary=summ, imiss=imiss)
}

pred50 = cbind(test_pred, test_pred25, test_pred75)
is50 = intervalScore(pred50, test_labels, 0.5)
is50$summary

pred80 = cbind(test_pred, test_pred10, test_pred90)
is80 = intervalScore(pred80, test_labels, 0.80)
is80$summary


#pred80 = cbind(train_pred, train_pred10, train_pred90)
#is80 = intervalScore(pred80, train_labels, 0.80)
is80$summary
