cars <- read.csv("~/Downloads/Car_Prices_Poland_Kaggle.csv")

length(unique(cars[['fuel']]))
sum(is.na(cars$fuel))
unique(cars[['fuel']])

library(ggplot2)
library(forcats)

barplot(table(cars$fuel))

plt = ggplot(cars) +
  geom_bar(aes(x = forcats::fct_infreq(fuel)))

table(cars$fuel)

plt + labs(x = "Fuel type")

ggplot(cars, aes(x = fct_reorder(fuel, price, .fun = median), y=price)) + 
  geom_boxplot() + labs(x = "Fuel type")


plt = ggplot(cars) +
  geom_bar(aes(x = forcats::fct_infreq(city)))
plt + labs(x = "City")
library(dplyr)
filtered = cars %>% group_by(city) %>% filter(n()>= 100) %>% ungroup()
less_than_10 = cars %>% group_by(city) %>% filter(n() < 10) %>% ungroup()
less_than_100 = cars %>% group_by(city) %>% filter(n() < 100) %>% ungroup()
table(filtered$city)
length(unique(cars[['city']]))


ggplot(cars, aes(x = fct_reorder(city, log(price), .fun = median), y=log(price))) + 
  geom_boxplot() + labs(x = "City")


table(cars$province)
cleaned_cars <- cars %>% group_by(province) %>% filter(n() >= 50) %>% ungroup() %>% as.data.frame()
table(cleaned_cars$province)
barplot(table(cleaned_cars$province))
plt = ggplot(cleaned_cars) +
  geom_bar(aes(x = forcats::fct_infreq(province)))
plt + labs(x = "Province") + scale_x_discrete(label=abbreviate)

ggplot(cleaned_cars, aes(x = fct_reorder(province, log(price), .fun = median), y=log(price))) + 
  geom_boxplot() + labs(x = "Province") + scale_x_discrete(label=abbreviate)

length(unique(dat[['model']]))
sum(is.na(dat$model))
unique(dat[['model']])