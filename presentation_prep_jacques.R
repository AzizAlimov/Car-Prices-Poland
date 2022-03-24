
dat = read.csv(file='Car_Prices_Poland_Kaggle.csv')

dat$model_gen = paste(dat$model, dat$generation_name, sep=" ")
length(unique(dat[['model_gen']]))


summary(dat)
dat$mark[dat$mark == ""] <- NA
dat$model[dat$model == ""] <- NA
dat$generation_name[dat$generation_name == ""] <- NA

length(unique(dat[['mark']]))
sum(is.na(dat$mark))
unique(dat[['mark']])

library(ggplot2)
library(forcats)

barplot(table(dat$mark))

plt = ggplot(dat) +
  geom_bar(aes(x = forcats::fct_infreq(mark)))

plt + labs(x = "Car Mark", y = "Count") + scale_x_discrete(label=abbreviate)


plt = ggplot(dat) +
  geom_bar(aes(x = forcats::fct_infreq(model)))




length(unique(dat[['model']]))
sum(dat$model=="")
unique(dat[['model']])

table(dat$mark)


ggplot(dat, aes(x = fct_reorder(model, log(price), .fun = median), y=log(price))) + 
  geom_boxplot() + labs(x = "Model") +theme(
                                            axis.text.x=element_blank(),
                                            axis.ticks.x=element_blank())

ggplot(dat, aes(x = fct_reorder(model_gen, log(price), .fun = median), y=log(price))) + 
  geom_boxplot() + labs(x = "Model") +theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())

dat[46712,"generation_name"]

length(unique(dat[['generation_name']]))
sum(is.na(dat$generation_name))
unique(dat[['generation_name']])


dat_opel_agela = dat[dat$model=="agila",]
dat_opel_2008 = dat_opel_agela[dat_opel_agela$generation_name=="gen-b-2008",]
dat_opel_2000 = dat_opel_agela[dat_opel_agela$generation_name=="gen-a-2000-2007",]


table(dat_opel_2008$year)
table(dat_opel_2000$year)
