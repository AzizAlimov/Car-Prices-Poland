
dat_orig = read.csv(file='data/Car_Prices_Poland_Kaggle.csv')
dat = dat_orig
dat$price_pln = dat$price
dat$price = dat$price * 0.23
dat = dat[dat$price < 200000,]
dat = dat[dat$price > 750,]

dat = dat[dat$year > 1980,]
dat = dat[dat$mileage < 999999,]

library(dplyr)
library(forcats)
library(ggplot2)

dat_filtered = dat %>%
  group_by(model) %>%
  filter(n() >= 900)

dat_filtered = dat_filtered %>% group_by(province) %>% filter(n() >= 50) %>% ungroup() %>% as.data.frame()
table(dat_filtered$province)

plt = ggplot(dat_filtered) +
  geom_bar(aes(x = forcats::fct_infreq(model)))

plt + labs(x = "Car Model", y = "Count") + scale_x_discrete(label=abbreviate)


length(unique(dat_filtered[['model']]))
sum(dat_filtered$model=="")
unique(dat_filtered[['model']])

#x = subset(dat_filtered, fuel %in% c('Electric'))
#x = subset(dat_filtered, fuel %in% c('LPG'))
#x = subset(dat_filtered, fuel %in% c('Hybrid'))
#x = subset(dat_filtered, fuel %in% c('CNG'))

dat_filtered$fuel[dat_filtered$fuel == 'CNG'] = 'NaturalGas'
dat_filtered$fuel[dat_filtered$fuel == 'LPG'] = 'NaturalGas'
dat_filtered$fuel[dat_filtered$fuel == 'Electric'] = 'Hybrid_Electric'
dat_filtered$fuel[dat_filtered$fuel == 'Hybrid'] = 'Hybrid_Electric'

dat_filtered[, c('mark_cat')] = NA
dat_filtered$mark_cat[dat_filtered$mark %in% c('mini', 'mercedes-benz', 'volvo', 'bmw', 'audi')] = 'Euro_lux'
dat_filtered$mark_cat[dat_filtered$mark %in% c('hyundai', 'nissan', 'honda', 'toyota', 'kia', 'mazda', 'mitsubishi')] = 'Asia'
dat_filtered$mark_cat[dat_filtered$mark %in% c('opel', 'volkswagen', 'renault', 'skoda', 'alfa-romeo', 'citroen', 'fiat', 'peugeot', 'seat')] = 'Euro'
dat_filtered$mark_cat[dat_filtered$mark %in% c('ford', 'chevrolet')] = 'US'

length(unique(dat_filtered[['mark_combined']]))
sum(is.na(dat_filtered$mark_combined))
unique(dat_filtered[['mark_combined']])


dat_filtered$mark_model = paste(dat_filtered$mark, "_", dat_filtered$model, sep="")
length(unique(dat_filtered[['mark_model']]))
unique(dat_filtered[['mark_model']])

#dat_filtered$model_gen = paste(dat_filtered$model, "_", dat_filtered$generation_name, sep="")
#length(unique(dat_filtered[['model_gen']]))
#unique(dat_filtered[['model_gen']])



small = c("skoda_fabia", "toyota_yaris", "renault_clio", "opel_corsa", "volkswagen_polo", "ford_fiesta")
medium = c("seat_leon", "peugeot_308", "nissan_qashqai", "kia_sportage", "kia_ceed", "hyundai_i30", "skoda_octavia", "renault_megane", "ford_focus", "opel_astra", "audi_a3", "bmw_seria-1", "volkswagen_golf")
large = c("mazda_6", "skoda_superb", "ford_mondeo", "ford_kuga", "opel_insignia", "audi_a4", "bmw_seria-3", "volkswagen_passat")
executive = c("volvo_xc-90", "audi_a6", "bmw_seria-5", "mercedes-benz_c-klasa", "mercedes-benz_e-klasa")
mpv = c("opel_zafira")

dat_filtered[, c('car_type')] = NA
dat_filtered$car_type[dat_filtered$mark_model %in% small] = 'small'
dat_filtered$car_type[dat_filtered$mark_model %in% medium] = 'medium'
dat_filtered$car_type[dat_filtered$mark_model %in% large] = 'large'
dat_filtered$car_type[dat_filtered$mark_model %in% executive] = 'executive'
dat_filtered$car_type[dat_filtered$mark_model %in% mpv] = 'mpv'
sum(is.na(dat_filtered$car_type))
unique(dat_filtered[['car_type']])
table(dat_filtered$car_type)




hist(log(dat_filtered$vol_engine+1000), main ="Histogram of Engine Volume", xlab = "Engine Volume")
summary(log(dat_filtered$vol_engine+1000))


hist(dat_filtered$year, main ="Histogram of Year", xlab = "Year")
summary(dat_filtered$year)

hist(dat_filtered$mileage, breaks=100, main ="Histogram of Mileage", xlab = "Mileage")
summary(dat_filtered$mileage, breaks=100)
hist(dat_filtered$mileage^(1/3), main ="Histogram of Mileage", xlab = "Mileage")
summary(dat_filtered$mileage^(1/3))
hist(log(dat_filtered$mileage+1000), main ="Histogram of Mileage", xlab = "Mileage")
summary(log(dat_filtered$mileage+1000))


quantile(dat_filtered$mileage, probs = seq(0, 1, 1/6)) 

library(magrittr)
library(tibble)

myTransform0 = function(dataset)
{ 
  dataTransform= as_tibble(dataset) %>%
    dplyr::mutate( log_vol_engine=log(vol_engine+1000),
                    mileage_cat=cut(mileage, mileage_breaks)) %>%
    dplyr::select( price, mark, mark_model, mark_cat, year, mileage, mileage_cat,
                   vol_engine, log_vol_engine, province, fuel, car_type)
  dataTransform
}

mileage_breaks = c(-Inf, 50000, 100000, 150000, 200000, 250000, Inf)

dat_processed = myTransform0(dat_filtered)

table(dat_processed$mileage_cat)
sum(is.na(dat_processed$mileage_cat))
sum(is.na(dat_processed$log_vol_engine))

selectLess = function(dataset)
{ 
  dataTransform= as_tibble(dataset) %>%
    dplyr::select( price, mark_cat, year, mileage_cat,
                  log_vol_engine, province, fuel, car_type)
  dataTransform
}

dat_selected = selectLess(dat_processed)

save(file="data/car_prices_transformed_all.RData", dat_processed)
save(file="data/car_prices_transformed.RData", dat_selected)





