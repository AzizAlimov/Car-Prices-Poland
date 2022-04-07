library(dplyr)
library(forcats)
library(ggplot2)
library(magrittr)
library(tibble)
library(corrplot)

transform_price <- function(Data) {
  Data$price_pln = Data$price
  Data$price = Data$price * 0.23
  Data
}

transform_province <- function(Data) {
  Data$province[Data$city == "Warszawa"] = "Warszawa"
  Data$gdpc = NA
  Data$gdpc[Data$province == "Warszawa"] = 133.37
  Data$gdpc[Data$province == "Zachodniopomorskie"] = 50.7
  Data$gdpc[Data$province == "Wielkopolskie"] = 66.21
  Data$gdpc[Data$province == "Warmińsko-mazurskie"] = 42.57
  Data$gdpc[Data$province == "Świętokrzyskie"] = 43.65
  Data$gdpc[Data$province == "Śląskie"] = 60.09
  Data$gdpc[Data$province == "Pomorskie"] = 57.67
  Data$gdpc[Data$province == "Podlaskie"] = 44.52
  Data$gdpc[Data$province == "Podkarpackie"] = 41.94
  Data$gdpc[Data$province == "Opolskie"] = 47.72
  Data$gdpc[Data$province == "Mazowieckie"] = 51.63
  Data$gdpc[Data$province == "Małopolskie"] = 55.45
  Data$gdpc[Data$province == "Lubuskie"] = 49.39
  Data$gdpc[Data$province == "Lubelskie"] = 41.32
  Data$gdpc[Data$province == "Łódzkie"] = 58.84
  Data$gdpc[Data$province == "Kujawsko-pomorskie"] = 49.44
  Data$gdpc[Data$province == "Dolnośląskie"] = 67.15
  Data
}

transform_fuel <- function(Data) {
  Data$fuel[Data$fuel == 'CNG'] = 'NaturalGas'
  Data$fuel[Data$fuel == 'LPG'] = 'NaturalGas'
  Data$fuel[Data$fuel == 'Electric'] = 'Hybrid_Electric'
  Data$fuel[Data$fuel == 'Hybrid'] = 'Hybrid_Electric'
  Data
}

transform_mark <- function(Data) {
  Data[, c('mark_cat')] = NA
  Data$mark_cat[Data$mark %in% c('mini', 'mercedes-benz', 'volvo', 'bmw', 'audi')] = 'Euro_lux'
  Data$mark_cat[Data$mark %in% c('hyundai', 'nissan', 'honda', 'toyota', 'kia', 'mazda', 'mitsubishi')] = 'Asia'
  Data$mark_cat[Data$mark %in% c('opel', 'volkswagen', 'renault', 'skoda', 'alfa-romeo', 'citroen', 'fiat', 'peugeot', 'seat')] = 'Euro'
  Data$mark_cat[Data$mark %in% c('ford', 'chevrolet')] = 'US'
  Data$mark_model = paste(Data$mark, "_", Data$model, sep="")
  Data
}

transform_car_type <- function(Data) {
  small = c("skoda_fabia", "toyota_yaris", "renault_clio", "opel_corsa", "volkswagen_polo", "ford_fiesta")
  medium = c("seat_leon", "peugeot_308", "nissan_qashqai", "kia_sportage", "kia_ceed", "hyundai_i30", "skoda_octavia", "renault_megane", "ford_focus", "opel_astra", "audi_a3", "bmw_seria-1", "volkswagen_golf")
  large = c("mazda_6", "skoda_superb", "ford_mondeo", "ford_kuga", "opel_insignia", "audi_a4", "bmw_seria-3", "volkswagen_passat")
  executive = c("volvo_xc-90", "audi_a6", "bmw_seria-5", "mercedes-benz_c-klasa", "mercedes-benz_e-klasa")
  mpv = c("opel_zafira")
  
  Data[, c('car_type')] = NA
  Data$car_type[Data$mark_model %in% small] = 'small'
  Data$car_type[Data$mark_model %in% medium] = 'medium'
  Data$car_type[Data$mark_model %in% large] = 'large'
  Data$car_type[Data$mark_model %in% executive] = 'executive'
  Data$car_type[Data$mark_model %in% mpv] = 'mpv'
  Data
}

filter_outliers <- function(Data) {
  Data = Data %>%
    filter(price < 200000, price > 750, year > 1980, mileage < 999999) %>%
    group_by(model) %>%
    filter(n() >= 900) %>%
    group_by(province) %>%
    filter(n() >= 50) %>%
    ungroup() %>%
    as.data.frame()
  Data
}

transform_mileage_bin_vol_engine <- function(Data, mileage_breaks) { 
  as_tibble(Data) %>%
    dplyr::mutate( log_vol_engine=log(vol_engine+1000),
                   mileage_cat=cut(mileage, mileage_breaks)) %>%
    dplyr::select( price, mark, mark_model, mark_cat, year, mileage, mileage_cat,
                   vol_engine, log_vol_engine, province, fuel, car_type, gdpc)
}

filter_categories <- function(Data) { 
  as_tibble(Data) %>%
    dplyr::select( price, mark_cat, year, mileage_cat,
                   log_vol_engine, fuel, car_type, gdpc)
}

perform_transformations <- function(Data) {
  dat_filtered = transform_price(Data)
  dat_filtered = filter_outliers(dat_filtered)
  dat_filtered = transform_province(dat_filtered)
  dat_filtered = transform_fuel(dat_filtered)
  dat_filtered = transform_mark(dat_filtered)
  dat_filtered = transform_car_type(dat_filtered)
  
  mileage_breaks = c(-Inf, 50000, 100000, 150000, 200000, 250000, Inf)
  dat_processed = transform_mileage_bin_vol_engine(dat_filtered, mileage_breaks)
  dat_processed
}

select_subset <- function(Data, num) {
  set.seed(123)
  nn = nrow(dat_selected)
  nsubset = num
  isubset = sample(nn, nsubset)
  subset = dat_processed[isubset,]
  subset
}

spearman_corr <- function(Data, var_indices) {
  cordata= Data[,var_indices]
  corMat=cor(cordata,method="spearman")
  print(round(corMat,3))
  corrplot(corMat, method="number", type="upper", diag=FALSE)
}

dat = read.csv(file='./data/Car_Prices_Poland_Kaggle.csv')
dat_processed = perform_transformations(dat)
dat_selected = filter_categories(dat_processed)

save(file="data/car_prices_transformed_all.RData", dat_processed)
save(file="data/car_prices_transformed.RData", dat_selected)

subset <- select_subset(dat_processed, 12000)
subset_selected = filter_categories(subset)

spearman_corr(subset, c(1,5,6,8,13))

save(file="data/car_prices_subset.RData", subset_selected)
save(file="data/car_prices_subset_all.RData", subset)
