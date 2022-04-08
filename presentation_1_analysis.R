library(ggplot2)
library(forcats)
library(dplyr)
library(corrplot)

cars <- read.csv("./data/Car_Prices_Poland_Kaggle.csv")

print_info <- function(Data, category_name) {
  cat("Explanatory variable:", category_name, "\n")
  num_categories = length(unique(Data[[category_name]]))
  cat("Num categories: ", length(unique(Data[[category_name]])), "\n")
  num_na = sum(is.na(Data[[category_name]]))
  cat("Num NA:", num_na, "\n")
  print(table(Data[[category_name]]))
}

make_boxplot <- function(Data, category_name, x_name) {
    plot = ggplot(Data) +
      aes(x = fct_reorder(.data[[category_name]], price, .fun=median), y=price) +
      geom_boxplot() +
      labs(x = x_name)
}

make_hist <- function(Data, category_name, x_name, continuous) {
  if (continuous) {
    if (category_name == "year") {
      bin_size = 35
    } else {
      bin_size = 300
    }
    ggplot(Data, aes(x=.data[[category_name]])) +
      geom_histogram(bins=bin_size) +
      labs(x = x_name)
  } else {
    ggplot(Data) +
      aes(x = fct_reorder(.data[[category_name]], price, .fun = median)) + 
      geom_histogram(stat="count") + labs(x = x_name)
  }
}

make_scatterplot <- function(Data, category_name, x_name) {
  plot(Data[[category_name]], Data$price, ylab="Price in USD", xlab=x_name,main=sprintf("Scatterplot  Price vs %s", x_name))
}

make_plots_print_info <- function(Data, category_name, x_name, make_boxplot=TRUE, make_hist = TRUE, continuous = FALSE) {
  if (make_boxplot) {
    plot(make_boxplot(Data, category_name, x_name))
    logged_price = Data
    logged_price$price = log(Data$price)
    plot(make_boxplot(logged_price, category_name, x_name))
  }
  if (make_hist) {
    plot(make_hist(Data, category_name, x_name, continuous))
  }
  if (continuous) {
    make_scatterplot(Data, category_name, x_name)
  } else {
    print_info(Data, category_name)
  }
}

clean_data <- function(Data) {
  Data = Data %>% group_by(province) %>% filter(n() >= 50) %>% ungroup() %>% as.data.frame()
  Data %>% filter(year > 1987) 
}

cars <- clean_data(cars)

make_plots_print_info(cars, 'fuel', 'Fuel type')

nrow(unique(cars['city']))
less_than_10 = cars %>% group_by(city) %>% filter(n() < 10) %>% ungroup()
nrow(unique(less_than_10['city']))
less_than_100 = cars %>% group_by(city) %>% filter(n() < 100) %>% ungroup()
nrow(unique(less_than_100['city']))

make_plots_print_info(cars, 'province', "Province")


# Barry's stuff starts here

make_plots_print_info(cars, "year", "Year", FALSE, TRUE)
make_plots_print_info(cars, "mileage", "Mileage", FALSE, TRUE, TRUE)
make_plots_print_info(cars, "vol_engine", "Engine Volume", FALSE, TRUE, TRUE)

cordata= cars[,c(5,6,7,11)]
corMat=cor(cordata,method="spearman") 
print(round(corMat,3))

hist(cars$price,xlab = "Price", main="Histogram of Price",breaks = 200)
hist(log(cars$price),xlab = "log(Price)", main="Histogram of log(Price)",breaks = 200)
cars$exp[cars$price>19228] <- "Expensive"
cars$exp[4830>cars$price] <- "Cheap"
cars$exp = as.factor(cars$exp)
cars$exp  = factor(cars$exp, levels=c("Cheap", "Average", "Expensive"))
plot(cars$exp, main="Binning of Price", xlab="Category of Price", ylab="Frequence")

