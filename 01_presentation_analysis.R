# Generates the initial plots and stats used for the first project presentation


library(ggplot2)
library(forcats)
library(dplyr)
library(corrplot)

cars <- read.csv("./data/Car_Prices_Poland_Kaggle.csv")

# Given dataframe and categorical variable, print number of categories and 
# number of NA values
print_info <- function(Data, category_name) {
  cat("Explanatory variable:", category_name, "\n")
  num_categories = length(unique(Data[[category_name]]))
  cat("Num categories: ", length(unique(Data[[category_name]])), "\n")
  num_na = sum(is.na(Data[[category_name]]))
  cat("Num NA:", num_na, "\n")
  print(table(Data[[category_name]]))
}

# Given dataframe and categorical variable, plot the boxplot vs price. x_name is
# x-axis label
make_boxplot <- function(Data, category_name, x_name) {
    plot = ggplot(Data) +
      aes(x = fct_reorder(.data[[category_name]], price, .fun=median), y=price) +
      geom_boxplot() +
      labs(x = x_name)
}

# Given dataframe and variable, plot the frequency histogram. x_name is x-axis label
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

# Given dataframe and variable, plot the scatterplot of variable vs price.
make_scatterplot <- function(Data, category_name, x_name) {
  plot(Data[[category_name]], Data$price, ylab="Price in USD", xlab=x_name,main=sprintf("Scatterplot  Price vs %s", x_name))
}

# Given dataframe and variable, make boxplot, histogram, and/or scatterplot
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

# Clean up data from presentation. Convert price to USD.
clean_and_prepare_data <- function(Data) {
  Data$price = Data$price * 0.23
  Data$logprice = log(Data$price)
  Data = Data %>% group_by(province) %>% filter(n() >= 50) %>% ungroup() %>% as.data.frame()
  Data %>% filter(year > 1987) 
}

# Print Spearman correlation of categories
get_spearman_coor <- function(Data, categories) {
  cordata= Data[,categories]
  corMat=cor(cordata,method="spearman") 
  print(round(corMat,3))
}

cars <- clean_and_prepare_data(cars)

make_plots_print_info(cars, 'fuel', 'Fuel type')
make_plots_print_info(cars, 'province', "Province")
make_plots_print_info(cars, "year", "Year", FALSE, TRUE, TRUE)
make_plots_print_info(cars, "mileage", "Mileage (km)", FALSE, TRUE, TRUE)
make_plots_print_info(cars, "vol_engine", "Engine Volume (cc)", FALSE, TRUE, TRUE)
make_plots_print_info(cars, "price", "Price (USD)", FALSE, TRUE, TRUE)
make_plots_print_info(cars, "logprice", "Log Price Log(USD)", FALSE, TRUE, TRUE)
make_plots_print_info(cars, "mark", "Car Mark")
make_plots_print_info(cars, "model", "Car Model")

get_spearman_coor(cars, c(5,6,7,11))

save(file="data/presentation_data.RData", cars)
