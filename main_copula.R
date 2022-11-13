dev.off()
# uploading libraries used in project
library(tidyquant) 
library(ggplot2)
library(dplyr)
library(tidyr)
library(GGally)
library(xts)
library(tidyverse)
library(MASS)
library(xtable)

# to modelling we will use an interval period of 8 years which ends with 2022-08-23  
last_observed_day <- as.Date("2022-08-23")
first_observed_day <- last_observed_day %m+% years(-8)

# let's define a ticker labels for indexes used in project 
labels_indexes <- c("AAPL", "MSFT", "NVDA", "C", "GS", "JPM", "DIS", "NFLX", "TWTR")

# downloading daily variables
data_all <- tq_get(x = labels_indexes, from = first_observed_day, to = last_observed_day)
data_all %>% head
data_all %>% group_by(symbol) %>% count 

# we will use data on Close for each business day
data <- data_all %>% dplyr::select(c(date, symbol, close)) %>% pivot_wider(names_from = symbol, values_from = close)
data %>% head

# any missing values
sum(is.na(data)) 

# convert to logarithmic increments
data_log <- apply(log(data %>% dplyr::select(-date)), 2, diff) %>% as.data.frame()
data_log %>% head
data_log %>% str
data_log_tibble <- data_log %>% mutate('date' = data$date[-1], .before = AAPL) %>% as_tibble()

setwd('C:/Users/edyta/Desktop/GitHub/repos/copula_modelling')
source('plot_indexes.R')

# plot time series, correlation matrices & histograms for indexes
indexes_plot(data, 'level')
indexes_plot(data_log_tibble, 'increment')
plot_corr_matrix(data_log, 'pearson')
plot_corr_matrix(data_log, 'spearman')
plot_corr_matrix(data_log, 'kendall')
plot_hist(data_log)

# fitting marginals distributions
source('fitting_marginals.R')