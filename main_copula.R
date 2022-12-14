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
library(copula)
library(rvinecopulib)
library(e1071)

# to modelling we will use an interval period of 8 years which ends with 2022-08-23  
last_observed_day <- as.Date("2022-08-24")
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

# correlation matrixes
pearson_log <- cor(data_log, method = 'pearson')
spearman_log <- cor(data_log, method = 'spearman')
kendall_log <- cor(data_log, method = 'kendall')

main_github_path <- 'C:/Users/edyta/Desktop/GitHub/repos/copula_modelling'
setwd(main_github_path)
source('plot_indexes.R')

# plot time series, correlation matrices & histograms for indexes
indexes_plot(data, 'level')
indexes_plot(data_log_tibble, 'increment')
plot_corr_matrix(data_log, 'pearson')
plot_corr_matrix(data_log, 'spearman')
plot_corr_matrix(data_log, 'kendall')
plot_hist(data_log)

# saving plots
# source('saving_plots.R')

# fitting marginals distributions
source('fitting_marginals.R')
# plot fitted marginals distributions
source('plot_fitted_marginals.R')
# fitted parameters in final type used to copula calibration

# fit elliptical and archimedean copulas to data
# source('ellipt_archimed_copula_fitting.R')

# File with calibrated copulas and random sampling from copulas and correlation matrices plots
source('sample_from_calibrated_ellipt_archimed_copulas.R')

# fit vine copulas to data - c-vine, d-vine and r-vine structures under consideration
# source('vines_copula_fitting.R')

# checking criterion values when different families of pair-copulas are in usage
# NOT RUN!
# source('vines_copulas_family.R')
# plot scatter plot for pairs of indexes (real observed data and simulated data) 
# if you would like to change copula from witch simulated data comes run file 'sample_from_calibrated_*.R'
# and custom interested parameters
scatter_plot()
scatter_plot_vines()