dev.off()
main_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(main_path)

####  Libraries  ---------------------------------------------------------------------
source("Libraries.R")

#### Load data  -------------------------------------------------------------------
# We will use data from an interval period of 8 years which ends with 2022-08-23  
last_observed_day <- as.Date("2022-08-24")
first_observed_day <- last_observed_day %m+% years(-8)

# Let's define a ticker labels for indexes used in project 
labels_indexes <- c("AAPL", "MSFT", "NVDA", 
                    "C", "GS", "JPM", 
                    "DIS", "NFLX", "ACGL") # before 2022 Nov TWTR now ACGL
indn <- length(labels_indexes)

# Downloading daily variables
data <- tq_get(x = labels_indexes, from = first_observed_day, to = last_observed_day)
data %>% 
  group_by(symbol) %>% 
  count 

# We will use data on Close for each business day
data <- data %>% 
  dplyr::select(c(date, symbol, close)) %>% 
  pivot_wider(names_from = symbol, values_from = close)

data_num <- data %>% 
  dplyr::select(-date)

# Any missing values
sum(is.na(data)) 

#### Convert to logarithmic increments  -----------------------------------------------
data_log <- apply(log(data_num), 2, diff) %>% 
  as.tibble()

data_log_date <- data_log %>%
  mutate(date = data$date[-1], .before = AAPL)

#### Correlation matrices  -----------------------------------------------------------
cor_mat <- list("pearson" = cor(data_log, method = 'pearson'),
                "spearman" = cor(data_log, method = 'spearman'),
                "kendall" = cor(data_log, method = 'kendall')
)

#### Plot time series, correlation matrices & histograms for indexes  ----------------
source('functions_plots.R')
indexes_plot(data, 'level')
indexes_plot(data_log_date, 'increment')
cor_mat_pt <- list('pearson' = plot_corr_matrix(data_num, 'pearson'),
                   'spearman' = plot_corr_matrix(data_num, 'spearman'),
                   'kendall' = plot_corr_matrix(data_num, 'kendall')
)
plot_hist(data_log)

#### Saving plots  -------------------------------------------------------------------
# source('saving_plots.R')

#### Fitting marginals distributions  ------------------------------------------------
source('fitting_marginals.R')

# Selected set for distributions:
# normal = 'normal', cauchy = 'cauchy', logistic = 'logistic', tstudent = 't'

marginals_fit <- function(data, 
                          marginals = c('normal', 't', 'cauchy', 'logistic')) {

  marginal_distr_nm <- marginals
  metric <- c('loglik', 'aic', 'bic', 'fitted_params')
  
  # Creating empty list used to filled by fitted marginal distributions
  fitted_marginals <- lapply(metric, 
                             function(x) 
                               as.list(sapply(marginal_distr_nm, function(x) NULL))
  ) %>% 
    setNames(metric)
  marginal_distr_after_fitting <- list()
  
  # Marginal distribution calibration
  for (j in 1:length(marginal_distr_nm)) {
    
    distribution <- marginal_distr_nm[j]
    
    for (ind_nm in labels_indexes) {
      marginal_distr_after_fitting[[ind_nm]] <- fitdistr(data_log[[ind_nm]],distribution)
    }
    
    fitted_marginals$fitted_params[[distribution]] <- 
      sapply(marginal_distr_after_fitting, function(x) x$estimate)
    fitted_marginals$aic[[distribution]] <- 
      sapply(marginal_distr_after_fitting, AIC)
    fitted_marginals$bic[[distribution]] <- 
      sapply(marginal_distr_after_fitting, BIC)
    fitted_marginals$loglik[[distribution]] <- 
      sapply(marginal_distr_after_fitting, logLik)
  }
  # Results
  criterion_names <- metric[metric != "fitted_params"]
  criterion_summary <-  fitted_marginals[criterion_names] %>% 
    lapply(function(x) as.data.frame(x, row.names = labels_indexes))
  
  return(list("params" = fitted_marginals,
              "metrics" = criterion_summary))
  
}

marginals_fitted <- marginals_fit(data_log)  

# Saving results
setwd(paste0(main_path,'/outputs/marginals/'))

for (i in criterion_names) {
  write.csv(marginals_fitted[["metrics"]][[i]], 
            paste0("marginals_",i,".csv"))
}
setwd(main_path)

#### Plot fitted densities
marginals_pt <- list("IT_comapnies" = grid.arrange(plot_fitted_densities(data_log, 'AAPL'), 
                                                   plot_fitted_densities(data_log, 'MSFT'), 
                                                   plot_fitted_densities(data_log, 'NVDA'), 
                                                   ncol=3),
                     "financials_comapnies" = grid.arrange(plot_fitted_densities(data_log, 'C'), 
                                                           plot_fitted_densities(data_log, 'GS'), 
                                                           plot_fitted_densities(data_log, 'JPM'), 
                                                           ncol=3),
                     "services_comapnies" = grid.arrange(plot_fitted_densities(data_log, 'DIS'), 
                                                         plot_fitted_densities(data_log, 'NFLX'), 
                                                         plot_fitted_densities(data_log, 'ACGL'), 
                                                         ncol=3)
)

# Saving results
setwd(paste0(main_path,'/figures/'))

for (i in names(marginals_pt)) {
  jpeg(filename = paste0(i, ".jpeg"),
       width = 1600, height = 700)
  plot(marginals_pt[[i]])
  dev.off()
}

setwd(main_path)

##### COPULA CALIBRATION ----------------------------------------------------------
####  Elliptical & Archimedean
# Transform the marginals to the unit interval
pseudo_data <- pobs(data_log)

# Define copulas objects
copulas <- list(normal = normalCopula(dim = indn, dispstr = "un"),
               t = tCopula(dim = indn, dispstr = "un"),
               frank = frankCopula(dim = indn),
               clayton = claytonCopula(dim = indn),
               gumbel = gumbelCopula(dim = indn))

# Fitting copulas using 'mpl' - Maximum pseudo-likelihood estimator
ellip_arch <- sapply(copulas,
                         function(x) 
                           copula::fitCopula(x, data = pseudo_data, method = "mpl"))

metric_ellip_arch <- list('AIC' = sapply(ellip_arch, AIC),
                         'BIC' = sapply(ellip_arch, BIC),
                         'Log-likelihood' = sapply(ellip_arch, logLik)
)

ellip_arch %>% print()
metric_ellip_arch %>% print()

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