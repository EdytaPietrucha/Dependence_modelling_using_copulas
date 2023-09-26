dev.off()
main_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(main_path)

####  libraries & functions ----------------------------------------------------
source("libraries.R")
source('functions.R')

#### Load data  ----------------------------------------------------------------
# We will use data from following 8 years, period ends with 2022-08-23  
last_observed_day <- as.Date("2022-08-24")
first_observed_day <- last_observed_day %m+% years(-8)

# Let's define a ticker labels for indexes used in project 
labels_indexes <- c("AAPL", "MSFT", "NVDA", 
                    "C", "GS", "JPM", 
                    "DIS", "NFLX", "AMZN") # before 2022 Nov TWTR now ACGL as a replacement (AMZN - amazon?)
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

#### Convert to logarithmic increments  ----------------------------------------
data_log <- apply(log(data_num), 2, diff) %>% 
  as.tibble()

data_log_date <- data_log %>%
  mutate(date = data$date[-1], .before = AAPL)

#### Correlation matrices  -----------------------------------------------------
cor_mat <- list("pearson" = cor(data_log, method = 'pearson'),
                "spearman" = cor(data_log, method = 'spearman'),
                "kendall" = cor(data_log, method = 'kendall')
)

#### Plot time series, correlation matrices & histograms for indexes  ----------
indexes_plot(data, 'level')
indexes_plot(data_log_date, 'increment')
cor_mat_pt <- list('pearson' = plot_corr_matrix(data_log, 'pearson'),
                   'spearman' = plot_corr_matrix(data_log, 'spearman'),
                   'kendall' = plot_corr_matrix(data_log, 'kendall')
)
plot_hist(data_log)

#### Saving plots  -------------------------------------------------------------
source('saving_plots.R')

#### Fitting marginal distributions  -------------------------------------------
# Selected set for distributions:
# normal = 'normal', cauchy = 'cauchy', logistic = 'logistic', tstudent = 't'
marginals_fitted <- marginals_fit(data_log)  
# The best score has been reached for t-Student distribution for all marginals

# Saving results
setwd(paste0(main_path,'/outputs/marginals/'))
criterion_names <- c('loglik', 'aic', 'bic')
for (i in criterion_names) {
  write.csv(marginals_fitted[["metrics"]][[i]], 
            paste0("marginals_",i,".csv"))
}
setwd(main_path)

#### Plot fitted densities  ----------------------------------------------------
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
                                                         plot_fitted_densities(data_log, 'AMZN'), 
                                                         ncol=3)
)

# Saving results
setwd(paste0(main_path,'/figures/marginals/'))

for (i in names(marginals_pt)) {
  jpeg(filename = paste0(i, ".jpeg"),
       width = 1600, height = 700)
  plot(marginals_pt[[i]])
  dev.off()
}

setwd(main_path)

#### COPULA CALIBRATION --------------------------------------------------------
####  Elliptical & Archimedean  ------------------------------------------------
# Transform marginals to the unit interval
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
                           copula::fitCopula(x, 
                                             data = pseudo_data, 
                                             method = "mpl")
                     )

metric_ellip_arch <- list('AIC' = sapply(ellip_arch, AIC),
                         'BIC' = sapply(ellip_arch, BIC),
                         'Log-likelihood' = sapply(ellip_arch, logLik)
)

ellip_arch %>% 
  print()
metric_ellip_arch %>% 
  print()
# The best score has been reached for t-Student copula
setwd(paste0(main_path, "/outputs/"))
write.csv(t(as.data.frame(metric_ellip_arch)), 
          "metrics_elliptical_archimedean.csv")

##### Sampling from t-Student copula  ----------------------------------------------
# Sampling will be sensitive on seed, so we define default seed as 123
set.seed(123)

# As final marginal distributions the choice is a t-student distributions to all
# marginals as it gives us the best score for AIC, BIC and LogLik metrics
final_marginals <- 't' # in c('norm', 't', 'cauchy', 'logis')
params_list_after_fitting <- marginal_parameter_preprocess(final_marginals = final_marginals)

# Coefficients from t-Student copula
fitted_coef <- coef(ellip_arch[["t"]])
# Define correlation method used to copula calibration
cor_method <- 'spearman' # \in c("pearson", "kendall", "spearman")

fitted_copula <- tCopula(param = P2p(cor_mat[[cor_method]]),
        df = fitted_coef[names(fitted_coef) == "df"],
        dim = indn, 
        dispstr="un",
        df.fixed = T)

# Creating multivariate distribution for t-Studnet copula and t-Student marginals
join_distribution <- mvdc(copula=fitted_copula, 
                          margins=rep(final_marginals,9),
                          paramMargins=params_list_after_fitting)

# Define number of simulations
simn <- dim(data_log)[1]

# Random generator for a multivariate distribution
join_distribution_sample <- rMvdc(simn, join_distribution)

# Generate a random variate:	rt_ls(df, mu, sigma) =	rt(df)*sigma + mu
# Marginals parameters preparation
fit_params <- marginals_fitted$params[[final_marginals]]
location <- fit_params[if_else((fit_params %>% rownames() == 'm') %>% sum == 1, 'm', 'location'),]
scale <- fit_params[grepl('^s+', fit_params %>% row.names()) %>% which(),]

# Data scaling
sampling_from_copula <- matrix(nrow = dim(join_distribution_sample)[1],
                               ncol = dim(join_distribution_sample)[2])
for (i in 1:indn){
  sampling_from_copula[,i] <- join_distribution_sample[,i]*scale[i] + location[i]
}

sampling_from_copula <- as.data.frame(sampling_from_copula)
colnames(sampling_from_copula) <- labels_indexes
sampling_from_copula %>% 
  head

# cor_mat$pearson - cor(sampling_from_copula)
# plot correlation matrix
cor_nm <- "kendall" 
plot_corr_matrix(sampling_from_copula, cor_nm) + 
  labs(title = paste0(cor_nm,' correlation matrix for random sample from calibrated t-Student copula'),
       subtitle = paste0('Marginals come from ',final_marginals,' distribution'))

cor_mat[[cor_nm]] - cor(sampling_from_copula, method = cor_nm)

#### Generate sample from fitted Elliptical & Archimedean copulas  ---------------------

# As final marginal distributions the choice is a t-student distributions to all
# marginals as it gives us the best score for AIC, BIC and LogLik metrics
# For copula calibration we will use a spearman matrix

sim_t_123 <- simulate_from_copula(marginals = "t",
                     cor_method = 'spearman',
                     copula_nm = "t",
                     seed_fixed = 123)

# Saving simulations in csv file
seed_fixed <- 123
for (copula_nm in c('normal', 't', 'frank' , 'clayton', 'gumbel')) {
  
  simulated_data <- simulate_from_copula(marginals = "t",
                       cor_method = 'spearman',
                       copula_nm = copula_nm,
                       seed_fixed = 123)
  # Saving results
  setwd(paste0(main_path,'/outputs/simulations/'))
  write.csv(simulated_data, 
            paste0(copula_nm, "_seed_",seed_fixed,".csv"))
  
}
setwd(main_path)

####  Vines copulas  -------------------------------------------------------------------

# Define family of bicopulas used in the calibration
family_bicop <- c('archimedean', 'elliptical') 
# Define criterion for family selection
criterion_vines <- 'loglik' # criterion_vines

## C-vine structure selection - methodology is based on assumption that main root for each edge
# corresponds to index which has the highest value of column sum from Spearman correlation matrix
correlation_matrix <- cor_mat$spearman
cvine_struc <- cvine_structure(order(colSums(cor_mat$spearman)))

## D-vine structure selection - methodology is based on the assumption that companies from one sector are the most dependent
# and should be inseparably next to each other. For the analysis purpose we are running a simulation which
# randomly sets an order of the indexes from the same sector then fit a vine copula and return loglik number 

# DO NOT RUN!
source('selecting_dvine_structure.R')
dvine_struc_all <- fread('outputs/dvine_struc.csv')
# Select structure with the highest LogLik number & Convert to numeric
dvine_struc_seq <- dvine_struc_all %>% 
  filter(LogLik == max(LogLik)) %>%
  dplyr::select(Structure) %>%
  str_split(pattern = ",") %>% 
  unlist() %>%
  as.numeric()

dvine_struc <- dvine_structure(dvine_struc_seq)

## R-vie - Structure based  on Dissman's structure selection algorithm (https://cran.r-project.org/web/packages/rvinecopulib/rvinecopulib.pdf page 31)
rvine_struc <- NA

# Define vine copulas objects
vine_structures <-  list('dvine' = dvine_struc,
                         'cvine' = cvine_struc,
                         'rvine' = rvine_struc
)

# Fitting copulas using "mle" for maximum likelihood 
vines <- lapply(vine_structures,
                    function(x) 
                      vinecop(pseudo_data, 
                              structure = x, 
                              keep_data = TRUE, 
                              family_set = family_bicop, 
                              selcrit = criterion_vines, 
                              tree_crit = 'rho', 
                              par_method = 'mle')
)

metric_vines <- list('AIC' = sapply(vines, AIC),
                     'BIC' = sapply(vines, BIC),
                     'Log-likelihood' = sapply(vines, logLik)
)

vines %>% 
  print()
metric_vines %>% 
  print()

setwd(paste0(main_path, "/outputs/"))
write.csv(t(as.data.frame(metric_vines)), 
          "metrics_vines.csv")

# The best score has been reached for d-vine structure
# plot first two copula trees
plot(vines$dvine, edge_labels = "family_tau", tree = 1:2)
plot(vines$cvine, edge_labels = "family_tau", tree = 1:2)
plot(vines$rvine, edge_labels = "family_tau", tree = 1:2)

# Saving plots with vine trees
setwd(main_path)
source("trees_plots_save.R")

#### Different bi-copula families  ---------------------------------------------
# Vine structures can use different bi-copula families in calibration. 
# Below code investigate criterion scores for various families of pair-copulas in usage
# NOT RUN!
# source('vines_copulas_family.R')

#### Generate sample from fitted Vines copulas  --------------------------------
sim_dvine_123 <- simulate_from_vine_copula(marginals = "t",
                                     copula_nm = "dvine",
                                     seed_fixed = 123)

for (copula_nm in c('dvine', 'cvine', 'rvine')) {
  # Saving results
  
  simulated_data <- simulate_from_vine_copula(marginals = "t",
                                              copula_nm = copula_nm,
                                              seed_fixed = 123)
  
  setwd(paste0(main_path,'/outputs/simulations/'))
  write.csv(simulated_data, 
            paste0(copula_nm, "_seed_",seed_fixed,".csv"))
}

#### Correlation matrices for sampled joint distributions ----------------------
setwd(paste0(main_path,"/outputs/simulations/"))
simulations <- lapply(list.files(paste0(main_path,"/outputs/simulations/")), function(x) fread(x)) %>%
  setNames(gsub(".csv", "", list.files(paste0(main_path,"/outputs/simulations/"))))

setwd(paste0(main_path,'/figures/correlation/'))
for (csv in names(simulations)) {
  
  data_sim <- simulations[[csv]]
  
  cor_nm <- "spearman"
  jpeg(filename = paste0(cor_nm,"_copula_", csv,".jpeg"),
       width = 1200, height = 700)
  
  # plot correlation matrix
  plot_corr_matrix(data_sim[,-"V1"], cor_nm) + 
    labs(title = paste0(stringr::str_to_title(cor_nm),' correlation matrix for random sample from calibrated ', str_split(csv, "_")[[1]][1], ' copula'),
         subtitle = paste0('Marginals come from ',final_marginals,' distribution'))
  
  dev.off()
  
}

#### Scatter plots for sampled joint distributions -----------------------------
for (csv in names(simulations)) {
  setwd(paste0(main_path,'/figures/GS_vs_DIS/'))
  
  jpeg(filename = paste0("GS_vs_DIS_",str_split(csv, "_")[[1]][1] ,".jpeg"),
       width = 1200, height = 700)
  
  scatter_plot(data_sim = simulations[[csv]],copula_name = str_split(csv, "_")[[1]][1])
  
  dev.off()
  
}

scatter_plot(data_sim = simulations$dvine_seed_123,copula_name = "d-vine")
scatter_plot(data_real = as.data.frame(pseudo_data), 
             data_sim = as.data.frame(pobs(simulations$dvine_seed_123)),
             copula_name = "d-vine",
             dot_size = 2)