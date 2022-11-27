# In this part we will estimate copulas parameters and sample from calibrated copulas
# Sampling will be sensitive on seed, so we define default seed as 123
set.seed(123)

# As final marginal distributions the choice is a t-student distributions to all
# marginals as is gives us the best AIC, BIC, LogLik estimation
final_marginals <- 't' # in c('norm', 't', 'cauchy', 'logis')
source('marginals_fitted_params_list.R')
params_list_after_fitting %>% print

# Define correlation method used to copula calibration
cor_method <- 'spearman' # \in c("pearson", "kendall", "spearman")

if (cor_method == 'pearson') {
  corelation_matrix <- pearson_log
} else if (cor_method == 'kendall') {
  corelation_matrix <- kendall_log
} else if (cor_method == 'spearman') {
  corelation_matrix <- spearman_log
}

# Define copula to calibrate
copula_to_fit <- 't' # \in c('normal', 't', 'frank' , 'clayton', 'gumbel')
copula_params_fit <- fits_ellip_arch[[copula_to_fit]]
fitted_coef <- copula_params_fit %>% coef
fitted_coef %>% print()

if (copula_to_fit == 'normal') {
  copula_object_after_fitting <- normalCopula(param = fitted_coef, 
                                              dim = indexes_num, 
                                              dispstr="un")
  copula_name <- 'Gaussian'
} else if (copula_to_fit == 't') {
  copula_object_after_fitting <- tCopula(param = fitted_coef %>% head(-1),
                                         df = fitted_coef %>% tail(1),
                                         dim = indexes_num, 
                                         dispstr="un")
  copula_name <- 't-Student'
} else if(copula_to_fit == 'frank') {
  copula_object_after_fitting <- frankCopula(param = fitted_coef, 
                                             dim = indexes_num)
  copula_name <- 'Frank'
}else if(copula_to_fit == 'clayton') {
  copula_object_after_fitting <- claytonCopula(param = fitted_coef, 
                                             dim = indexes_num)
  copula_name <- 'Clayton'
}else if(copula_to_fit == 'gumbel') {
  copula_object_after_fitting <- gumbelCopula(param = fitted_coef, 
                                             dim = indexes_num)
  copula_name <- 'Gumbel'
}

# creating object multivariate distribution via copula and parametric margins
copula_fit <- mvdc(copula=copula_object_after_fitting, 
                   margins=rep(final_marginals,9),
                   paramMargins=params_list_after_fitting )

# Now, we can random sample from calibrated copulas functions
# define number of simulations
number_sim <- data_log %>% dim %>% head(1)

sampling_from_copula <- rMvdc(number_sim, copula_fit)
# Generate a random variate:	rt_ls(df, mu, sigma) =	rt(df)*sigma + mu
fit_params <- fitted_marginals$fitted_params[[final_marginals]]
location <- fit_params[if_else((fit_params %>% rownames() == 'm') %>% sum == 1, 'm', 'location'),]
scale <- fit_params[grepl('^s+', fit_params %>% row.names()) %>% which(),]
  
for (i in 1:copula_dim){
  sampling_from_copula[,i] <- sampling_from_copula[,i]*scale[i] + location[i]
}

sampling_from_copula <- sampling_from_copula %>% as.data.frame()
colnames(sampling_from_copula) <- labels_indexes
sampling_from_copula %>% head

# plot correlation matrix
plot_corr_matrix(sampling_from_copula, 'spearman') + 
  labs(title = paste0(corr_name,' correlation matrix for random sample from calibrated ', copula_name, ' copula'),
       subtitle = paste0('Marginals come from ',final_marginals,' distribution'))