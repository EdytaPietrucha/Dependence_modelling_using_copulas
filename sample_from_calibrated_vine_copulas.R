# Prevoiusly we investigated that the best fit to marginals is t-Student distribution, becouse of that
# we will only take into consideration final marginals distr as 't'
set.seed(123) 
model <- 'rvine' # \in c('dvine', 'cvine', 'rvine')

if (model == 'dvine') {
  model_struc <- fits_vines$dvine
  vine_name <- 'D-vine'
} else if (model == 'cvine') {
  model_struc <- fits_vines$cvine
  vine_name <- 'C-vine'
} else if (model == 'rvine') {
  model_struc <- fits_vines$rvine
  vine_name <- 'R-vine'
}
sample_vine <- rvinecop(number_sim, model_struc)
sampling_from_vine_copula <- matrix(nrow = number_sim, ncol = copula_dim)

fitted_t <- fitted_marginals$fitted_params$t

for (i in 1:copula_dim){
  sampling_from_vine_copula[,i] <- qt(sample_vine[,i], df = fitted_t['df',i])*fitted_t['s',i] + fitted_t['m',i]
}

sampling_from_vine_copula <- sampling_from_vine_copula %>% as.data.frame()
colnames(sampling_from_vine_copula) <- labels_indexes
sampling_from_vine_copula %>% head

# plot correlation matrix
plot_corr_matrix(sampling_from_vine_copula, 'spearman') + 
  labs(title = paste0(corr_name,' correlation matrix for random sample from calibrated ', vine_name, ' copula structure'),
       subtitle = paste0('Marginals come from t distribution'))