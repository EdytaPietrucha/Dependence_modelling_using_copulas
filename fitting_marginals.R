# fitting marginal distributions to financial indexes when considering below distribution functions:
# normal = 'normal', cauchy = 'cauchy', logistic = 'logistic', tstudent = 't'
distr_after_fitting <- list()
indexes_num <- length(data_log)
fitted_descrip <- c('loglik', 'aic', 'bic', 'fitted_params')
marginals_distr_to_fit <- c('normal', 't', 'cauchy', 'logistic') 
distr_num <- length(marginals_distr_to_fit)
# creating empty list used to filled by fitted marginal distributions
fitted_marginals <- lapply(fitted_descrip, 
                           function(x) 
                             as.list(sapply(marginals_distr_to_fit, function(x) NULL))
                           ) %>% 
  setNames(fitted_descrip)
# marginal distribution calibration
for (j in 1:distr_num) {
  
  distribution <- marginals_distr_to_fit[j]
  
  for (i in 1:indexes_num) {
    distr_after_fitting[[i]] <- fitdistr(data_log[,i],distribution)
  }
  
  fitted_marginals$fitted_params[[distribution]] <- sapply(distr_after_fitting, function(x) x$estimate)
  fitted_marginals$aic[[distribution]] <- sapply(distr_after_fitting, AIC)
  fitted_marginals$bic[[distribution]] <- sapply(distr_after_fitting, BIC)
  fitted_marginals$loglik[[distribution]] <- sapply(distr_after_fitting, logLik)
}
# results
criterion_names <- c('aic', 'bic', 'loglik')
criterion_summary <-  fitted_marginals[criterion_names] %>% lapply(function(x) as.data.frame(x, row.names = labels_indexes))
criterion_summary %>% print()