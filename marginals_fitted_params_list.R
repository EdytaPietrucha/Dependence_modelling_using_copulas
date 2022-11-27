# This part is to create list of fitted marginal parameters. It is preparation to copula calibration
# params_list_after_fitting - a list whose each component is a list 
params_list_after_fitting <- sapply(labels_indexes, function(x) NULL) %>% as.list

if (final_marginals == 't') {
  for (i in 1:index_num) {
    params_list_after_fitting[[i]] <- list(df = fitted_marginals$fitted_params$t['df',][i])
  }
} else if (final_marginals == 'norm') {
  for (i in 1:index_num) {
    params_list_after_fitting[[i]] <- list(mean = fitted_marginals$fitted_params$normal['mean',][i], 
                                           sd = fitted_marginals$fitted_params$normal['sd',][i])
  }
} else if (final_marginals == 'cauchy') {
  for (i in 1:index_num) {
    params_list_after_fitting[[i]] <- list(location = fitted_marginals$fitted_params$cauchy['location',][i], 
                                           scale = fitted_marginals$fitted_params$cauchy['scale',][i])
  }
} else if (final_marginals == 'logis') {
  for (i in 1:index_num) {
    params_list_after_fitting[[i]] <- list(location = fitted_marginals$fitted_params$logistic['location',][i], 
                                           scale = fitted_marginals$fitted_params$logistic['scale',][i])
  }
}
params_list_after_fitting %>% print