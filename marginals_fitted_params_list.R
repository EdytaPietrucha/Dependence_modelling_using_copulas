# This part is to create list of fitted marginal parameters. It is preparation before copula calibration
params_list_after_fitting <- sapply(labels_indexes, function(x) NULL)

if (final_marginals == 't') {
  for (i in 1:indn) {
    params_list_after_fitting[[i]] <- list(df = marginals_fitted$params$t['df',][i])
  }
} else if (final_marginals == 'norm') {
  for (i in 1:indn) {
    params_list_after_fitting[[i]] <- list(mean = marginals_fitted$params$normal['mean',][i], 
                                           sd = marginals_fitted$params$normal['sd',][i])
  }
} else if (final_marginals == 'cauchy') {
  for (i in 1:indn) {
    params_list_after_fitting[[i]] <- list(location = marginals_fitted$params$cauchy['location',][i], 
                                           scale = marginals_fitted$params$cauchy['scale',][i])
  }
} else if (final_marginals == 'logis') {
  for (i in 1:indn) {
    params_list_after_fitting[[i]] <- list(location = marginals_fitted$params$logistic['location',][i], 
                                           scale = marginals_fitted$params$logistic['scale',][i])
  }
}