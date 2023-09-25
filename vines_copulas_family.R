# searching bicopulas in different sets of copulas families
family_cop <- list(all = 'all',
                   parametrics_except_tll = 'parametric',
                   nonparametrics_indep_tll = 'nonparametric',
                   oneparam = 'onepar', # ("gaussian", "clayton", "gumbel", "frank", and "joe"),
                   twoparam = 'twopar', # ("t", "bb1", "bb6", "bb7", and "bb8"),)
                   ellipticals = 'elliptical',
                   arch = 'archimedean',
                   kendalls_inversion = 'itau', # ("indep","gaussian", "t","clayton", "gumbel", "frank", "joe")
                   kernel_transform = 'tll',
                   epllip_archimed = c('elliptical', 'archimedean')
)

fits_cvine <- lapply(family_cop,
                    function(x) vinecop(pseudo_data, 
                                        structure = cvine_struc, 
                                        keep_data = TRUE, 
                                        family_set = x, 
                                        selcrit = 'loglik', 
                                        tree_crit = 'rho',
                                        par_method = 'mle'))
fits_cvine %>% 
  print()
criterion_family_cvines <- list('AIC' = sapply(fits_cvine, AIC),
                                'BIC' = sapply(fits_cvine, BIC),
                                'Log-likelihood' = sapply(fits_cvine, logLik)
                                )

fits_dvine <- lapply(family_cop,
                    function(x) vinecop(pseudo_data, 
                                        structure = dvine_struc, 
                                        keep_data = TRUE, 
                                        family_set = x, 
                                        selcrit = 'loglik', 
                                        tree_crit = 'rho',
                                        par_method = 'mle'))
fits_dvine %>% 
  print()
criterion_family_dvines <- list('AIC' = sapply(fits_dvine, AIC),
                                'BIC' = sapply(fits_dvine, BIC),
                                'Log-likelihood' = sapply(fits_dvine, logLik)
)

fits_rvine <- lapply(family_cop,
                    function(x) vinecop(pseudo_data, 
                                        structure = rvine_struc, 
                                        keep_data = TRUE, 
                                        family_set = x, 
                                        selcrit = 'loglik', 
                                        tree_crit = 'rho',
                                        par_method = 'mle'))
fits_rvine %>% 
  print()
criterion_family_rvines <- list('AIC' = sapply(fits_rvine, AIC),
                                'BIC' = sapply(fits_rvine, BIC),
                                'Log-likelihood' = sapply(fits_rvine, logLik)
)

#saving results as csv file
setwd(paste0(main_path, '/outputs'))

for (j in c("AIC", "BIC", "Log-likelihood")) {
  
  results <-
    rownames_to_column(as.data.frame(criterion_family_cvines[[j]]), "family_bicop") %>% 
    left_join(tibble::rownames_to_column(as.data.frame(criterion_family_dvines[[j]]), "family_bicop")) %>% 
    left_join(tibble::rownames_to_column(as.data.frame(criterion_family_rvines[[j]]), "family_bicop")) %>%
    setNames(c("family", "cvine", "dvine". "rvine"))
  
  write_csv(results, paste0(j, "_for_varius_bicopulas_families.csv"))
}
setwd(main_path)