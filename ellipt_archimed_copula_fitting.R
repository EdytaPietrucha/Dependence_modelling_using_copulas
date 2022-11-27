# fitting elliptical i archimedean copulas
# Transform the marginals to the unit interval
pseudo_data <- data_log %>% pobs
# The copulas described above - object copula
copulas = list(normal = normalCopula(dim = indexes_num , dispstr = "un"),
               t = tCopula(dim = indexes_num , dispstr = "un"),
               frank = frankCopula(dim = indexes_num ),
               clayton = claytonCopula(dim = indexes_num ),
               gumbel = gumbelCopula(dim = indexes_num ))
# 'mpl' - Maximum pseudo-likelihood estimator
fits_ellip_arch = sapply(copulas,
                         function(x) copula::fitCopula(x, data = pseudo_data, method = "mpl"))

measures_copulas <- list('AIC' = sapply(fits_ellip_arch, AIC),
                         'BIC' = sapply(fits_ellip_arch, BIC),
                         'Log-likelihood' = sapply(fits_ellip_arch, logLik)
)
measures_copulas %>% print()
fits_ellip_arch %>% print()