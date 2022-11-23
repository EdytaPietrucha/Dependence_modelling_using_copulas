# fitting elliptical i archimedean copulas
# Transform the marginals to the unit interval
pseudo_data <- data_log %>% pobs
index_num <- pseudo_data %>% dim %>% tail(1)
# The copulas described above
copulas = list(normal = copula::normalCopula(dim = index_num, dispstr = "un"),
               t = copula::tCopula(dim = index_num, dispstr = "un"),
               frank = copula::frankCopula(dim = index_num),
               clayton = copula::claytonCopula(dim = index_num),
               gumbel = copula::gumbelCopula(dim = index_num))
# 'mpl' - Maximum pseudo-likelihood estimator
fits_ellip_arch = sapply(copulas,
                         function(x) copula::fitCopula(x, data = pseudo_data, method = "mpl"))

measures_copulas <- list('AIC' = sapply(fits_ellip_arch, AIC),
                         'BIC' = sapply(fits_ellip_arch, BIC),
                         'Log-likelihood' = sapply(fits_ellip_arch, logLik)
)
measures_copulas %>% print()
fits_ellip_arch %>% print()