# fitting vines copulas
set.seed(127)
# select vine copula parameters used in model calibration 
family <- c('archimedean', 'elliptical')
model_crit <- 'loglik'
# C-vine structure - methodology is based on assumption that main root for each edge
# corresponds to variable which has the highest value of sum from Spearman correlation matrix with all variables
correlation_matrix <- cor_mat$spearman
cvine_struc <- cvine_structure(correlation_matrix %>% colSums() %>% order())
# C-vine structure - methodology is based on the assumption that companies from one sector are the most correlated
# and should be inseparably next to each other - to pick up the best structure of indexes an investiagation has been conducted
#  NOT RUN!
# source('selecting_dvine_structure.R')
dvine_final_struc <- fread('outputs/dvine_final_structure.csv')
setwd(main_github_path)
dvine_final_struc <- dvine_final_struc %>% dplyr::select(x) %>% unlist() %>% as.numeric()
dvine_struc <- dvine_structure(dvine_final_struc)
# structure based  on Dissman's structure selection algorithm (https://cran.r-project.org/web/packages/rvinecopulib/rvinecopulib.pdf page 31)
rvine_struc <- NA

vine_structures <-  list('dvine' = dvine_struc,
                         'cvine' = cvine_struc,
                         'rvine' = rvine_struc
)

#plot(rvine_struc)
fits_vines = lapply(vine_structures,
                    function(x) vinecop(pseudo_data, structure = x, keep_data = TRUE, 
                                        family_set = family, 
                                        selcrit = model_crit, tree_crit = 'rho', par_method = 'mle')
)
fits_vines %>% print()

measures_vine_copulas <- list('AIC' = sapply(fits_vines, AIC),
                              'BIC' = sapply(fits_vines, BIC),
                              'Log-likelihood' = sapply(fits_vines, logLik)
)
measures_vine_copulas %>% print()
# plot first two copula trees
plot(fits_vines$dvine, edge_labels = "family_tau", tree = 1:2)
plot(fits_vines$cvine, edge_labels = "family_tau", tree = 1:2)
plot(fits_vines$rvine, edge_labels = "family_tau", tree = 1:2)