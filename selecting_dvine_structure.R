dvine_name <- c('dvine_object', 'LogLik', 'structure')
family <- c('archimedean', 'elliptical')
n <- 20 # number of simulations

dvine_output <- lapply(dvine_name, 
       function(x) 
         as.list(sapply(paste0('d',seq(1:n)), function(x) NULL))
) %>% 
  setNames(dvine_name)

for (i in 1:n) {
  
  sequence <- c(sample(1:3,3), sample(4:6, 3), sample(7:9, 3))
  model_struc <- dvine_structure(sequence)
  # fitting d-vine stucture
  fit_vine <- vinecop(pseudo_data, structure = model_struc, keep_data = TRUE, family_set = family,
                      selcrit = 'loglik', tree_crit = 'rho', par_method = 'mle')
  # saving results in list
  dvine_output$dvine_object[[i]] <- fit_vine 
  dvine_output$LogLik[[i]] <- fit_vine %>% logLik() %>% head(1) %>% round(2)
  dvine_output$structure[[i]] <- sequence
}
dvine_output %>% print
# saving results as csv file
setwd(main_github_path)
write.csv(dvine_output$structure, "dvine_structure.csv")
write.csv(dvine_output$LogLik, "dvine_loglik.csv")
# select final dvine structure based on loglik value and saving as csv
dvine_final_struc <- dvine_output$structure[[which.max(new)]]
write.csv(dvine_final_struc, 'dvine_final_structure.csv')