####  D-vine structure selection analysis  --------------------------------------
# Number of simulations
ndvine <- 100
struc_nms <- c("Structure", "LogLik", "AIC", "BIC")
dvine_output <- data.frame(matrix(NA, 
                                  nrow = ndvine, 
                                  ncol = length(struc_nms))) %>%
  setNames(struc_nms)

## Simulation part  ------------------------------------------------

for (i in 1:ndvine) {
  
  # Define sector order
  sector_order <- sample(1:3, 3)
  temp <- 3*sector_order
  
  # Sample random sequence of the indexes
  dvine_sequence <- c(sample((temp[1] - 2) : temp[1]), sample((temp[2] - 2) : temp[2]), sample((temp[3] - 2) : temp[3]))
  dvine_struc_rand <- dvine_structure(dvine_sequence)
  
  # Vine fitting
  fit_vine <- vinecop(pseudo_data, 
                      structure = dvine_struc_rand, 
                      keep_data = TRUE, 
                      family_set = family_bicop,
                      selcrit = criterion_vines, 
                      tree_crit = 'rho', 
                      par_method = 'mle')
  
  # output
  dvine_output[i,] <- c("Structure" = paste(as.character(dvine_sequence), collapse = ","),
                        "LogLik" = round(logLik(fit_vine)[1], 2),
                        "AIC" = round(AIC(fit_vine), 2),
                        "BIC" = round(BIC(fit_vine), 2))
  
}

# Saving the result
setwd(paste0(main_path, "/outputs"))
fwrite(dvine_output, "dvine_struc.csv")
setwd(main_path)