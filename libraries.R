###  Load libraries  ---------------------------------------------------
print("Load R libraries")

lib_nm <- c("tidyquant", "ggplot2", "data.table", "dplyr", "tidyr", "GGally", "gridExtra",
            "xts", "tidyverse", "MASS", "xtable", "copula", "rvinecopulib", "e1071")

sapply(lib_nm, function(x) library(x, character.only = T))

print("Libraries have been loaded")