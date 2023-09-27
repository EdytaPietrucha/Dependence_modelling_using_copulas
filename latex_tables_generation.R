#### Criterion scores for marginals  -------------------------------------------
setwd(paste0(main_path, "/outputs/marginals/"))

data_temp <- lapply(list.files(paste0(main_path, "/outputs/marginals/")), function(x) fread(x)) %>%
  setNames(gsub(".csv", "", list.files(paste0(main_path, "/outputs/marginals/"))))

for (crit in names(data_temp)) {

    tbl <- as.tibble(data_temp[[crit]]) 
  
    if (crit == "marginals_aic") {
      crit_nm <- "AIC"
    } else if (crit == "marginals_bic") {
      crit_nm <- "BIC"
    } else if (crit == "marginals_loglik") {
      crit_nm <- "Log-likelihood"
    }
    
    setwd(paste0(main_path, "/outputs/latex_tables/"))
  
    kbl_tab <- kableExtra::kbl(tbl, 
                             col.names = c("Ticker", "Gaussian", "t-Student","Cauchy", "Logistic"),
                             format = 'latex',
                             digits = 2,
                             caption = paste0(crit_nm, ' scores for fitted marginals.'),
                             booktabs = T,
                             linesep = "") %>%
    kable_styling(bootstrap_options = "striped",
                  full_width = F,
                  font_size = 11,
                  latex_options = "HOLD_position")
  
    writeLines(kbl_tab, paste0(crit_nm, "_marginals.tex"))
}

#### Criterion scores for fitted copulas  ------------------
setwd(paste0(main_path, "/outputs/"))

data_temp <- lapply(list.files(paste0(main_path, "/outputs/"), "metrics*"), function(x) fread(x)) %>%
  setNames(gsub(".csv", "", list.files(paste0(main_path, "/outputs/"), "metrics*")))

for (crit in names(data_temp)) {
  
  tbl <- as.tibble(data_temp[[crit]])
  
  if (crit == "metrics_elliptical_archimedean") {
     col_nms <- c("", "Gaussian", "t-Student","Frank", "Clayton", "Gumbel")
     copulas_nm <- "elliptical \\& archimedean"
  } else if (crit == "metrics_vines") {
      col_nms <- c("", "D-vine", "C-vine","R-vine")
      copulas_nm <- "C-vine, D-vine \\& R-vine"
  } 
  
  setwd(paste0(main_path, "/outputs/latex_tables/"))
  kbl_tab <- kableExtra::kbl(tbl, 
                             col.names = col_nms,
                             format = 'latex',
                             digits = 2,
                             caption = paste0('Criterion scores for fitted ', copulas_nm, " copulas."),
                             booktabs = T,
                             linesep = "") %>%
    kable_styling(bootstrap_options = "striped",
                  full_width = F,
                  font_size = 11,
                  latex_options = "HOLD_position")
  
  writeLines(kbl_tab, paste0(crit, ".tex"))
}

#### D-vine structure selection  -----------------------------------------------

setwd(paste0(main_path, "/outputs/"))

data_temp <- fread(paste0(main_path, "/outputs/dvine_struc.csv"))
dvine_best <- data_temp  %>% 
  filter(LogLik == max(LogLik))
data_temp <- data_temp[sample(1:100, 9),] %>%
  rbind(dvine_best)

tbl <- as.tibble(data_temp) %>%
  dplyr::select(c("Structure", "LogLik")) 

setwd(paste0(main_path, "/outputs/latex_tables/"))
kbl_tab <- kableExtra::kbl(tbl, 
                           col.names = c("Structure", "Log-likelihood"),
                           format = 'latex',
                           digits = 2,
                           caption = 'Criterion scores for different D-vine structures.',
                           booktabs = T,
                           linesep = "") %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 11,
                latex_options = "HOLD_position")

writeLines(kbl_tab, "dvine_struc.tex")
#### Fitting different bicopula families  --------------------------------------

setwd(paste0(main_path, "/outputs/"))

data_temp <- lapply(list.files(paste0(main_path, "/outputs/"), "*families.csv"), function(x) fread(x)) %>%
  setNames(gsub(".csv", "", list.files(paste0(main_path, "/outputs/"), "*families.csv")))

for (crit in names(data_temp)) {
  
  tbl <- as.tibble(data_temp[[crit]])
  setwd(paste0(main_path, "/outputs/latex_tables/"))

  kbl_tab <- kableExtra::kbl(tbl, 
                             col.names = c("Bicopula family", "C-vine", "D-vine","R-vine"),
                             format = 'latex',
                             digits = 2,
                             caption = paste0(str_split(crit,"_")[[1]][1], ' scores for fitted vine copulas while variour bicopula families are in usage.'),
                             booktabs = T,
                             linesep = "") %>%
    kable_styling(bootstrap_options = "striped",
                  full_width = F,
                  font_size = 11,
                  latex_options = "HOLD_position")
  
  writeLines(kbl_tab, paste0(crit, ".tex"))
}