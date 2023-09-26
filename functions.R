print("Load necessary functions")
################################################################################
####  COPULAS CALIBARTION  -----------------------------------------------------
################################################################################
# Simulations from vine copulas  -----------------------------------------------
simulate_from_vine_copula <- function(marginals = "t",
                                      copula_nm = "dvine",
                                      seed_fixed = 123) {
  
  params_list_after_fitting <- marginal_parameter_preprocess(final_marginals = marginals)
  fit_params <- marginals_fitted$params[[marginals]]
  location <- fit_params[if_else((fit_params %>% rownames() == 'm') %>% sum == 1, 'm', 'location'),]
  scale <- fit_params[grepl('^s+', fit_params %>% row.names()) %>% which(),]
  
  if (copula_nm == 'dvine') {
    model_struc <- vines$dvine
    vine_name <- 'D-vine'
  } else if (copula_nm == 'cvine') {
    model_struc <- vines$cvine
    vine_name <- 'C-vine'
  } else if (copula_nm == 'rvine') {
    model_struc <- vines$rvine
    vine_name <- 'R-vine'
  }
  
  # random generation for the vine copula distribution
  set.seed(seed_fixed)
  sample_vine <- rvinecop(simn, model_struc)
  sampling_from_vine_copula <- matrix(nrow = simn, 
                                      ncol = indn)
  
  for (i in 1:indn){
    sampling_from_vine_copula[,i] <- qt(sample_vine[,i], df = fit_params['df',i])*scale[i] + location[i]
  }
  
  sampling_from_vine_copula <- as.data.frame(sampling_from_vine_copula)
  colnames(sampling_from_vine_copula) <- labels_indexes
  
  return(sampling_from_vine_copula)
  
}

# Simulations from archimedean eliptical copulas  ------------------------------
simulate_from_copula <- function(
                                 marginals = "t", # in c('normal', 't', 'cauchy', 'logis')
                                 cor_method = 'spearman', # correlation method used to copula calibration
                                 copula_nm = "t",
                                 seed_fixed = 123) {

  params_list_after_fitting <- marginal_parameter_preprocess(final_marginals = marginals)
  fit_params <- marginals_fitted$params[[marginals]]
  location <- fit_params[if_else((fit_params %>% rownames() == 'm') %>% sum == 1, 'm', 'location'),]
  scale <- fit_params[grepl('^s+', fit_params %>% row.names()) %>% which(),]
  
  corelation_matrix <- cor_mat[[cor_method]]
  
  # Define number of simulations
  simn <- dim(data_log)[1]

    copula_to_fit <- copula_nm 
    copula_params_fit <- ellip_arch[[copula_to_fit]]
    fitted_coef <- coef(copula_params_fit)
    
    if (copula_to_fit == 'normal') {
      copula_object_after_fitting <- normalCopula(param = fitted_coef,
                                                  dim = indn, 
                                                  dispstr="un")
      copula_name <- 'Gaussian'
    } else if (copula_to_fit == 't') {
      copula_object_after_fitting <- tCopula(param = fitted_coef[names(fitted_coef) != "df"],
                                             df = fitted_coef[names(fitted_coef) == "df"],
                                             dim = indn, 
                                             dispstr="un",
                                             df.fixed = T)
      copula_name <- 't-Student'
    } else if(copula_to_fit == 'frank') {
      copula_object_after_fitting <- frankCopula(param = fitted_coef, 
                                                 dim = indn)
      copula_name <- 'Frank'
    }else if(copula_to_fit == 'clayton') {
      copula_object_after_fitting <- claytonCopula(param = fitted_coef, 
                                                   dim = indn)
      copula_name <- 'Clayton'
    }else if(copula_to_fit == 'gumbel') {
      copula_object_after_fitting <- gumbelCopula(param = fitted_coef, 
                                                  dim = indn)
      copula_name <- 'Gumbel'
    }
    
    # creating object multivariate distribution via copula and parametric margins
    copula_fit <- mvdc(copula=copula_object_after_fitting, 
                       margins=rep(marginals,9),
                       paramMargins=params_list_after_fitting )
    
    # Now, we can random sample from calibrated copulas functions
    set.seed(seed_fixed)
    sampling_from_copula <- rMvdc(simn, copula_fit)
    # Generate a random variate:	rt_ls(df, mu, sigma) =	rt(df)*sigma + mu
    
    for (i in 1:indn){
      sampling_from_copula[,i] <- sampling_from_copula[,i]*scale[i] + location[i]
    }
    
    sampling_from_copula <- as.data.frame(sampling_from_copula)
    colnames(sampling_from_copula) <- labels_indexes
    
    return(sampling_from_copula)
    # # Saving results
    # setwd(paste0(main_path,'/outputs/simulations/'))
    # write.csv(sampling_from_copula, 
    #           paste0(copula_name, "_seed_",seed_fixed,".csv"))
}


################################################################################
####  MARGINALS FITTING  -------------------------------------------------------
################################################################################

#### Fitting marginals from c('normal', 'cauchy', 'logistic', 't') -------------
marginals_fit <- function(data, 
                          marginals = c('normal', 't', 'cauchy', 'logistic')) {
  
  marginal_distr_nm <- marginals
  metric <- c('loglik', 'aic', 'bic', 'fitted_params')
  
  # Creating empty list used to filled by fitted marginal distributions
  fitted_marginals <- lapply(metric, 
                             function(x) 
                               as.list(sapply(marginal_distr_nm, function(x) NULL))
  ) %>% 
    setNames(metric)
  marginal_distr_after_fitting <- list()
  
  # Marginal distribution calibration
  for (j in 1:length(marginal_distr_nm)) {
    
    distribution <- marginal_distr_nm[j]
    
    for (ind_nm in labels_indexes) {
      marginal_distr_after_fitting[[ind_nm]] <- fitdistr(data_log[[ind_nm]],distribution)
    }
    
    fitted_marginals$fitted_params[[distribution]] <- 
      sapply(marginal_distr_after_fitting, function(x) x$estimate)
    fitted_marginals$aic[[distribution]] <- 
      sapply(marginal_distr_after_fitting, AIC)
    fitted_marginals$bic[[distribution]] <- 
      sapply(marginal_distr_after_fitting, BIC)
    fitted_marginals$loglik[[distribution]] <- 
      sapply(marginal_distr_after_fitting, logLik)
  }
  # Results
  criterion_names <- metric[metric != "fitted_params"]
  criterion_summary <-  fitted_marginals[criterion_names] %>% 
    lapply(function(x) as.data.frame(x, row.names = labels_indexes))
  
  return(list("params" = fitted_marginals$fitted_params,
              "metrics" = criterion_summary))
  
}

#### Marginals paramters preprocessing  ----------------------------------------
# This part is to create list of fitted marginal parameters. It is preparation before copula calibration
marginal_parameter_preprocess <- function(final_marginals = "t") {
  
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
  
  return(params_list_after_fitting)
}

################################################################################
####  PLOTS FUNCTIONS  ---------------------------------------------------------
################################################################################

# define my palette colors
my_palette <- c("#D73027", "#FDAE61", "#FFFFBF", 
                "#A50026", '#ABD9E9', '#4575B4', 
                '#313695', 'black', "lawngreen")

# customize plot text size
axis_size <- 12
basic_size <- 24

# customize my_style settings
my_style <-  theme_classic(
  base_size = basic_size - 6
  ) + 
  theme(
    legend.position = 'none'
    ) + 
  theme(
    plot.subtitle = element_text(size = basic_size - 10, colour='black')
    ) + 
  theme(
    plot.title = element_text(size = basic_size, face = 'bold')
    ) + 
  theme(
    axis.text=element_text(size=axis_size), 
    axis.title.y = element_text(size = axis_size), 
    axis.title.x = element_text(size = axis_size)
    ) +
  theme(
    legend.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color=NA),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white")
    ) 

#### Plot ts (both levels and log-increments)  ---------------------------------
# please, customize your time series plot type: type \in c('level', 'increment')
indexes_plot <- function(dataset, type) { 
  
  stopifnot("`dataset` must be a tibble." = is_tibble(dataset));
  stopifnot("`type` must be 'level' or 'increment'." = 
              if_else(type == 'level' || type == 'increment', TRUE, FALSE))
  
  if (type == 'level') {
    xts_title <- 'Levels';
    legend_position = 'topleft';
  }
  else if (type == 'increment') {
    xts_title <- 'Logarithmic increments';
    legend_position = 'bottomleft';
  }
  
  data_xts <- xts(dataset %>% dplyr::select(-date), 
                  dataset$date)
  
  plt <- plot(data_xts, 
              main = xts_title, lwd = 3, col = my_palette, cex=1.3)
  
  addLegend(
    legend_position, on=1, legend.names = colnames(data_xts), col = my_palette,
    lty=rep(1,9),
    lwd=rep(2,9),
    ncol=3,
    bg="white",
    cex = 0.55, text.font = 2
    )
  
  return(plt)
}

#### Plot correlation matrix ---------------------------------------------------
# we will be using 3 different correlation measures when modelling a dependence structure of data
# cor_method \in c("pearson", "kendall", "spearman")
plot_corr_matrix <- function(data, cor_method) {
  
  stopifnot("`cor_method` must be 'pearson', 'kendall' or 'spearman'." = 
              if_else(cor_method == 'pearson' || cor_method == 'kendall' || cor_method == 'spearman', 
                      TRUE, FALSE))
  
  # creating title name for plots
  title_name <- paste0(str_to_title(cor_method), ' correlation matrix')
  
  # making plot
  plt <- ggpairs(
    data,
    upper = list(continuous = wrap('cor', method = cor_method, size = 6, col = 'black')),
    lower = list(combo = wrap("points", bins = 30, size = 7)),
    diag = list(continuous = wrap("densityDiag", alpha = 0.5)),
    title = title_name
    ) + 
    my_style
  
  return(plt)
}

#### Plot histogram ------------------------------------------------------------
plot_hist <- function(data) {
  
  title_name <- 'Histogram'
  
  my_style_hist <- my_style + 
    theme(
      axis.title.x = element_blank(), 
      axis.title.y = element_blank()
      )
  
  plt <- ggplot(gather(data_log), aes(value)) + 
    geom_histogram(aes(y=..density..), bins = 50) + 
    facet_wrap(~key, scales = 'free_x') + my_style_hist + 
    labs(title = title_name) 
  
  return(plt)
}

#### Plot fitted marginals  ----------------------------------------------------
plot_fitted_densities <- function(data, index_name) {
  
  # Creating vector with x-values for density function scaled for each index
  x_values <- seq(min(data[,index_name]), max(data[,index_name]), 
                  length.out = dim(data)[1])
  
  i <- which(labels_indexes == index_name)
  
  # Creating density values for each fitted marginal distribution
  dt_values <- 1/marginals_fitted$params$t['s',i] * dt((x_values - marginals_fitted$params$t['m',i])/marginals_fitted$params$t['s',i], marginals_fitted$params$t['df',i])
  
  dlogis_values <- dlogis(x_values, marginals_fitted$params$logistic['location', i], marginals_fitted$params$logistic['scale', i])
  
  dnorm_values <- dnorm(x_values, marginals_fitted$params$normal['mean',i], marginals_fitted$params$normal['sd',i])
  
  dcauchy_values <- dcauchy(x_values, marginals_fitted$params$cauchy['location', i], marginals_fitted$params$cauchy['scale', i])
  
  # Creating fitted density plots
  plt <- ggplot(data, aes(x = get(index_name))) + 
    geom_histogram(aes(y = ..density..), bins = 60) + 
    geom_line(aes(x = x_values, y = dlogis_values, colour = 'logistic'), size = 1.5) + 
    geom_line(aes(x = x_values, y = dnorm_values, colour = 'normal'), size = 1.5) + 
    geom_line(aes(x = x_values, y = dcauchy_values, colour = 'Cauchy'), size = 1.5) + 
    geom_line(aes(x = x_values, y = dt_values, colour = 't-Student'), size = 1.5) + 
    my_style +
    theme(axis.text=element_text(size=24), 
          axis.title.x = element_text(size = 34),
          panel.background = element_rect(fill = "grey90"),
          legend.background = element_rect(fill = "grey90")) +
    ylab('') + 
    xlab(labels_indexes[i]) + 
    scale_colour_manual("Distribution", 
                        values = c("t-Student"=my_palette[7], "logistic"=my_palette[9], 
                                   "normal" = my_palette[2], "Cauchy"=my_palette[4])) +
    theme(legend.position = c(0, 1),
          legend.justification = c(-0.05, 1),
          legend.text = element_text(size=16))
  return(plt)
}

#### Scatter plot for real observed data points vs simulated data points -------
scatter_plot <- function(ind1 = 'DIS', 
                         ind2 = 'GS', 
                         data_real = data_log, 
                         data_sim,
                         copula_name = "",
                         dot_size = 6) {
  
  title_nm <- paste0('Scatter plot ', ind2, '~', ind1)
  subtitle_nm <- paste0('Simulation from ', copula_name, ' copula') 
    
  plt1 <- ggplot(data = data_real, aes(x = get(ind1), y = get(ind2))) + 
    geom_point(size = dot_size) + 
    my_style + 
    xlab(ind1) + 
    ylab(ind2) +
    ggtitle(title_nm, 
            subtitle = subtitle_nm) + 
    geom_point(data = data_sim, 
               aes(x = get(ind1), y = get(ind2)), 
               colour = '#D73027', 
               size = dot_size)
  
  return(plt1)
}
print("Functions have been loaded")