####  PLOTS FUNCTIONS  -----------------------------------------------------------
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

#### Plot ts (both levels and log-increments)  --------------------------
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

#### Plot correlation matrix -----------------------------------------------------
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

#### Plot histogram ----------------------------------------------------------------
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

#### Plot fitted marginals  --------------------------------------------------------
plot_fitted_densities <- function(data, index_name) {
  
  # Creating vector with x-values for density function scaled for each index
  x_values <- seq(min(data[,index_name]), max(data[,index_name]), 
                  length.out = dim(data)[1])
  
  i <- which(labels_indexes == index_name)
  
  # Creating density values for each fitted marginal distribution
  dt_values <- 1/fitted_marginals$fitted_params$t['s',i] * dt((x_values - fitted_marginals$fitted_params$t['m',i])/fitted_marginals$fitted_params$t['s',i], fitted_marginals$fitted_params$t['df',i])
  
  dlogis_values <- dlogis(x_values, fitted_marginals$fitted_params$logistic['location', i], fitted_marginals$fitted_params$logistic['scale', i])
  
  dnorm_values <- dnorm(x_values, fitted_marginals$fitted_params$normal['mean',i], fitted_marginals$fitted_params$normal['sd',i])
  
  dcauchy_values <- dcauchy(x_values, fitted_marginals$fitted_params$cauchy['location', i], fitted_marginals$fitted_params$cauchy['scale', i])
  
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

#### Scatter plot for real observed data points vs simulated data points -----------
#####  elliptical and archimedean copulas -----------------------------------
scatter_plot <- function(ind1 = 'DIS', 
                         ind2 = 'GS', 
                         data_real = data_num, 
                         data_sim = sampling_from_copula,
                         copula_name = "") {
  
  title_nm <- paste0('Scatter plot ', ind1, '~', ind2)
  subtitle_nm <- paste0('Simulation from ', copula_name, ' copula') 
    
  plt1 <- ggplot(data = data_real, aes(x = get(ind1), y = get(ind2))) + 
    geom_point(size = 6) + 
    my_style + 
    xlab(ind1) + 
    ylab(ind2) +
    ggtitle(title_nm, 
            subtitle = subtitle_nm) + 
    geom_point(data = data_sim, 
               aes(x = get(ind1), y = get(ind2)), 
               colour = '#D73027', 
               size = 6)
  
  return(plt1)
}
# scatter_plot()
#####  vines copulas  -------------------------------------------------------
scatter_plot_vines <- function(ind1 = 'DIS', 
                               ind2 = 'GS', 
                               data_sim = sampling_from_vine_copula) {
  
  plt1 <- ggplot(data = data_log, aes(x = get(ind1), y = get(ind2))) + 
    geom_point(size = 6) + my_style + xlab(indexes_to_plot[1]) + ylab(indexes_to_plot[2]) +
    ggtitle(paste0('Scatter plot ', indexes_to_plot[1], '~', indexes_to_plot[2]), 
            subtitle = paste0('Simulation from decomposition ', vine_name)) + 
    geom_point(data = data_sim, aes(x = get(ind1), y = get(ind2)), 
               colour = '#D73027', size = 6)
  return(plt1)
}
#scatter_plot_vines()