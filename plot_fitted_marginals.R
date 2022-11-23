# This part is to plot fitted density to indexes
# Let's create a function which return plot of fitted distributions
plot_fitted_densities <- function(index_name) {
  # Creating vector with x-values for density function scaled for each index
  x_values <- seq(data_log[,index_name] %>% min(), data_log[,index_name] %>% max(), 
                  length.out = dim(data_log)[1])
  i <- which(labels_indexes == index_name)
  # creating density values for each fitted marigal distribution
  dt_values <- 1/fitted_marginals$fitted_params$t['s',i] * dt((x_values - fitted_marginals$fitted_params$t['m',i])/fitted_marginals$fitted_params$t['s',i], fitted_marginals$fitted_params$t['df',i])
  dlogis_values <- dlogis(x_values, fitted_marginals$fitted_params$logistic['location', i], fitted_marginals$fitted_params$logistic['scale', i])
  dnorm_values <- dnorm(x_values, fitted_marginals$fitted_params$normal['mean',i], fitted_marginals$fitted_params$normal['sd',i])
  dcauchy_values <- dcauchy(x_values, fitted_marginals$fitted_params$cauchy['location', i], fitted_marginals$fitted_params$cauchy['scale', i])
  # Creating fitted density plots
  plt <- ggplot(data_log, aes(x = get(index_name))) + geom_histogram(aes(y = ..density..), bins = 60) + 
    geom_line(aes(x = x_values, y = dlogis_values, colour = 'logistic'), size = 1.5) + 
    geom_line(aes(x = x_values, y = dnorm_values, colour = 'normal'), size = 1.5) + 
    geom_line(aes(x = x_values, y = dcauchy_values, colour = 'Cauchy'), size = 1.5) + my_style +
    geom_line(aes(x = x_values, y = dt_values, colour = 't-Student'), size = 1.5) + 
    theme(axis.text=element_text(size=24), 
          axis.title.x = element_text(size = 34),
          panel.background = element_rect(fill = "grey90"),
          legend.background = element_rect(fill = "grey90")) +
    ylab('') + xlab(labels_indexes[i]) + 
    scale_colour_manual("Distribution", 
                        values = c("t-Student"=my_palette[7], "logistic"=my_palette[9], 
                                   "normal" = my_palette[2], "Cauchy"=my_palette[4])) +
    theme(legend.position = c(0, 1),
          legend.justification = c(-0.05, 1),
          legend.text = element_text(size=16))
  return(plt)
}

require(gridExtra)
grid.arrange(plot_fitted_densities('AAPL'), 
             plot_fitted_densities('MSFT'), 
             plot_fitted_densities('NVDA'), ncol=3)
grid.arrange(plot_fitted_densities('C'), 
             plot_fitted_densities('GS'), 
             plot_fitted_densities('JPM'), ncol=3)
grid.arrange(plot_fitted_densities('DIS'), 
             plot_fitted_densities('NFLX'), 
             plot_fitted_densities('TWTR'), ncol=3)