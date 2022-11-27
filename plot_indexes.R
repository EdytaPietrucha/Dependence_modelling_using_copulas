####  STYLE OF PLOTS ####
# define my palette colors
my_palette <- c("#D73027", "#FDAE61", "#FFFFBF", 
                "#A50026", '#ABD9E9', '#4575B4', 
                '#313695', 'black', "lawngreen")

# customize plot text size
axis_size <- 12
basic_size <- 24

my_style <-  theme_classic(base_size = basic_size - 6) + theme(legend.position = 'none') + 
  theme(plot.subtitle = element_text(size = basic_size - 10, colour='black')) + 
  theme(plot.title = element_text(size = basic_size, face = 'bold')) + 
  theme(axis.text=element_text(size=axis_size), 
        axis.title.y = element_text(size = axis_size), 
        axis.title.x = element_text(size = axis_size)) +
  theme(
    legend.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color=NA),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white")) 

#### PLOT TIME SERIES ####
# please, customize your time series plot type: type \in c('level', 'increment')
indexes_plot <- function(dataset, type) { 
  
  stopifnot("`dataset` must be a tibble." = is_tibble(dataset));
  stopifnot("`type` must be 'level' or 'increment'." = 
              if_else(type == 'level' || type == 'increment', TRUE, FALSE))
  
  if (type == 'level') {
    xts_title <- 'Plot of indexes levels';
    legend_position = 'topleft';
  }
  else if (type == 'increment') {
    xts_title <- 'Plot of logarithmic increments';
    legend_position = 'bottomleft';
  }
  
  data_xts <- xts(dataset %>% dplyr::select(-date), dataset$date)
  
  plot(data_xts, main = xts_title, lwd = 3, col = my_palette, cex=1.3)
  addLegend(legend_position, on=1, legend.names = colnames(data_xts), col = my_palette,
            lty=rep(1,9),
            lwd=rep(2,9),
            ncol=3,
            bg="white",
            cex = 0.55, text.font = 2)
}

#### PLOT CORRELATION MATRIX ####
# we will be using 3 different correlation measures when modelling a dependence structure of data
# cor_method \in c("pearson", "kendall", "spearman")
plot_corr_matrix <- function(data, cor_method){
  
  stopifnot("`cor_method` must be 'pearson', 'kendall' or 'spearman'." = 
              if_else(cor_method == 'pearson' || cor_method == 'kendall' || cor_method == 'spearman', 
                      TRUE, FALSE))
  
  # creating title name for plots
  title_name <- paste0(str_to_title(cor_method), ' correlation matrix')
  # making plot
  plt <- ggpairs(data,
          upper = list(continuous = wrap('cor', method = cor_method, size = 6, col = 'black')),
          lower = list(combo = wrap("points", bins = 30, size = 7)),
          diag = list(continuous = wrap("densityDiag", alpha = 0.5)),
          title = title_name) + my_style
  return(plt)
}

#### PLOT HISTOGRAM ####
plot_hist <- function(data) {
  
  title_name <- 'Histogram of indexes'
  
  my_style_hist <- my_style + theme(axis.title.x = element_blank(), 
                                    axis.title.y = element_blank())
  
  plt <- ggplot(gather(data_log), aes(value)) + 
    geom_histogram(aes(y=..density..), bins = 50) + 
    facet_wrap(~key, scales = 'free_x') + my_style_hist + 
    labs(title = title_name) 
  return(plt)
}