# saving generated figures in jpeg format in figures folder
setwd(paste0(main_path,'/figures/'))

jpeg(filename = "indexes_lvls.jpeg",
    width = 1200, height = 700)
indexes_plot(data, 'level')
dev.off()

jpeg(filename = "indexes_increments.jpeg",
     width = 1200, height = 700)
indexes_plot(data_log_date, 'increment')
dev.off()

jpeg(filename = "histogram_indexes.jpeg",
     width = 1200, height = 700)
plot_hist(data_log)
dev.off()

setwd(paste0(main_path,'/figures/correlation/'))
jpeg(filename = "pearson_matrix.jpeg",
     width = 1200, height = 700)
plot_corr_matrix(data_log, 'pearson')
dev.off()

jpeg(filename = "spearman_matrix.jpeg",
     width = 1200, height = 700)
plot_corr_matrix(data_log, 'spearman')
dev.off()

jpeg(filename = "kendall_matrix.jpeg",
     width = 1200, height = 700)
plot_corr_matrix(data_log, 'kendall')
dev.off()