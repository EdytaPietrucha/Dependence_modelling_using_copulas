# saving generated figures in jpeg format in figures folder
setwd(paste0(main_path,'/figures/vine_trees/'))

jpeg(filename = "trees_dvine.jpeg",
     width = 1200, height = 700)
plot(vines$dvine, edge_labels = "family_tau", tree = 1:2)
dev.off()

jpeg(filename = "trees_cvine.jpeg",
     width = 1200, height = 700)
plot(vines$cvine, edge_labels = "family_tau", tree = 1:2)
dev.off()

jpeg(filename = "trees_rvine.jpeg",
     width = 1200, height = 700)
plot(vines$rvine, edge_labels = "family_tau", tree = 1:2)
dev.off()

setwd(main_path)