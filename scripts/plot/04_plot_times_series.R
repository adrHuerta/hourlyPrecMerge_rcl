
rm(list = ls())
require("xts")


obs_data <- readRDS("./data/processed/obs/obs_data_qc.rds")
obs_data <- obs_data$value["2014-01/2019-12"]

png("./data/output/plots/plot_ts_1.png",width=900,height=1000, res = 150)

lattice::xyplot(obs_data[,1:22],layout = c(4,6),
                par.strip.text = list(cex=0.7, lines=1.3),
                par.settings = list(strip.background=list(col="gray",lwd=5)),
                xlab="Tiempo",ylab="Precipitación (mm/h)")  
dev.off()

png("./data/output/plots/plot_ts_2.png",width=900,height=1000, res = 150)

lattice::xyplot(obs_data[,23:46],layout = c(4,6),
                par.strip.text = list(cex=0.7, lines=1.3),
                par.settings = list(strip.background=list(col="gray",lwd=5)),
                xlab="Tiempo",ylab="Precipitación (mm/h)")  
dev.off()