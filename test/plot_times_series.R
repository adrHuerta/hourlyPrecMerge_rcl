require("xts")
require("lattice")

obs_data <- readRDS("./data/raw/obs/obs_data.rds")


png("./data/output/plots/plot_ts_1.png",width=900,height=700, res = 150)
xyplot(obs_data$value[,1:23],layout = c(4,6),
       par.strip.text = list(cex=0.5, lines=0.9),
       par.settings = list(strip.background=list(col="gray",lwd=5)),
       xlab="Tiempo",ylab="Precipitación (mm/h)")  
dev.off()

png("./data/output/plots/plot_ts_2.png",width=900,height=700, res = 150)
xyplot(obs_data$value[,23:46],layout = c(4,6),
       par.strip.text = list(cex=0.5, lines=0.9),
       par.settings = list(strip.background=list(col="gray",lwd=5)),
       xlab="Tiempo",ylab="Precipitación (mm/h)")  
dev.off()

