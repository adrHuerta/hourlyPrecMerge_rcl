require("xts")
require("lattice")

obs_data <- readRDS("./data/processed/obs/obs_data_qc.rds")
datafinal=xts(datasola3[1:52583,c(-1)],order.by = indexx[1:52583])

png("./data/output/plots/plot_ts_1.png",width=900,height=1000, res = 150)
xyplot(datafinal[,1:22],layout = c(4,6),
       par.strip.text = list(cex=0.7, lines=1.3),
       par.settings = list(strip.background=list(col="gray",lwd=5)),
       xlab="Tiempo",ylab="Precipitación (mm/h)")  
dev.off()

png("./data/output/plots/plot_ts_2.png",width=900,height=1000, res = 150)
xyplot(datafinal[,23:46],layout = c(4,6),
       par.strip.text = list(cex=0.7, lines=1.3),
       par.settings = list(strip.background=list(col="gray",lwd=5)),
       xlab="Tiempo",ylab="Precipitación (mm/h)")  
dev.off()
# min(obs_data$value$ID47A0894A, na.rm = T)
# which(obs_data$value$ID47A0894A==-11234.8)
# which(obs_data$value$ID47A0894A<0)
# replace(obs_data$value$ID47A0894A,obs_data$value$ID47A0894A=="-11234.8","NA")
