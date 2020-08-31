
data_obs <- readRDS("./data/output/example_qc.rds")
basins <- raster::shapefile("./data/shapes/CHIRILU.shp")

area_study <- list("sp.polygons", basins,  col="grey10", fill="grey90")

example <- data_obs$no_qc
example$QC=data_obs$qc$OBS

#2018-02-15 16:00:00"
png("./data/output/plots/plot_example_qc.png",width=900,height=650, res = 150)
       sp::spplot(example, col.regions=rainbow(5),c("OBS", "QC"),
       sp.layout=list(area_study), 
       scales = list(draw = TRUE),
       xlim=c(-77.4,-76.0),ylim=c(-12.32,-11.0))
dev.off()

