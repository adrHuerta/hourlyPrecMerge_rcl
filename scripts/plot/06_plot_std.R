rm(list = ls())

early_std <- readRDS("./data/output/metrics.rds")
early_std <- early_std$temporal
obs_data <- readRDS("./data/processed/obs/obs_data_qc_v3.rds")

xyz <- as.data.frame(obs_data$xyz)

#add statistics
xyz$CC=early_std$corr

xyz$BIAS=early_std$bias
xyz$BIAS[xyz$BIAS< -100] = -100
xyz$BIAS[xyz$BIAS > 100] = 100

xyz$FAR=early_std$FAR
xyz$POD=early_std$POD

#SPDF
obs_data$xyz = sp::SpatialPointsDataFrame(coords=xyz[,3:4],
                                          data=xyz, proj4string=sp::CRS(" +proj=longlat +datum=WGS84 
                                             +no_defs +ellps=WGS84 +towgs84=0,0,0") )         

#read shapes
mypoints <- obs_data$xyz
basins <- raster::shapefile("./data/shapes/CHIRILU.shp")
r <- raster::raster("./data/shapes/chirilu_ProjectRaster.tif")

#plot
plota <-sp::spplot(mypoints, "CC",xlim=c(-77.4,-76.0),ylim=c(-12.32,-11.0),
                   key.space=list(space = "bottom",cex= 1.0,pch=19,
                                  lwd=8,height = 1.5,width = 1),
                   scales = list(draw=T,
                                 y = list(tck=c(-1, -1)),
                                 x = list(tck=c(-1, -1))), 
                   cuts = 4,pch =19,lwd=8, main= c("a) CC"),
                   col.regions=c('red', "orange1",'green','blue'), 
                   legendEntries = c("0 -0.25", "0.25 0.5", "0.5 0.75","<0.75 ")) + 
  latticeExtra::as.layer(sp::spplot(basins, fill="transparent", col="white"), under =TRUE)+ 
  latticeExtra::as.layer(sp::spplot(r, col.regions=grey.colors(50)) , under = TRUE)


plotb <-sp::spplot(mypoints, "BIAS",xlim=c(-77.4,-76.0),ylim=c(-12.32,-11.0),
                   key.space=list(space = "bottom",cex=1.0,pch=19,
                                  lwd=8,height = 1.5, width = 1), 
                   scales = list(draw=T,
                                 y = list(tck=c(-1, -1)),
                                 x = list(tck=c(-1, -1))), 
                   cuts = 4,pch =19,lwd=8, main= c("b) Bias"),
                   col.regions=c('violetred', "violet",'springgreen','springgreen4'), 
                   legendEntries = c("-0.5 >", "-0.5 - 0", "0 - 0.5","< 0.5 ")) + 
  latticeExtra::as.layer(sp::spplot(basins, fill="transparent",  col="white"), under = TRUE) + 
  latticeExtra::as.layer(sp::spplot(r, col.regions=grey.colors(50)) , under = TRUE)

plotc <-sp::spplot(mypoints, "POD", xlim=c(-77.4,-76.0),ylim=c(-12.32,-11.0),
                   key.space=list(space = "bottom",cex=1.0,pch=19,
                                  lwd=8,height = 1.5, width = 1), 
                   scales=list(draw=T,
                               y = list(tck=c(-1, -1)),
                               x = list(tck=c(-1, -1))), 
                   cuts = 3,pch =19,lwd=8, main= c("a) POD"),
                   col.regions=c('orange1', 'green','blue'), 
                   legendEntries = c("0-0.3", "0.3-0.6", "0.6-1")) + 
  latticeExtra::as.layer(sp::spplot(basins, fill="transparent",  col="white"), under = TRUE) + 
  latticeExtra::as.layer(sp::spplot(r, col.regions=grey.colors(50)) , under = TRUE)

plotd <-sp::spplot(mypoints, "FAR", xlim=c(-77.4,-76.0),ylim=c(-12.32,-11.0),
                   key.space=list(space = "bottom",cex=1.0,pch=19,
                                  lwd=8,height = 1.5, width = 1), 
                   scales=list(draw=T,
                               y = list(tck=c(-1, -1)),
                               x = list(tck=c(-1, -1))), 
                   cuts = 3,pch =19,lwd=8, main= c("b) FAR"),
                   col.regions=c('blue', 'green','orange1'), 
                   legendEntries = c("0-0.3", "0.3-0.6", "0.6-1")) + 
  latticeExtra::as.layer(sp::spplot(basins, fill="transparent",  col="white"), under = TRUE) + 
  latticeExtra::as.layer(sp::spplot(r, col.regions=grey.colors(50)) , under = TRUE)



png("./data/output/plots/plot_std.png",width=850,height=500,res=100)
gridExtra::grid.arrange(plota,plotb, ncol=2,nrow=1)
dev.off()

png("./data/output/plots/plot_std2.png",width=850,height=500,res=100)
gridExtra::grid.arrange(plotc,plotd, ncol=2,nrow=1)
dev.off()



