
early_std <-readRDS("./data/output/early_std.rds")
obs_data <-readRDS("./data/processed/obs/obs_data_qc_v2.rds")

xyz =as.data.frame(obs_data$xyz)

#add statistics
xyz$CC=early_std$CC
xyz$BIAS=early_std$Bias

# remove ID's Out of limit
xyz=xyz[-c(6,40,41),]

#SPDF
obs_data$xyz = SpatialPointsDataFrame(coords=xyz[,3:4],
               data=xyz, proj4string=CRS(" +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") )         

writeOGR(obs_data$xyz, dsn='./data/shapes/',layer="mypoints",driver="ESRI Shapefile")

#reed shapes
mypoints <- raster::shapefile("./data/shapes/mypoints.shp")
basins <- raster::shapefile("./data/shapes/CHIRILU.shp")
r <- raster::raster("./data/shapes/chirilu_ProjectRaster.tif")

#plot
plota <-spplot(mypoints, "CC",key.space="bottom", xlim=c(-77.4,-76.0),ylim=c(-12.32,-11.0),
        scales=list(draw=T,y = list(tck=c(-1, -1)),x = list(tck=c(-1, -1))), cuts = 3,pch =19,lwd=6,
        col.regions=c('red', 'green', 'blue'), main= c("a) CC"),
        legendEntries = c("-1.0 -0.5>", "-0.5 0.5", "<0.5 1.0")) + 
        as.layer(spplot(zona_estudio, fill="transparent",  col="white"), under = TRUE) + 
        as.layer(spplot(elev, col.regions=grey.colors(50)) , under = TRUE)

plotb <-spplot(mypoints, "Bias",key.space="bottom", xlim=c(-77.4,-76.0),ylim=c(-12.32,-11.0), 
        scales=list(draw=T,y = list(tck=c(-1, -1)),x = list(tck=c(-1, -1))), cuts = 3,pch =19,lwd=6,
        col.regions=c('violetred1', 'paleturquoise1', 'turquoise3'), main= c("b) Bias"),
        legendEntries = c("-1.0 -0.5>", "-0.5 0.5", "<0.5 1.0")) + 
        as.layer(spplot(zona_estudio, fill="transparent",  col="white"), under = TRUE) + 
        as.layer(spplot(elev, col.regions=grey.colors(50)) , under = TRUE)


png("./data/output/plots/plot_std.png",width=900,height=650,)
gridExtra::grid.arrange(plota,plotb, ncol=2,nrow=1)
dev.off()
