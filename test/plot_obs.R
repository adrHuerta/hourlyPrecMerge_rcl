

obs_data <- readRDS("./data/raw/obs/obs_data.rds")

border <- raster::shapefile("./data/shapes/DEPARTAMENTOS.shp")
provs<- raster::shapefile("./data/shapes/PROVINCIAS.shp")
basins <- raster::shapefile("./data/shapes/CHIRILU.shp")

r <- raster::raster("./data/shapes/chirilu_ProjectRaster.tif")
yat <- round(seq(extent(r)@ymin, 
                 extent(r)@ymax, length.out = 3),2)
xat <- round(seq(extent(r)@xmin, 
                 extent(r)@xmax, length.out = 3),2)

sl1 <- list('sp.pointLabel', obs_data$xyz, label=obs_data$xyz$CODE,
            cex=0.5, col='black')

png("./scripts/plot/plot_obs.png",width=531,height=350) 
spplot(r,col.regions = viridis_pal(option="D")(255),
      sp.layout=list(list(provs, col="white",fill="gray87",first=TRUE),
                     list(border),sl1),
       scales = list(x = list(at = xat), y = list(at = yat)),
       xlim=c(-77.4,-76.0),ylim=c(-12.32,-11.1))+
       layer(sp.points(obs_data$xyz,pch =19,lwd=8,col="red2"))+
       layer(sp.polygons(basins)) 
       grid.text("altitud(m.s.n.m)", x=unit(0.93, "npc"), y=unit(0.50, "npc"),gp=gpar(fontsize=9), rot=-90)

dev.off()
  