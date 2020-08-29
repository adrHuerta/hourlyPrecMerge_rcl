

# foldersat="./data/raw/early/2017/02"
# list_rasters <- list.files() %>%
#                .[match(foldersat,.)] %>%
#                as.list() %>%
#                lapply(., function(z){
#                z <- list.files(z, full.names = T)
#                z_res <- as.list(z)
#                z_res <- lapply(z_res, function(x){
#                x <- raster(x)
#                rc2 <- crop(x, area )
#    })
#  })

border <- raster::shapefile("./data/shapes/DEPARTAMENTOS.shp")
basins <- raster::shapefile("./data/shapes/CHIRILU.shp")
rt1 <- raster::raster("./data/raw/early/2017/03/3B-HHR-E.MS.MRG.3IMERG.20170316-S213000-E215959.1290.V06B.tif")
rt2 <- raster::raster("./data/raw/early/2017/03/3B-HHR-E.MS.MRG.3IMERG.20170316-S220000-E222959.1320.V06B.tif")
r <- rt1+rt2

r_peru <- crop(r, extent(-85,-65.0, -20,1))
r_chirilu <- crop(r, extent(-77.4,-76.0, -12.32,-11.0))

# Set color palette
zeroCol <-"#ffffff" # #white 
color <-rainbow(100)
myTheme <- rasterVis::rasterTheme(region = c(zeroCol,color))

# Plot

png("./data/output/plots/plot_early.png",width=900,height=700, res = 150)
rasterVis::levelplot(r_peru, par.settings = myTheme, margin = FALSE, 
          main = expression("Precipitación" ~ (mm ~ hr^{-1})),
          xlab="",ylab="")+
          layer(sp.polygons(border, lwd=0.8))

dev.off()

png("./data/output/plots/plot_early_chirilu.png",width=900,height=700, res = 150)
rasterVis::levelplot(r_chirilu, par.settings = myTheme, margin = FALSE, 
          main = expression("Precipitación" ~ (mm ~ hr^{-1})),
          xlab="",ylab="")+
  layer(sp.polygons(border, lwd=0.8))+
  layer(sp.polygons(basins, lwd=0.8))
dev.off()
