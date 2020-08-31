#plot early an event
require(raster)

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

border <- shapefile("./data/shapes/DEPARTAMENTOS.shp")
basins <- shapefile("./data/shapes/CHIRILU.shp")
rt1 <- raster("./data/raw/early/2017/03/3B-HHR-E.MS.MRG.3IMERG.20170316-S213000-E215959.1290.V06B.tif")
rt2 <- raster("./data/raw/early/2017/03/3B-HHR-E.MS.MRG.3IMERG.20170316-S220000-E222959.1320.V06B.tif")
r <- rt1+rt2

r_peru <- crop(r, extent(-85,-65.0, -18.7,0.3))
r_chirilu <- crop(r, extent(-77.4,-76.0, -12.4,-11.0))

# Set color palette
zeroCol <-"#ffffff" # #white 
color <-rainbow(100)
myTheme <- c(zeroCol,color)

# Plot

png("./data/output/plots/plot_early.png",width=900,height=650, res = 150)

sp1 <-sp::spplot(r_peru, col.regions=myTheme,scales = list(draw = FALSE), 
             colorkey = list(space = "bottom"),
             main=list(label="(a)",cex=0.9))+
             latticeExtra::layer(sp.polygons(border, lwd=0.8))
  

sp2 <-sp::spplot(r_chirilu, col.regions=myTheme,scales = list(draw = FALSE),
             colorkey = list(space = "bottom"),
             main=list(label="(b)",cex=0.9))+
             latticeExtra::layer(sp.polygons(border, lwd=0.8))+
             latticeExtra::layer(sp.polygons(basins, lwd=0.8))
 
gridExtra::grid.arrange(sp1,sp2,ncol=2)

dev.off()



