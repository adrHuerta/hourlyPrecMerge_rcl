
library(maptools)
library(rasterVis)
library(viridis)
library(grid)

#read shapefiles
basins <- readShapeSpatial("./data/shapes/CHIRILU")
border <- readShapePoly("./data/shapes/DEPARTAMENTOS")
provs <- readShapePoly("./data/shapes/PROVINCIAS")
lakes <- readShapePoly("./data/shapes/Lakesperu")
rivers <- readShapeSpatial("./data/shapes/rios_CRL")
riverleyend <- readShapeSpatial("./data/shapes/riosleyend") 
lakeleyend <- readShapePoly("./data/shapes/lake")
#read raster
r <- raster("./data/shapes/chirilu_ProjectRaster.tif")

#coords
yat <- round(seq(extent(r)@ymin, 
                 extent(r)@ymax, length.out = 3),2)
xat <- round(seq(extent(r)@xmin, 
                 extent(r)@xmax, length.out = 3),2)
#layouts
arrow = list("SpatialPolygonsRescale", layout.north.arrow(type=1), offset = c(-76.09,-11.3), scale=0.1)
scale <- list("SpatialPolygonsRescale",layout.scale.bar(),offset = c(-76.5,-12.28),scale = 0.4, fill=c("black","white"))
text1 <- list("sp.text", c(-76.99,-11.22), "Lagunas naturales",cex=0.65)
text2 <- list("sp.text", c(-76.99,-11.29), "Rios principales",cex=0.65)
text3 <- list("sp.text", c(-76.5,-12.29), "0",cex=0.6)
text4 <- list("sp.text", c(-76.1,-12.29), "20 km",cex=0.6)
sp.label <- function(x, label) {list("sp.text", coordinates(x), label,cex=0.8)}


#Create the plot 
png("./data/output/plots/plot_area.png",width=900,height=700, res = 150)
spplot(r,col.regions = viridis_pal(option="D")(255),
       scales = list(x = list(at = xat),
                     y = list(at = yat)),
       sp.layout=list(list(provs, col="white",fill="gray87", first=TRUE),arrow,scale,
       text1,text2,text3,text4,sp.label(basins,basins$NOMBRE)),
       xlim=c(-77.4,-76.0),ylim=c(-12.32,-11.0),
       par.settings = 
          list(layout.widths = 
                  list(right.padding = 7)))+
   layer(sp.polygons(border, lwd=0.8))+
   layer(sp.polygons(basins, lwd=0.8))+
   layer(sp.polygons(lakes, lwd=0.8,col="dodgerblue1",fill = "dodgerblue1"))+
   layer(sp.polygons(rivers, lwd=0.8,col="navy"))+
   layer(sp.polygons(riverleyend, lwd=1.0,col="navy"))+
   layer(sp.polygons(lakeleyend, lwd=0.8,col="dodgerblue1",fill="dodgerblue1"))
   grid.text("Altitud (msnm)", x=unit(0.93, "npc"), y=unit(0.50, "npc"),gp=gpar(fontsize=12), rot=-90) # ok
   grid.text("Cuenca Alto Mantaro", x=unit(0.65, "npc"), y=unit(0.70, "npc"),gp=gpar(fontsize=8))
   
dev.off()



      