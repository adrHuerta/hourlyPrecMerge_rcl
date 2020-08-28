# if this script is ok, added to ./scripts/plot/"same_name"
# the ouput (that is the .png), should be added at ./data/output/plots (add to gitignore!)


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
riverleyend <- readShapeSpatial("./data/shapes/riosleyend") # do you know another way to add legend ? No D: jajaja, I dont tend to use too many legends, be practical, do not worry :)
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
text1 <- list("sp.text", c(-76.99,-11.22), "Lagunas naturales",cex=0.8)
text2 <- list("sp.text", c(-76.99,-11.29), "Rios principales",cex=0.8)
text3 <- list("sp.text", c(-76.5,-12.29), "0",cex=0.6)
text4 <- list("sp.text", c(-76.1,-12.29), "20 km",cex=0.6)
sp.label <- function(x, label) {list("sp.text", coordinates(x), label,cex=0.8)}


#Create the plot 
png("./scripts/plot/study_area.png",width=531,height=350)
spplot(r,col.regions = viridis_pal(option="D")(255),
       scales = list(x = list(at = xat),
                     y = list(at = yat)),
       sp.layout=list(list(provs, col="white",fill="gray87", first=TRUE),arrow,scale,
       text1,text2,text3,text4,sp.label(basins,basins$NOMBRE)),
       xlim=c(-77.4,-76.0),ylim=c(-12.32,-11.1))+
   layer(sp.polygons(border, lwd=0.8))+
   layer(sp.polygons(basins, lwd=0.8))+
   layer(sp.polygons(lakes, lwd=0.8,col="dodgerblue1",fill = "dodgerblue1"))+
   layer(sp.polygons(rivers, lwd=0.8,col="navy"))+
   layer(sp.polygons(riverleyend, lwd=1.0,col="navy"))+
   layer(sp.polygons(lakeleyend, lwd=0.8,col="dodgerblue1",fill="dodgerblue1"))
   grid.text("altitud(m.s.n.m)", x=unit(0.93, "npc"), y=unit(0.50, "npc"),gp=gpar(fontsize=9), rot=-90)
   grid.text("Cuenca Alto Mantaro", x=unit(0.7, "npc"), y=unit(0.70, "npc"),gp=gpar(fontsize=8))
   
dev.off()



      