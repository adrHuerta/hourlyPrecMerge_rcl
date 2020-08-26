
library(maptools)
library(rasterVis)
library(viridis)
library(grid)
#library(rgdal)

#read shapefiles
#xySpD  <- readShapeSpatial("./data/raw/obs/XY/SPDF")
basins <- readShapeSpatial("./data/shapes/CHIRILU")
border <- readShapeLines("./data/shapes/DEPARTAMENTOS")
watershed <- readShapeSpatial("./data/shapes/Lakesperu")
rivers<- readShapeSpatial("./data/shapes/rios_CRL")

r<-raster("./data/shapes/chirilu_ProjectRaster.tif")
#projection(chirilu)


yat <- round(seq(extent(r)@ymin, 
                extent(r)@ymax, length.out = 5),3)
xat <- round(seq(extent(r)@xmin, 
                extent(r)@xmax, length.out = 5),2)
scale <- list("SpatialPolygonsRescale",
              layout.scale.bar(height=0.05),
              offset = c(-76.20,-12.25),
              scale = 5000, fill=c("transparent","black"))
text1 <- list("sp.text", c(-76.33,-12.25), "0")
text2 <- list("sp.text", c(-76.04,-12.25), "10 km")

arrow = list("SpatialPolygonsRescale", layout.north.arrow(),
             offset = c(-11.5,-76.6), scale = 0.5, which = 2)

spplot(r, col.regions = viridis_pal(option="D")(255),
       panel = function(...){
         panel.levelplot(...)
         panel.abline(h = yat, v = xat, col = "grey0", lwd = 0.8, lty = 3) 
       },
       scales = list(x = list(at = xat),
                     y = list(at = yat)),
       sp.layout=arrow,
       xlim=c(-77.4,-76.0),ylim=c(-12.4,-11.1))+
      layer(sp.polygons(basins, lwd=0.8))+
      layer(sp.polygons(border, lwd=0.8))+
      layer(sp.polygons(watershed, lwd=0.8,col = "blue"))+
      layer(sp.polygons(rivers, lwd=0.8))
      grid.text("altitud(m.s.n.m)", x=unit(0.95, "npc"), y=unit(0.50, "npc"), rot=-90)

      