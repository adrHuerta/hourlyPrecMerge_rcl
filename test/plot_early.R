# Load libraries
library('rasterVis')
library(RColorBrewer)
library(maptools)
library(mapdata)


r <- raster::raster("./data/raw/early/2017/02/3B-HHR-E.MS.MRG.3IMERG.20170212-S210000-E212959.1260.V06B.tif")
r <- crop(r, extent(-85,-65.0, -20,-1))

# Set color palette
zeroCol <-"#ffffff" # #white 
color2 <-rainbow(50)
myTheme2 <- rasterTheme(region = c(zeroCol,color2))

# Plot
levelplot(r, par.settings = myTheme2, margin = FALSE, main = expression("Precipitation" ~ (mm ~ día^{-1})))

bPols <- map2SpatialPolygons(border, IDs=border$DEPARTAMEN,proj4string=CRS("+proj=longlat +datum=WGS84"))

levelplot+bPols
