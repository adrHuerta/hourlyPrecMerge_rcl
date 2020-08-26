obs_data <- read.table("./data/raw/obs/DR_Lima_QC1.csv",
                       sep = ",", 
                       header = TRUE)

# dates
obs_data$DATE <- as.POSIXct(obs_data$DATE, format = "%Y-%m-%d %H:%M")

# correcting some bad names
colnames(obs_data)[13] = "EMA-ANTONIO_RAIMONDI"
colnames(obs_data)[44] = "12_DE_OCTUBRE"

obs_data <- xts::xts(obs_data[,-1],
                     obs_data$DATE)

# spatial points
xy <- raster::shapefile("./data/raw/obs/XY/SPDF.shp")
xy@data$ESTACION <- chartr(" ", "_", xy$ESTACION)
xy@data$CODE <- paste("ID", xy@data$CODE, sep = "")

xy_subset <- xy[match(colnames(obs_data), xy$ESTACION),]
raster::plot(xy_subset, axes = TRUE)

xy_chirilu <- xy_subset[xy_subset@data$LAT < -10,] # some stations are too far
raster::plot(xy_chirilu, axes = TRUE)

# re subsetting 
obs_data <- obs_data[, xy_chirilu$ESTACION]
rownames(xy_chirilu@data) <- NULL

xy_chirilu <- xy_chirilu[, c("CODE", "ESTACION", "LON", "LAT", "ALT")]
colnames(obs_data) <- xy_chirilu@data$CODE

# saving as list (.rds)
obs_data <- list(value = obs_data,
                 xyz = xy_chirilu)

saveRDS(obs_data, file = "./data/raw/obs/obs_data.rds")
