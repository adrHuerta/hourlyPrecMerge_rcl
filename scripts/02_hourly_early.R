library(raster)
library(dplyr)

obs_data <- readRDS("./data/processed/obs/obs_data_qc_v2.rds")
path  <- getwd()
setwd("./data/raw/early/")
folder_sat <- c("early_cut") # file early CHIRILU

list_raster <- list.files() %>%
               .[match(folder_sat,.)] %>%
               as.list() %>%
               lapply(.,function(z){
               z <- list.files(z, full.names = T)
               z_res <- as.list(z)
               z_res <- lapply(z_res, function(x){
               rc2 <- raster(x) 
                    })
               })

data_e <- stack(list_raster[[1]])


# early hourly stack
indice <- as.numeric(rep(1:nlayer,rep(2,nlayer)))
early_hr <- stackApply(data_e, indice, fun = sum)

# early time series
obs_data$xyz <- as.data.frame(obs_data$xyz)
# IDS removed ID472A0766, ID472542FE, ID4726B574, ID47E33064
obs_data$xyz <- SpatialPointsDataFrame(coords=obs_data$xyz[,3:4],data=obs_data$xyz[-c(34,16,36,27),],
                proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


ts_early<- t(extract(early_hr,obs_data$xyz))
colnames(ts_early) <- obs_data$xyz$CODE
sat_data <- xts::xts(ts_early, order.by = index(obs_data$value))

#save
setwd(path)
saveRDS(object=sat_data, file = "./data/processed/sat/sat_data.rds")
