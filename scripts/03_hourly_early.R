"%>%" = magrittr::`%>%`
source("./src/Check_data.R")

# checking imerg files
check_imerg_file(location = "./data/raw/early/early_cut",
                 data_ini = "2014-01-01 05:00",
                 data_end = "2020-01-01 04:30")

# obs data
obs_data <- readRDS("./data/processed/obs/obs_data_qc_v3.rds")

# list_raster <- list.files("./data/raw/early/early_cut", 
#                           full.names = TRUE) %>%
#                lapply(function(z){
#                raster::raster(z)
#                })
# 
# list_raster <- mapply(function(x, y){
#   list_raster[[x]] + list_raster[[y]]
#   },
#   x = seq(1, length(list_raster), 2),
#   y = seq(2, length(list_raster), 2))
# 
# # early hourly stack
# raster::writeRaster(list_raster,
#                     filename = "./data/processed/early/early_chirilu.nc")

# time since: 2014-01-01 05:00
sat_data <- raster::brick("./data/processed/early/early_chirilu.nc")
raster::projection(sat_data) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# early time series
obs_data$xyz <- as.data.frame(obs_data$xyz)
# IDS removed ID472A0766, ID472542FE, ID4726B574, ID47E33064
# no se supone que esto ya se elimino en 02_select_obs? 
# eliminar todo en 02_select_obs, para ya no hacer esto
obs_data$xyz <- sp::SpatialPointsDataFrame(coords = obs_data$xyz[,3:4],
                                           data = obs_data$xyz[-c(34,16,36,27),],
                                           proj4string = sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


ts_early<- t(extract(early_hr,obs_data$xyz))
colnames(ts_early) <- obs_data$xyz$CODE
sat_data <- xts::xts(ts_early, order.by = index(obs_data$value))

#save
# setwd(path) no se debe hacer esto!
saveRDS(object=list(sat_data = sat_data, 
                    early_hr = early_hr), 
        file = "./data/processed/sat/sat_data.rds")
