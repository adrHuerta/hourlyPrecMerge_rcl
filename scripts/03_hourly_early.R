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
early_hr <- raster::brick("./data/processed/early/early_chirilu.nc")
raster::projection(early_hr) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#early time series
sat_data <- t(raster::extract(early_hr,obs_data$xyz))
colnames(sat_data) <- colnames(obs_data$value)
Time  <- zoo::index(obs_data$value)
sat_data <- xts::xts(sat_data, order.by = Time)

#save
saveRDS(object=list(sat_data = sat_data, 
                    early_hr = early_hr), 
        file = "./data/processed/sat/sat_data.rds")
