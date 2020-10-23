rm(list = ls())
"%>%" = magrittr::`%>%`
source("./src/Check_data.R")

# checking imerg files
# check_imerg_file(location = "./data/raw/early/early_cut",
#                  data_ini = "2014-01-01 05:00",
#                  data_end = "2020-01-01 04:30")

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

#early time series complete 
sat_data <- t(raster::extract(early_hr,obs_data$xyz))
colnames(sat_data) <- colnames(obs_data$value)
Time  <- seq(as.POSIXct("2014-01-01 01:00"), as.POSIXct("2020-01-01 00:00"), by='hour')
sat_data <- xts::xts(sat_data, order.by = Time)

# extract ts period 11-03 2014-2019

ts1 <- sat_data["2014-11/2015-03"]
ts2 <- sat_data["2015-11/2016-03"]
ts3 <- sat_data["2016-11/2017-03"]
ts4 <- sat_data["2017-11/2018-03"]
ts5 <- sat_data["2018-11/2019-03"]
 
sat_data2 <- rbind(ts1,ts2,ts3,ts4,ts5)
 
early_hr2 <- early_hr
dates_labels <- as.character(zoo::index(sat_data2)) %>%
                 stringr::str_replace_all(":" , ".") %>%
                    stringr::str_replace_all("-" , ".") %>%
                      stringr::str_replace_all(" " , ".")

early_hr2<-early_hr2[[paste0("X",dates_labels)]]

#save
saveRDS(object=list(value = sat_data, 
                    early_hr = early_hr), 
        file = "./data/processed/sat/sat_data.rds")

saveRDS(object=list(value = sat_data2, 
                    early_hr = early_hr2), 
        file = "./data/processed/sat/sat_data2.rds")


