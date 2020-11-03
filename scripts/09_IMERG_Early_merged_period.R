## IMERG_Early_merged_period.R
# Merged of observed and GPM IMERG Early, data median of the methods applied 
# to improve the satellite estimation
# 
# written by 


rm(list = ls())
"%>%" = magrittr::`%>%`

library(xts)
library(raster)

source("./src/interpolation.R")

obs <- readRDS("./data/processed/obs/obs_data_qc_v2.rds")
sat <-readRDS("./data/processed/sat/sat_data.rds")$early


#data obs 2014-2019
obs$value <- obs$value["2014-01/2019-12"]
crs(obs$xyz) = "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"


# Period
dates <- c("2019-01-01 00:00:00", "2019-01-02 00:00:00")
dates <- list(dates)

############### MERGING METHODS 
# model00: SAT
lapply(dates, function(period){
  
    period_obs_data <- obs$value[ paste(period, collapse = "/") ]
    period_obs_data <- period_obs_data[, colSums(is.na(period_obs_data)) != nrow(period_obs_data)]
    XYZ_period <- obs$xyz[match(colnames(period_obs_data), obs$xyz$CODE), ]
  
    lapply(as.character(time(period_obs_data)), function(time_step){
    
    XYZ_period@data["obs"] <- period_obs_data[time_step,] %>% as.numeric()
    gauge_points_time <- XYZ_period %>% .[complete.cases(.@data),]
    gridded_cov_1_time <- subset(sat, which(raster::getZ(sat) == time_step) - seq(0:0, 0))
 
    model00 <- gridded_cov_1_time
    names(model00) <- time_step
    model00 <- raster::setZ(model00, time_step)
    model00
    
  }) -> model_res
  
  raster::brick(model_res)  
  
}) -> model00_res 

# model01: IDW
lapply(dates, function(period){
  
    period_obs_data <- obs$value[ paste(period, collapse = "/") ]
    period_obs_data <- period_obs_data[, colSums(is.na(period_obs_data)) != nrow(period_obs_data)]
    XYZ_period <- obs$xyz[match(colnames(period_obs_data), obs$xyz$CODE), ]
  
    lapply(as.character(time(period_obs_data)), function(time_step){
    
    XYZ_period@data["obs"] <- period_obs_data[time_step,] %>% as.numeric()
    gauge_points_time <- XYZ_period %>% .[complete.cases(.@data),]
    gridded_cov_1_time <- subset(sat, which(raster::getZ(sat) == time_step) - seq(0:0, 0))
     
    model00 <- IDW(gauge_points = gauge_points_time, gridded_cov = gridded_cov_1_time)
    names(model00) <- time_step
    model00 <- raster::setZ(model00, time_step)
    model00
    
  }) -> model_res
  
  raster::brick(model_res)  
  
}) -> model01_res 

# model02: OK
lapply(dates, function(period){
  
  period_obs_data <- obs$value[ paste(period, collapse = "/") ]
  period_obs_data <- period_obs_data[, colSums(is.na(period_obs_data)) != nrow(period_obs_data)]
  XYZ_period <- obs$xyz[match(colnames(period_obs_data), obs$xyz$CODE), ]
  
  lapply(as.character(time(period_obs_data)), function(time_step){
    
    XYZ_period@data["obs"] <- period_obs_data[time_step,] %>% as.numeric()
    gauge_points_time <- XYZ_period %>% .[complete.cases(.@data),]
    gridded_cov_1_time <- subset(sat, which(raster::getZ(sat) == time_step) - seq(0:0, 0))
    
    model00 <- OK(gauge_points = gauge_points_time, gridded_cov = gridded_cov_1_time)
    names(model00) <- time_step
    model00 <- raster::setZ(model00, time_step)
    model00
    
  }) -> model_res
  
  raster::brick(model_res)  
  
}) -> model02_res 


# model03: RIDW
lapply(dates, function(period){
  
    period_obs_data <- obs$value[ paste(period, collapse = "/") ]
    period_obs_data <- period_obs_data[, colSums(is.na(period_obs_data)) != nrow(period_obs_data)]
    XYZ_period <- obs$xyz[match(colnames(period_obs_data), obs$xyz$CODE), ]
  
    lapply(as.character(time(period_obs_data)), function(time_step){
    
    XYZ_period@data["obs"] <- period_obs_data[time_step,] %>% as.numeric()
    gauge_points_time <- XYZ_period %>% .[complete.cases(.@data),]
    gridded_cov_1_time <- subset(sat, which(raster::getZ(sat) == time_step) - seq(0:0, 0))
      
    model00 <- RIDW(gauge_points = gauge_points_time, gridded_cov = gridded_cov_1_time)
    names(model00) <- time_step
    model00 <- raster::setZ(model00, time_step)
    model00
    
  }) -> model_res
  
  raster::brick(model_res)  
  
}) -> model03_res 

# model04: RIDW2
lapply(dates, function(period){
  
    period_obs_data <- obs$value[ paste(period, collapse = "/") ]
    period_obs_data <- period_obs_data[, colSums(is.na(period_obs_data)) != nrow(period_obs_data)]
    XYZ_period <- obs$xyz[match(colnames(period_obs_data), obs$xyz$CODE), ]
  
    lapply(as.character(time(period_obs_data)), function(time_step){
    
    XYZ_period@data["obs"] <- period_obs_data[time_step,] %>% as.numeric()
    gauge_points_time <- XYZ_period %>% .[complete.cases(.@data),]
    gridded_cov_2_time <- subset(sat, which(raster::getZ(sat) == time_step) - seq(0:0, 1))
    
    model00 <- RIDW(gauge_points = gauge_points_time, gridded_cov = gridded_cov_2_time)
    names(model00) <- time_step
    model00 <- raster::setZ(model00, time_step)
    model00
    
  }) -> model_res
  
  raster::brick(model_res)  
  
}) -> model04_res 

# model05: CM_IDW
lapply(dates, function(period){
  
    period_obs_data <- obs$value[ paste(period, collapse = "/") ]
    period_obs_data <- period_obs_data[, colSums(is.na(period_obs_data)) != nrow(period_obs_data)]
    XYZ_period <- obs$xyz[match(colnames(period_obs_data), obs$xyz$CODE), ]
  
    lapply(as.character(time(period_obs_data)), function(time_step){
    
    XYZ_period@data["obs"] <- period_obs_data[time_step,] %>% as.numeric()
    gauge_points_time <- XYZ_period %>% .[complete.cases(.@data),]
    gridded_cov_1_time <- subset(sat, which(raster::getZ(sat) == time_step) - seq(0:0, 0))
   
    model00 <- CM_IDW(gauge_points = gauge_points_time, gridded_cov = gridded_cov_1_time)
    names(model00) <- time_step
    model00 <- raster::setZ(model00, time_step)
    model00
    
  }) -> model_res
  
  raster::brick(model_res)  
  
}) -> model05_res 

# model06: RK
lapply(dates, function(period){
  
    period_obs_data <- obs$value[ paste(period, collapse = "/") ]
    period_obs_data <- period_obs_data[, colSums(is.na(period_obs_data)) != nrow(period_obs_data)]
    XYZ_period <- obs$xyz[match(colnames(period_obs_data), obs$xyz$CODE), ]
  
    lapply(as.character(time(period_obs_data)), function(time_step){
    
    XYZ_period@data["obs"] <- period_obs_data[time_step,] %>% as.numeric()
    gauge_points_time <- XYZ_period %>% .[complete.cases(.@data),]
    gridded_cov_1_time <- subset(sat, which(raster::getZ(sat) == time_step) - seq(0:0, 0))

    model00 <- RK(gauge_points = gauge_points_time, gridded_cov = gridded_cov_1_time)
    names(model00) <- time_step
    model00 <- raster::setZ(model00, time_step)
    model00
    
  }) -> model_res
  
  raster::brick(model_res)  
  
}) -> model06_res 

# model07: RK2
lapply(dates, function(period){
  
    period_obs_data <- obs$value[ paste(period, collapse = "/") ]
    period_obs_data <- period_obs_data[, colSums(is.na(period_obs_data)) != nrow(period_obs_data)]
    XYZ_period <- obs$xyz[match(colnames(period_obs_data), obs$xyz$CODE), ]
  
    lapply(as.character(time(period_obs_data)), function(time_step){
    
    XYZ_period@data["obs"] <- period_obs_data[time_step,] %>% as.numeric()
    gauge_points_time <- XYZ_period %>% .[complete.cases(.@data),]
    gridded_cov_2_time <- subset(sat, which(raster::getZ(sat) == time_step) - seq(0:0, 1))
    
    model00 <- RK(gauge_points = gauge_points_time, gridded_cov = gridded_cov_2_time)
    names(model00) <- time_step
    model00 <- raster::setZ(model00, time_step)
    model00
    
  }) -> model_res
  
  raster::brick(model_res)  
  
}) -> model07_res 

# model08: CM_OK
lapply(dates, function(period){
  
    period_obs_data <- obs$value[ paste(period, collapse = "/") ]
    period_obs_data <- period_obs_data[, colSums(is.na(period_obs_data)) != nrow(period_obs_data)]
    XYZ_period <- obs$xyz[match(colnames(period_obs_data), obs$xyz$CODE), ]
  
    lapply(as.character(time(period_obs_data)), function(time_step){
    
    XYZ_period@data["obs"] <- period_obs_data[time_step,] %>% as.numeric()
    gauge_points_time <- XYZ_period %>% .[complete.cases(.@data),]
    gridded_cov_1_time <- subset(sat, which(raster::getZ(sat) == time_step) - seq(0:0, 0))

    model00 <- CM_OK(gauge_points = gauge_points_time, gridded_cov = gridded_cov_1_time)
    names(model00) <- time_step
    model00 <- raster::setZ(model00, time_step)
    model00
    
  }) -> model_res
  
  raster::brick(model_res)  
  
}) -> model08_res


merging_grid <-  list(model00_res, model01_res, model02_res, model03_res,
                      model04_res, model05_res, model06_res, model07_res, model08_res)


############### METHODS median 
names(merging_grid) <- c("SAT", 
                         "IDW", "OK",
                         "RIDW", "RIDW2","CM_IDW",
                         "RK", "RK2", "CM_OK")

merging <- Map(function(x, y) setNames(x, "Period"),
                      merging, names(merging))


median_value <- lapply(merging,
                       function(x){
                         x$Period
                       })

(1:raster::nlayers(median_value$IDW)) %>%
  lapply(function(x){
    raster::calc(raster::brick(median_value$IDW[[x]],
                               median_value$OK[[x]],
                               median_value$RIDW[[x]],
                               median_value$RIDW2[[x]],
                               median_value$CM_IDW[[x]],
                               median_value$RK[[x]],
                               median_value$RK2[[x]],
                               median_value$CM_OK[[x]]),
                 function(z) median(z, na.rm = TRUE))
  }) -> median_value

res <- raster::brick(median_value)
names(res) <- names(merging_events$SAT$Period)

#### SAVE
saveRDS(IMERG_merged=res, 
                file="./data/precessed/IMERG_Early_merged_2014-2019.rds")

