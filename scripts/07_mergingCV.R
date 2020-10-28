rm(list = ls())
"%>%" = magrittr::`%>%`

library(xts)
library(raster)
source("./src/interpolation.R")

obs <- readRDS("./data/processed/obs/obs_data_qc_v4.rds")
sat <-readRDS("./data/processed/sat/sat_data2.rds")$early

# events
E1_dates <- c("2015-02-09 10:00:00", "2015-02-10 07:00:00")
E2_dates <- c("2015-03-20 11:00:00", "2015-03-22 03:00:00")
E3_dates <- c("2016-02-25 11:00:00", "2016-02-25 23:00:00")
E4_dates <- c("2016-02-28 13:00:00", "2016-02-29 10:00:00")
E5_dates <- c("2017-03-15 13:00:00", "2017-03-16 03:00:00")
E6_dates <- c("2017-03-18 02:00:00", "2017-03-20 03:00:00")
E7_dates <- c("2018-01-25 08:00:00", "2018-01-26 02:00:00")
E8_dates <- c("2019-01-27 12:00:00", "2019-01-30 23:00:00")

Events_dates <- list(E1_dates, E2_dates, E3_dates,
                     E4_dates, E5_dates, E6_dates,
                     E7_dates, E8_dates)

# model00: SAT
# lapply(Events_dates, function(event){
#   
#   event_obs_data <- obs$value[ paste(event, collapse = "/") ]
#   event_obs_data <- event_obs_data[, colSums(is.na(event_obs_data)) != nrow(event_obs_data)]
#   XYZ_event <- obs$xyz[match(colnames(event_obs_data), obs$xyz$CODE), ]
#   
#   sapply(1:dim(XYZ_event)[1], function(station_step){
#     
#     XYZ_station_to_validate <- XYZ_event[station_step,]
#     XYZ_station_step <- XYZ_event[-station_step,]
#     event_obs_data_station_step <- event_obs_data[, -station_step]
#     
#     lapply(as.character(time(event_obs_data_station_step)), function(time_step){
#       
#       XYZ_station_step@data["obs"] <- event_obs_data_station_step[time_step,] %>% as.numeric()
#       gauge_points_time <- XYZ_station_step %>% .[complete.cases(.@data),]
#       gridded_cov_1_time <- subset(sat, which(raster::getZ(sat) == time_step) - seq(0:0, 0))
#       
#       model00 <- gridded_cov_1_time
#       model00
#       
#     }) -> model_res
#     
#     model_res <- raster::brick(model_res)
#     raster::extract(model_res, XYZ_station_to_validate)
#     
#   }) -> CV_data
#   
#   rownames(CV_data) <- as.character(time(event_obs_data))
#   colnames(CV_data) <- XYZ_event@data$CODE
#   CV_data
#   
# }) -> model00_CV_res 

# model00: SAT (parallel)
lapply(Events_dates, function(event){
  
  event_obs_data <- obs$value[ paste(event, collapse = "/") ]
  event_obs_data <- event_obs_data[, colSums(is.na(event_obs_data)) != nrow(event_obs_data)]
  XYZ_event <- obs$xyz[match(colnames(event_obs_data), obs$xyz$CODE), ]
  
  sapply(1:dim(XYZ_event)[1], function(station_step){
    
    XYZ_station_to_validate <- XYZ_event[station_step,]
    XYZ_station_step <- XYZ_event[-station_step,]
    event_obs_data_station_step <- event_obs_data[, -station_step]
    
    parallel::mclapply(as.character(time(event_obs_data_station_step)), function(time_step){
      
      XYZ_station_step@data["obs"] <- event_obs_data_station_step[time_step,] %>% as.numeric()
      gauge_points_time <- XYZ_station_step %>% .[complete.cases(.@data),]
      gridded_cov_1_time <- subset(sat, which(raster::getZ(sat) == time_step) - seq(0:0, 0))
      
      model00 <- gridded_cov_1_time
      model00
      
    }, 
    mc.cores = 5) -> model_res
    
    model_res <- raster::brick(model_res)
    raster::extract(model_res, XYZ_station_to_validate)
    
  }) -> CV_data
  
  rownames(CV_data) <- as.character(time(event_obs_data))
  colnames(CV_data) <- XYZ_event@data$CODE
  CV_data
  
}) -> model00_CV_res

# model01: IDW
lapply(Events_dates, function(event){
  
  event_obs_data <- obs$value[ paste(event, collapse = "/") ]
  event_obs_data <- event_obs_data[, colSums(is.na(event_obs_data)) != nrow(event_obs_data)]
  XYZ_event <- obs$xyz[match(colnames(event_obs_data), obs$xyz$CODE), ]
  
  sapply(1:dim(XYZ_event)[1], function(station_step){
    
    XYZ_station_to_validate <- XYZ_event[station_step,]
    XYZ_station_step <- XYZ_event[-station_step,]
    event_obs_data_station_step <- event_obs_data[, -station_step]
    
    parallel::mclapply(as.character(time(event_obs_data_station_step)), function(time_step){
      
      XYZ_station_step@data["obs"] <- event_obs_data_station_step[time_step,] %>% as.numeric()
      gauge_points_time <- XYZ_station_step %>% .[complete.cases(.@data),]
      gridded_cov_1_time <- subset(sat, which(raster::getZ(sat) == time_step) - seq(0:0, 0))
      
      model00 <- IDW(gauge_points = gauge_points_time, gridded_cov = gridded_cov_1_time)
      model00
      
    }, 
    mc.cores = 5) -> model_res
    
    model_res <- raster::brick(model_res)
    raster::extract(model_res, XYZ_station_to_validate)
    
  }) -> CV_data
  
  rownames(CV_data) <- as.character(time(event_obs_data))
  colnames(CV_data) <- XYZ_event@data$CODE
  CV_data
  
}) -> model01_CV_res

# model02: OK
lapply(Events_dates, function(event){
  
  event_obs_data <- obs$value[ paste(event, collapse = "/") ]
  event_obs_data <- event_obs_data[, colSums(is.na(event_obs_data)) != nrow(event_obs_data)]
  XYZ_event <- obs$xyz[match(colnames(event_obs_data), obs$xyz$CODE), ]
  
  sapply(1:dim(XYZ_event)[1], function(station_step){
    
    XYZ_station_to_validate <- XYZ_event[station_step,]
    XYZ_station_step <- XYZ_event[-station_step,]
    event_obs_data_station_step <- event_obs_data[, -station_step]
    
    parallel::mclapply(as.character(time(event_obs_data_station_step)), function(time_step){
      
      XYZ_station_step@data["obs"] <- event_obs_data_station_step[time_step,] %>% as.numeric()
      gauge_points_time <- XYZ_station_step %>% .[complete.cases(.@data),]
      gridded_cov_1_time <- subset(sat, which(raster::getZ(sat) == time_step) - seq(0:0, 0))
      
      model00 <- OK(gauge_points = gauge_points_time, gridded_cov = gridded_cov_1_time)
      model00
      
    }, 
    mc.cores = 5) -> model_res
    
    model_res <- raster::brick(model_res)
    raster::extract(model_res, XYZ_station_to_validate)
    
  }) -> CV_data
  
  rownames(CV_data) <- as.character(time(event_obs_data))
  colnames(CV_data) <- XYZ_event@data$CODE
  CV_data
  
}) -> model02_CV_res

# model03: GRA
lapply(Events_dates, function(event){
  
  event_obs_data <- obs$value[ paste(event, collapse = "/") ]
  event_obs_data <- event_obs_data[, colSums(is.na(event_obs_data)) != nrow(event_obs_data)]
  XYZ_event <- obs$xyz[match(colnames(event_obs_data), obs$xyz$CODE), ]
  
  sapply(1:dim(XYZ_event)[1], function(station_step){
    
    XYZ_station_to_validate <- XYZ_event[station_step,]
    XYZ_station_step <- XYZ_event[-station_step,]
    event_obs_data_station_step <- event_obs_data[, -station_step]
    
    parallel::mclapply(as.character(time(event_obs_data_station_step)), function(time_step){
      
      XYZ_station_step@data["obs"] <- event_obs_data_station_step[time_step,] %>% as.numeric()
      gauge_points_time <- XYZ_station_step %>% .[complete.cases(.@data),]
      gridded_cov_1_time <- subset(sat, which(raster::getZ(sat) == time_step) - seq(0:0, 0))
      
      model00 <- GRA(gauge_points = gauge_points_time, gridded_cov = gridded_cov_1_time)
      model00
      
    }, 
    mc.cores = 5) -> model_res
    
    model_res <- raster::brick(model_res)
    raster::extract(model_res, XYZ_station_to_validate)
    
  }) -> CV_data
  
  rownames(CV_data) <- as.character(time(event_obs_data))
  colnames(CV_data) <- XYZ_event@data$CODE
  CV_data
  
}) -> model03_CV_res

# model04: GDA
lapply(Events_dates, function(event){
  
  event_obs_data <- obs$value[ paste(event, collapse = "/") ]
  event_obs_data <- event_obs_data[, colSums(is.na(event_obs_data)) != nrow(event_obs_data)]
  XYZ_event <- obs$xyz[match(colnames(event_obs_data), obs$xyz$CODE), ]
  
  sapply(1:dim(XYZ_event)[1], function(station_step){
    
    XYZ_station_to_validate <- XYZ_event[station_step,]
    XYZ_station_step <- XYZ_event[-station_step,]
    event_obs_data_station_step <- event_obs_data[, -station_step]
    
    parallel::mclapply(as.character(time(event_obs_data_station_step)), function(time_step){
      
      XYZ_station_step@data["obs"] <- event_obs_data_station_step[time_step,] %>% as.numeric()
      gauge_points_time <- XYZ_station_step %>% .[complete.cases(.@data),]
      gridded_cov_1_time <- subset(sat, which(raster::getZ(sat) == time_step) - seq(0:0, 0))
      
      model00 <- GDA(gauge_points = gauge_points_time, gridded_cov = gridded_cov_1_time)
      model00
      
    }, 
    mc.cores = 5) -> model_res
    
    model_res <- raster::brick(model_res)
    raster::extract(model_res, XYZ_station_to_validate)
    
  }) -> CV_data
  
  rownames(CV_data) <- as.character(time(event_obs_data))
  colnames(CV_data) <- XYZ_event@data$CODE
  CV_data
  
}) -> model04_CV_res

# model05: RIDW
lapply(Events_dates, function(event){
  
  event_obs_data <- obs$value[ paste(event, collapse = "/") ]
  event_obs_data <- event_obs_data[, colSums(is.na(event_obs_data)) != nrow(event_obs_data)]
  XYZ_event <- obs$xyz[match(colnames(event_obs_data), obs$xyz$CODE), ]
  
  sapply(1:dim(XYZ_event)[1], function(station_step){
    
    XYZ_station_to_validate <- XYZ_event[station_step,]
    XYZ_station_step <- XYZ_event[-station_step,]
    event_obs_data_station_step <- event_obs_data[, -station_step]
    
    parallel::mclapply(as.character(time(event_obs_data_station_step)), function(time_step){
      
      XYZ_station_step@data["obs"] <- event_obs_data_station_step[time_step,] %>% as.numeric()
      gauge_points_time <- XYZ_station_step %>% .[complete.cases(.@data),]
      gridded_cov_1_time <- subset(sat, which(raster::getZ(sat) == time_step) - seq(0:0, 0))
      
      model00 <- RIDW(gauge_points = gauge_points_time, gridded_cov = gridded_cov_1_time)
      model00
      
    }, 
    mc.cores = 5) -> model_res
    
    model_res <- raster::brick(model_res)
    raster::extract(model_res, XYZ_station_to_validate)
    
  }) -> CV_data
  
  rownames(CV_data) <- as.character(time(event_obs_data))
  colnames(CV_data) <- XYZ_event@data$CODE
  CV_data
  
}) -> model05_CV_res

# model06: RIDW2
lapply(Events_dates, function(event){
  
  event_obs_data <- obs$value[ paste(event, collapse = "/") ]
  event_obs_data <- event_obs_data[, colSums(is.na(event_obs_data)) != nrow(event_obs_data)]
  XYZ_event <- obs$xyz[match(colnames(event_obs_data), obs$xyz$CODE), ]
  
  sapply(1:dim(XYZ_event)[1], function(station_step){
    
    XYZ_station_to_validate <- XYZ_event[station_step,]
    XYZ_station_step <- XYZ_event[-station_step,]
    event_obs_data_station_step <- event_obs_data[, -station_step]
    
    parallel::mclapply(as.character(time(event_obs_data_station_step)), function(time_step){
      
      XYZ_station_step@data["obs"] <- event_obs_data_station_step[time_step,] %>% as.numeric()
      gauge_points_time <- XYZ_station_step %>% .[complete.cases(.@data),]
      #gridded_cov_1_time <- subset(sat, which(raster::getZ(sat) == time_step) - seq(0:0, 0))
      gridded_cov_2_time <- subset(sat, which(raster::getZ(sat) == time_step) - seq(0:0, 1))
      
      model00 <- RIDW(gauge_points = gauge_points_time, gridded_cov = gridded_cov_2_time)
      model00
      
    }, 
    mc.cores = 5) -> model_res
    
    model_res <- raster::brick(model_res)
    raster::extract(model_res, XYZ_station_to_validate)
    
  }) -> CV_data
  
  rownames(CV_data) <- as.character(time(event_obs_data))
  colnames(CV_data) <- XYZ_event@data$CODE
  CV_data
  
}) -> model06_CV_res

# model07: CM_IDW
lapply(Events_dates, function(event){
  
  event_obs_data <- obs$value[ paste(event, collapse = "/") ]
  event_obs_data <- event_obs_data[, colSums(is.na(event_obs_data)) != nrow(event_obs_data)]
  XYZ_event <- obs$xyz[match(colnames(event_obs_data), obs$xyz$CODE), ]
  
  sapply(1:dim(XYZ_event)[1], function(station_step){
    
    XYZ_station_to_validate <- XYZ_event[station_step,]
    XYZ_station_step <- XYZ_event[-station_step,]
    event_obs_data_station_step <- event_obs_data[, -station_step]
    
    parallel::mclapply(as.character(time(event_obs_data_station_step)), function(time_step){
      
      XYZ_station_step@data["obs"] <- event_obs_data_station_step[time_step,] %>% as.numeric()
      gauge_points_time <- XYZ_station_step %>% .[complete.cases(.@data),]
      gridded_cov_1_time <- subset(sat, which(raster::getZ(sat) == time_step) - seq(0:0, 0))
      #gridded_cov_2_time <- subset(sat, which(raster::getZ(sat) == time_step) - seq(0:0, 1))
      
      model00 <- CM_IDW(gauge_points = gauge_points_time, gridded_cov = gridded_cov_1_time)
      model00
      
    }, 
    mc.cores = 5) -> model_res
    
    model_res <- raster::brick(model_res)
    raster::extract(model_res, XYZ_station_to_validate)
    
  }) -> CV_data
  
  rownames(CV_data) <- as.character(time(event_obs_data))
  colnames(CV_data) <- XYZ_event@data$CODE
  CV_data
  
}) -> model07_CV_res

# model08: RK
lapply(Events_dates, function(event){
  
  event_obs_data <- obs$value[ paste(event, collapse = "/") ]
  event_obs_data <- event_obs_data[, colSums(is.na(event_obs_data)) != nrow(event_obs_data)]
  XYZ_event <- obs$xyz[match(colnames(event_obs_data), obs$xyz$CODE), ]
  
  sapply(1:dim(XYZ_event)[1], function(station_step){
    
    XYZ_station_to_validate <- XYZ_event[station_step,]
    XYZ_station_step <- XYZ_event[-station_step,]
    event_obs_data_station_step <- event_obs_data[, -station_step]
    
    parallel::mclapply(as.character(time(event_obs_data_station_step)), function(time_step){
      
      XYZ_station_step@data["obs"] <- event_obs_data_station_step[time_step,] %>% as.numeric()
      gauge_points_time <- XYZ_station_step %>% .[complete.cases(.@data),]
      gridded_cov_1_time <- subset(sat, which(raster::getZ(sat) == time_step) - seq(0:0, 0))
      #gridded_cov_2_time <- subset(sat, which(raster::getZ(sat) == time_step) - seq(0:0, 1))
      
      model00 <- RK(gauge_points = gauge_points_time, gridded_cov = gridded_cov_1_time)
      model00
      
    }, 
    mc.cores = 5) -> model_res
    
    model_res <- raster::brick(model_res)
    raster::extract(model_res, XYZ_station_to_validate)
    
  }) -> CV_data
  
  rownames(CV_data) <- as.character(time(event_obs_data))
  colnames(CV_data) <- XYZ_event@data$CODE
  CV_data
  
}) -> model08_CV_res

# model09: RK2
lapply(Events_dates, function(event){
  
  event_obs_data <- obs$value[ paste(event, collapse = "/") ]
  event_obs_data <- event_obs_data[, colSums(is.na(event_obs_data)) != nrow(event_obs_data)]
  XYZ_event <- obs$xyz[match(colnames(event_obs_data), obs$xyz$CODE), ]
  
  sapply(1:dim(XYZ_event)[1], function(station_step){
    
    XYZ_station_to_validate <- XYZ_event[station_step,]
    XYZ_station_step <- XYZ_event[-station_step,]
    event_obs_data_station_step <- event_obs_data[, -station_step]
    
    parallel::mclapply(as.character(time(event_obs_data_station_step)), function(time_step){
      
      XYZ_station_step@data["obs"] <- event_obs_data_station_step[time_step,] %>% as.numeric()
      gauge_points_time <- XYZ_station_step %>% .[complete.cases(.@data),]
      #gridded_cov_1_time <- subset(sat, which(raster::getZ(sat) == time_step) - seq(0:0, 0))
      gridded_cov_2_time <- subset(sat, which(raster::getZ(sat) == time_step) - seq(0:0, 1))
      
      model00 <- RK(gauge_points = gauge_points_time, gridded_cov = gridded_cov_2_time)
      model00
      
    }, 
    mc.cores = 5) -> model_res
    
    model_res <- raster::brick(model_res)
    raster::extract(model_res, XYZ_station_to_validate)
    
  }) -> CV_data
  
  rownames(CV_data) <- as.character(time(event_obs_data))
  colnames(CV_data) <- XYZ_event@data$CODE
  CV_data
  
}) -> model09_CV_res

# model10: CM_OK
lapply(Events_dates, function(event){
  
  event_obs_data <- obs$value[ paste(event, collapse = "/") ]
  event_obs_data <- event_obs_data[, colSums(is.na(event_obs_data)) != nrow(event_obs_data)]
  XYZ_event <- obs$xyz[match(colnames(event_obs_data), obs$xyz$CODE), ]
  
  sapply(1:dim(XYZ_event)[1], function(station_step){
    
    XYZ_station_to_validate <- XYZ_event[station_step,]
    XYZ_station_step <- XYZ_event[-station_step,]
    event_obs_data_station_step <- event_obs_data[, -station_step]
    
    parallel::mclapply(as.character(time(event_obs_data_station_step)), function(time_step){
      
      XYZ_station_step@data["obs"] <- event_obs_data_station_step[time_step,] %>% as.numeric()
      gauge_points_time <- XYZ_station_step %>% .[complete.cases(.@data),]
      gridded_cov_1_time <- subset(sat, which(raster::getZ(sat) == time_step) - seq(0:0, 0))
      #gridded_cov_2_time <- subset(sat, which(raster::getZ(sat) == time_step) - seq(0:0, 1))
      
      model00 <- CM_OK(gauge_points = gauge_points_time, gridded_cov = gridded_cov_1_time)
      model00
      
    }, 
    mc.cores = 5) -> model_res
    
    model_res <- raster::brick(model_res)
    raster::extract(model_res, XYZ_station_to_validate)
    
  }) -> CV_data
  
  rownames(CV_data) <- as.character(time(event_obs_data))
  colnames(CV_data) <- XYZ_event@data$CODE
  CV_data
  
}) -> model10_CV_res


saveRDS(list(model00_CV_res, model01_CV_res, model02_CV_res, model03_CV_res,
             model04_CV_res, model05_CV_res, model06_CV_res, model07_CV_res,
             model08_CV_res, model09_CV_res, model10_CV_res),
        file = "./data/processed/merging/merging_grid_CV.RDS")