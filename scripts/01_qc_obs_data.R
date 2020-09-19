library(xts)
library(sp)

"%>%" = magrittr::`%>%`
source('./src/operational_qc.R')


# raw data
obs_data <- readRDS("./data/raw/obs/obs_data.rds")

# getting daily max value by month from PPISCO (only need one time)
ppisco_p <- raster::brick("./data/shapes/PPISCOp_sample.nc")
ppisco_p <- raster::setZ(ppisco_p, seq(as.Date("1981-01-01"), as.Date("2016-12-31"), by = "day"))
ppisco_p_max_monthly <- raster::zApply(ppisco_p, by = format(ppisco_p@z$time, "%m"), fun = max)

daily_max_monthly <- data.frame(raster::extract(ppisco_p_max_monthly, obs_data$xyz))
rownames(daily_max_monthly) <- obs_data$xyz$CODE

# qc of raw data
obs_data_qc <- obs_data

for(i in 1:dim(obs_data$value)[1]){
  
  xyz <- obs_data$xyz[, c("CODE", "LAT", "LON")]
  step_time <- obs_data$value[i,]
  xyz$DATE <- as.character(time(step_time))
  xyz$OBS <- as.numeric(step_time)
  
  if(all(is.na(step_time)) | (sum(!is.na(step_time)) < 3)){
    
    next
    
  } else {
    
    qc_internal_consistency_check(spatial_point = xyz) %>%
      qc_extreme_check(spatial_point = ., daily_monthly_limits = daily_max_monthly)  %>%
      qc_spatial_consistency(spatial_point = .) -> xyz_qc
    
    zoo::coredata(obs_data_qc$value[i, ]) <- xyz_qc@data$OBS
    print(i)
  }
}


saveRDS(obs_data_qc, file = "./data/processed/obs/obs_data_qc_v2.rds")
