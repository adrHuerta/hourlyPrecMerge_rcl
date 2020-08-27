require(xts)
rm(list = ls())
"%>%" = magrittr::`%>%`

source('./src/operational_qc.R')

# raw data
obs_data <- readRDS("./data/raw/obs/obs_data.rds")

# example date-times
example_dryday <- obs_data$value["2018-02-15 16:00:00"] 

# 2018-02-15 21:00:00
# 2018-02-15 16:00:00
# 2019-02-15 15:00:00

# example spatial point obj (as the operational data file format)
example_data <- obs_data$xyz[, c("CODE", "LAT", "LON")]
example_data$DATE <- as.character(time(example_wetday))
example_data$OBS <- as.numeric(example_dryday)

## raw
# non-QC plot
spplot(example_data[, "OBS"])

## quality control

# internal consistency check
example_data <- qc_internal_consistency_check(spatial_point = example_data)

# the extreme check
# getting daily max value by month from PPISCO (only need one time)
ppisco_p <- raster::brick("./data/shapes/PPISCOp_sample.nc")
ppisco_p <- raster::setZ(ppisco_p, seq(as.Date("1981-01-01"), as.Date("2016-12-31"), by = "day"))
ppisco_p_max_monthly <- raster::zApply(ppisco_p, by = format(ppisco_p@z$time, "%m"), fun = max)

daily_max_monthly <- data.frame(raster::extract(ppisco_p_max_monthly, example_data))
rownames(daily_max_monthly) <- example_data@data$CODE

example_data <- qc_extreme_check(spatial_point = example_data,
                                 daily_monthly_limits = daily_max_monthly)

# spatial consistency
example_data <- qc_spatial_consistency(spatial_point = example_data)

# QC-plot
spplot(example_data[, "OBS"])

obs_data_qc <- obs_data

for(i in 7208:dim(obs_data$value)[1]){
  
  xyz <- obs_data$xyz[, c("CODE", "LAT", "LON")]
  step_time <- obs_data$value[i,]
  xyz$DATE <- as.character(time(step_time))
  xyz$OBS <- as.numeric(step_time)
  
  if(all(is.na(step_time)) | (sum(step_time, na.rm = TRUE) == 0)){
    
    next
    
  } else {
    
    qc_internal_consistency_check(spatial_point = xyz) %>%
      qc_extreme_check(spatial_point = ., daily_monthly_limits = daily_max_monthly)  %>%
      qc_spatial_consistency(spatial_point = .) -> xyz_qc
    
    zoo::coredata(obs_data_qc$value[i, ]) <- xyz_qc@data$OBS
    print(i)
  }
  }
















piscop <- brick("/home/adrian/Documents/wa_budyko_datasets/netcdf/P/PPISCOpd.nc")
ext <- extent(c(-77.5, -75, -12.5, -10.5))
piscop_sample <- crop(piscop, ext)
writeRaster(piscop_sample, "./data/shapes/PPISCOp_sample.nc")
