rm(list = ls())

require(sp)
require(xts) 
"%>%" = magrittr::`%>%` 

# calling qc functionts
source('./src/operational_qc.R') 

# raw data
obs_data <- readRDS("./data/raw/obs/obs_data.rds")
example_event <- obs_data$value["2018-02-15 16:00:00"] 

# other date options
# 2018-02-15 16:00:00
# 2019-02-15 15:00:00

# example spatial point object
# (as the operational data file format from pc'semahi computer) 
example_obj <- obs_data$xyz[, c("CODE", "LAT", "LON")]
View(example_obj@data) # data format for interpolation
example_obj$DATE <- as.character(time(example_event))
example_obj$OBS <- as.numeric(example_event)

# non-QC dataplot
spplot(example_obj[, "OBS"])

## quality control

# getting daily max value by month from PPISCO (only need one time)
ppisco_p <- raster::brick("./data/shapes/PPISCOp_sample.nc") 
ppisco_p <- raster::setZ(ppisco_p, seq(as.Date("1981-01-01"), as.Date("2016-12-31"), by = "day"))
ppisco_p_max_monthly <- raster::zApply(ppisco_p, by = format(ppisco_p@z$time, "%m"), fun = max)

daily_max_monthly <- data.frame(raster::extract(ppisco_p_max_monthly, example_obj))
rownames(daily_max_monthly) <- example_obj@data$CODE

# applying qc # me aparece el error  Error in predict.gstat(model, blockvals, debug.level = debug.level, ...) 
example_obj %>% #need to change coordinates here with file Pisco
  qc_internal_consistency_check(spatial_point = .) %>%
  qc_extreme_check(spatial_point = ., 
                   daily_monthly_limits = daily_max_monthly) %>%
  qc_spatial_consistency(spatial_point = .) -> example_obj_qc

# QC dataplot
spplot(example_obj_qc[, "OBS"])

# save them as .rds as a list(qc=example_obj_qc,no_qc = example_obj),
# I know is the same, but load and save are not recommendable
saveRDS(list(qc=example_obj_qc,no_qc = example_obj), file="./data/output/example_qc.rds")

## 
# non-QC dataplot y QC dataplot should be shown in the report as example of qc