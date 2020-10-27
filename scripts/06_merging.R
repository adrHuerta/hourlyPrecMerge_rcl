rm(list = ls())
"%>%" = magrittr::`%>%`

library(xts)
library(raster)
source("./src/interpolation.R")

obs <- readRDS("./data/processed/obs/obs_data_qc_v4.rds")
sat <-readRDS("./data/processed/sat/sat_data2.rds")$early

# example event 

E3 <- obs$value["2016-02-25 11:00:00/2016-02-25 23:00:00"]
E3 <- E3[, colSums(is.na(E3)) != nrow(E3)]
XYZ <- obs$xyz[match(colnames(E3), obs$xyz$CODE), ]
E3_results <- list()

for(time_step_i in seq_along(time(E3))){
  time_step_i = 9
  time_step = as.character(time(E3)[time_step_i])
  XYZ@data["obs"] <- E3[time_step,] %>% as.numeric()
  gauge_points_time <- XYZ %>% .[complete.cases(.@data),]
  gridded_cov_1_time <- subset(sat, which(raster::getZ(sat) == time_step) - seq(0:0, 0))
  gridded_cov_2_time <- subset(sat, which(raster::getZ(sat) == time_step) - seq(0:0, 1))
  #gridded_cov_3_time <- subset(sat, which(raster::getZ(sat) == time_step) - seq(0:0, 1))

  merg01 <- GRA(gauge_points = gauge_points_time, gridded_cov = gridded_cov_1_time)
  merg02 <- GDA(gauge_points = gauge_points_time, gridded_cov = gridded_cov_1_time)
  merg03 <- RIDW(gauge_points = gauge_points_time, gridded_cov = gridded_cov_1_time)
  merg04 <- RIDW(gauge_points = gauge_points_time, gridded_cov = gridded_cov_2_time)
  merg05 <- CM(gauge_points = gauge_points_time, gridded_cov = gridded_cov_1_time)

  E3_results[[time_step_i]] <- raster::brick(merg01, merg02, merg03, merg04, merg05)
  
}
sp::spplot(E3_results[[time_step_i]])
