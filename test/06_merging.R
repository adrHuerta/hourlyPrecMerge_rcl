rm(list = ls())
"%>%" = magrittr::`%>%`

library(xts)
library(raster)
source("./src/interpolation.R")

obs <- readRDS("./data/processed/obs/obs_data_qc_v4.rds")
sat <-readRDS("./data/processed/sat/sat_data2.rds")$early 
# example event

E3 <- obs$value["2018-01-25 08:00:00/2018-01-26 02:00:00"]
E3 <- E3[, colSums(is.na(E3)) != nrow(E3)]
XYZ <- obs$xyz[match(colnames(E3), obs$xyz$CODE), ]
E3_results <- list()

for(time_step_i in seq_along(time(E3))){

  time_step_i = 6
  time_step = as.character(time(E3)[time_step_i])
  XYZ@data["obs"] <- E3[time_step,] %>% as.numeric()
  gauge_points_time <- XYZ %>% .[complete.cases(.@data),]
  gridded_cov_1_time <- subset(sat, which(raster::getZ(sat) == time_step) - seq(0:0, 0))
  gridded_cov_2_time <- subset(sat, which(raster::getZ(sat) == time_step) - seq(0:0, 1))

  model00 <- gridded_cov_1_time
  model01 <- IDW(gauge_points = gauge_points_time, gridded_cov = gridded_cov_1_time)
  model02 <- OK(gauge_points = gauge_points_time, gridded_cov = gridded_cov_1_time)
  model03 <- GRA(gauge_points = gauge_points_time, gridded_cov = gridded_cov_1_time)
  model04 <- GDA(gauge_points = gauge_points_time, gridded_cov = gridded_cov_1_time)
  model05 <- RIDW(gauge_points = gauge_points_time, gridded_cov = gridded_cov_1_time)
  model06 <- RIDW(gauge_points = gauge_points_time, gridded_cov = gridded_cov_2_time)
  model07 <- CM_IDW(gauge_points = gauge_points_time, gridded_cov = gridded_cov_1_time)
  model08 <- RK(gauge_points = gauge_points_time, gridded_cov = gridded_cov_1_time)
  model09 <- RK(gauge_points = gauge_points_time, gridded_cov = gridded_cov_2_time)
  model10 <- CM_OK(gauge_points = gauge_points_time, gridded_cov = gridded_cov_1_time)

  res <- raster::brick(model00, model01, model02,
                       model03, model04,
                       model05, model06, model07,
                       model08, model09, model10)

  names(res) <- c("SAT", "IDW", "OK",
                  "GRA", "GDA",
                  "RIDW", "RIDW2","CM_IDW",
                  "RK", "RK2", "CM_OK")

  E3_results[[time_step_i]] <- res

}
# 
# E3_model00 <- raster::brick(lapply(E3_results, function(x) x[[1]]))
# E3_model01 <- raster::brick(lapply(E3_results, function(x) x[[2]]))
# E3_model02 <- raster::brick(lapply(E3_results, function(x) x[[3]]))
# E3_model03 <- raster::brick(lapply(E3_results, function(x) x[[4]]))
# E3_model04 <- raster::brick(lapply(E3_results, function(x) x[[5]]))
# E3_model05 <- raster::brick(lapply(E3_results, function(x) x[[6]]))
# E3_model06 <- raster::brick(lapply(E3_results, function(x) x[[7]]))
# E3_model07 <- raster::brick(lapply(E3_results, function(x) x[[8]]))
# E3_model08 <- raster::brick(lapply(E3_results, function(x) x[[9]]))
