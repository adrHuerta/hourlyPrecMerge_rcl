rm(list = ls())
"%>%" = magrittr::`%>%`

# https://cran.r-project.org/web/packages/taskscheduleR/vignettes/taskscheduleR.html

source("./src/make_imerg_path.R")
source("./src/operational_qc.R") # Control de calidad
source("./src/points_inside_pixel.R") # Punto-grilla
source("./src/interpolation.R") # Mezcla

## CHIRILU area
chirilu <- raster::extent(c(-77.4, -76, -12.3, -11)) 

## EMAs
path_ema <- list.files("./data/ejemplo/", full.names = TRUE)
file_ema <- path_ema[which.max(file.mtime(path_ema))]
data_ema <- read.csv(file_ema, header = TRUE)
colnames(data_ema) <- c("NSET", "CODE", "LON", "LAT", "ALT", "DATE", "OBS")

## GPM-IMERG-Early
time_imerg <- unique(data_ema$DATE) %>% strptime("%Y-%m-%dT%H:%M:%S") - 1*60*60*5
file_imerg <- make_imerg_path(date_imerg = time_imerg,
                              dir_path = "./data/raw/early")

data_imerg <- lapply(file_imerg, function(x) raster::raster(x)) %>%
  raster::brick() %>%
  raster::stackApply(., indices = c(1,1,2,2), fun = sum) %>%
  raster::crop(chirilu)

## QC EMA

data_ema <- sp::SpatialPointsDataFrame(coords = data_ema[, c("LON", "LAT")],
                                       data = data_ema,
                                       proj4string = sp::CRS(raster::projection(data_imerg))) %>%
  raster::crop(chirilu)

# Valores maximos y minimos de precipitación diaria para cada mes (PPISCOp)
ppisco_p <- raster::brick("./data/shapes/PPISCOp_sample.nc")
ppisco_p <- raster::setZ(ppisco_p, seq(as.Date("1981-01-01"), as.Date("2016-12-31"), by = "day"))
ppisco_p_max_monthly <- raster::zApply(ppisco_p, by = format(ppisco_p@z$time, "%m"), fun = max)

daily_max_monthly <- data.frame(raster::extract(ppisco_p_max_monthly, data_ema))
rownames(daily_max_monthly) <- data_ema$CODE

# Aplicación del QC
qc_internal_consistency_check(spatial_point = data_ema) %>%
  qc_extreme_check(spatial_point = ., daily_monthly_limits = daily_max_monthly)  %>%
  qc_spatial_consistency(spatial_point = .) -> data_ema

## EMA-grilla

data_ema <- make_single_point(pts = data_ema,
                              rgrid = data_imerg[[1]])$xyz %>%
  .[complete.cases(.@data),]

colnames(data_ema@data)[7] <- "obs"

## Mezcla
IDW_res <- IDW(gauge_points = data_ema, gridded_cov = data_imerg[[1]])
OK_res <- OK(gauge_points = data_ema, gridded_cov = data_imerg[[1]])
RIDW_res <- RIDW(gauge_points = data_ema, gridded_cov = data_imerg[[1]])
RIDW2_res <- RIDW(gauge_points = data_ema, gridded_cov = data_imerg)
CM_IDW_res <- CM_IDW(gauge_points = data_ema, gridded_cov = data_imerg[[1]])
RK_res <- RK(gauge_points = data_ema, gridded_cov = data_imerg[[1]])
RK2_res <- RK(gauge_points = data_ema, gridded_cov = data_imerg)
CM_OK_res <- CM_OK(gauge_points = data_ema, gridded_cov = data_imerg[[1]])

chirilu_gridded <- raster::brick(IDW_res, OK_res,
                                 RIDW_res, RIDW2_res, CM_IDW_res,
                                 RK_res, RK2_res, CM_OK_res) %>%
  raster::calc(fun = function(x) sd(x, na.rm = TRUE))

