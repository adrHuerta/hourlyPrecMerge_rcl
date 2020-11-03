rm(list = ls())
"%>%" = magrittr::`%>%`

library(xts)
library(raster)

source("./src/interpolation.R")
source("./src/points_inside_pixel.R")

obs <- readRDS("./data/processed/obs/obs_data_qc_v2.rds")
sat <- readRDS("./data/processed/sat/sat_data.rds")$early

### 1.-Remover de EMAs no adecuadas
# data obs 2014-2019
obs$value <- obs$value["2014-01/2019-12"]

# remove ID's (no area CHIRILU)
df_obs <- data.frame(obs$value)
df_obs <- dplyr::select(df_obs,-ID472364DC,-ID110137,-ID47257764)

# remove by climatology montly
df_obs <- dplyr::select(df_obs,-ID472542FE,-ID47A0A5A6,-ID4721725E)

# remove by climatology hourly
df_obs <- dplyr::select(df_obs,-ID4726B574)

# remove by QC
df_obs <- dplyr::select(df_obs ,-ID472A0766,-ID47E33064)


# xts to spdf
obs$value <- xts::xts(df_obs, order.by = time(obs$value))

df_xyz <- data.frame(obs$xyz)
df_xyz<- dplyr::filter(df_xyz, CODE %in% colnames(obs$value))

obs$xyz <- sp::SpatialPointsDataFrame(coords = df_xyz[,3:4],
                                      data = df_xyz,
                                      proj4string = sp::CRS(raster::projection(sat[[1]]))
                                      )


### 2.- Punto-pixel
# getting single points 
new_xyz <- make_single_point(pts = obs$xyz, rgrid = sat)$xyz

# points to be merged
id_to_merge <- make_single_point(pts = obs$xyz, rgrid = sat)$no_single_points

# single ts
new_value <- obs$value[, -match(unlist(id_to_merge), colnames(obs$value))]

# merging no-single ts
id_to_merge <- id_to_merge %>%
  setNames(id_to_merge %>% lapply(function(x) paste(x, collapse = "_"))) %>%
  lapply(function(x){
    res <- obs$value[, x] %>% apply(1, mean, na.rm = TRUE)
    res[is.nan(res)] <- NA
    res
  }) %>%
  do.call("cbind", .)

id_to_merge <- xts::xts(id_to_merge, time(obs$value))

# single + new single ts
new_value <- cbind(new_value, id_to_merge)


obs <- list(value = round(new_value, 1),
            xyz = new_xyz)

# time series of non_NA values

obs$value %>%
  apply(1, function(x) sum(!is.na(x))) -> ts_non_na
ts_non_na <- xts::xts(ts_non_na, time(obs$value))
ts_non_na["2014-11-01-00/2014-11-01-23"] %>% plot(type = "p")

time_ini <- which(as.character(time(obs$value)) =="2014-11-01 00:00:00")

### 3.-Producto grillado CHIRILU

parallel::mclapply(time_ini:nrow(obs$value),
                   function(i){
                     
                     data_obs <- obs$value[i, ]
                     data_xyz <- obs$xyz
                     data_xyz$OBS = as.numeric(data_obs)
                     
                     label_date <- format(time(data_obs), "%Y-%m-%d-%H")
                     
                     data_obs <- data_xyz %>% .[complete.cases(.@data),]
                     colnames(data_obs@data)[9] <- "obs"
                     
                     cov_sat <- sat[[i:c(i-1)]]
                     cov_sat[[1]][cov_sat[[1]] < 0] <- 0
                     cov_sat[[2]][cov_sat[[2]] < 0] <- 0
                     
                     if( length(data_obs@data$obs) < 10) {
                       
                       chirilu_gridded <- cov_sat[[1]]
                       raster::values(chirilu_gridded) <- NA
                       
                     } else {
                       
                       # Mezcla
                       IDW_res <- IDW(gauge_points = data_obs, gridded_cov = cov_sat[[1]])
                       OK_res <- OK(gauge_points = data_obs, gridded_cov = cov_sat[[1]])
                       RIDW_res <- RIDW(gauge_points = data_obs, gridded_cov = cov_sat[[1]])
                       RIDW2_res <- RIDW(gauge_points = data_obs, gridded_cov = cov_sat)
                       CM_IDW_res <- CM_IDW(gauge_points = data_obs, gridded_cov = cov_sat[[1]])
                       RK_res <- RK(gauge_points = data_obs, gridded_cov = cov_sat[[1]])
                       RK2_res <- RK(gauge_points = data_obs, gridded_cov = cov_sat)
                       CM_OK_res <- CM_OK(gauge_points = data_obs, gridded_cov = cov_sat[[1]])
                       
                       chirilu_gridded <- raster::brick(IDW_res, OK_res,
                                                        RIDW_res, RIDW2_res, CM_IDW_res,
                                                        RK_res, RK2_res, CM_OK_res) %>%
                         raster::calc(fun = function(x) median(x, na.rm = TRUE))
                       
                       chirilu_gridded <- raster::setZ(chirilu_gridded, raster::getZ(cov_sat[[1]]))
                       names(chirilu_gridded) <- names(cov_sat[[1]])
                       
                     }
                     
                     raster::writeRaster(chirilu_gridded,
                                         file = file.path(".", "data", "processed", "merging", "chiriluV2", paste0("chiriluV2_", label_date,".nc")),
                                         overwrite = TRUE,
                                         format = "CDF",
                                         varname = "precp")
                    
                   },
                   mc.cores = 5)
