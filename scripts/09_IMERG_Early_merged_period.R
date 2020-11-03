## IMERG_Early_merged_period.R
# Merged of observed and GPM IMERG Early, data median of the methods applied 
# to improve the satellite estimation
# 

rm(list = ls())
"%>%" = magrittr::`%>%`

library(xts)
library(raster)

source("./src/interpolation.R")
source("./src/points_inside_pixel.R")

obs <- readRDS("./data/processed/obs/obs_data_qc_v2.rds")
sat <-readRDS("./data/processed/sat/sat_data.rds")$early

### 1.-Remover estaciones que no adecuadas
#data obs 2014-2019
obs$value <- obs$value["2014-01/2019-12"]

# remove ID's Out of limit
df_obs <- data.frame(obs$value)
df_obs<- dplyr::select(df_obs,-ID472364DC,-ID110137,-ID47257764)

# remove by climatology montly
df_obs<- dplyr::select(df_obs,-ID472542FE,-ID47A0A5A6,-ID4721725E)

# remove by climatology hourly
df_obs <- dplyr::select(df_obs,-ID4726B574)

# remove by QC
df_obs <- dplyr::select(df_obs ,-ID472A0766,-ID47E33064)


#to object xts and spdf
obs$value <- xts::xts(df_obs,order.by = index(obs$value))

df_xyz <- data.frame(obs$xyz)
df_xyz<- dplyr::filter(df_xyz, CODE %in% colnames(obs$value))

obs$xyz <- sp::SpatialPointsDataFrame(coords=df_xyz[,3:4],
                                           data=df_xyz,
                                           proj4string=sp::CRS(" +proj=longlat +datum=WGS84 +no_defs 
                                                               +ellps=WGS84 +towgs84=0,0,0") )

### 2.-Una sola estación por pixel
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


obs <-list(value = round(new_value, 1),
             xyz = new_xyz)

# date<-as.character(index(obs$value))
# which(date=="2015-01-01 00:00:00")
time_ini <-  8760

### 3.-Producto grillado CHIRILU

for(i in time_ini:nrow(obs$value)){
  
  data_obs <- obs$value[i,]
  data_xyz <- obs$xyz
  data_xyz$OBS=as.numeric(data_obs)
  
  label_date <-gsub(":", ".",as.character(index(data_obs)))
  
 
  data_obs<- data_xyz %>%
    .[complete.cases(.@data),]
  
  colnames(data_obs@data)[9] <- "obs"
  

  if(sum(is.na(obs$value[i,]))==ncol(obs$value)){
    
    chirilu_gridded = NA
    
  } else if(sum(is.na(obs$value[i,])) > 9 ) {
    
    chirilu_gridded = NA
  
  } else if(sum(is.na(obs$value[i,])) < 9 ){
    
    # Mezcla
    IDW_res<- IDW(gauge_points = data_obs, gridded_cov = sat[[i]])
    OK_res <- OK(gauge_points = data_obs, gridded_cov = sat[[i]])
    RIDW_res <- RIDW(gauge_points = data_obs, gridded_cov = sat[[i]])
    RIDW2_res <- RIDW(gauge_points = data_obs, gridded_cov = sat[[c(i-1):i]])
    CM_IDW_res <- CM_IDW(gauge_points = data_obs, gridded_cov = sat[[i]])
    RK_res <- RK(gauge_points = data_obs, gridded_cov = sat[[i]])
    RK2_res <- RK(gauge_points = data_obs, gridded_cov = sat[[c(i-1):i]])
    CM_OK_res <- CM_OK(gauge_points = data_obs, gridded_cov = sat[[i]])
    
    chirilu_gridded<- raster::brick(IDW_res, OK_res,
                                    RIDW_res, RIDW2_res, CM_IDW_res,
                                    RK_res, RK2_res, CM_OK_res) %>%
    raster::calc(fun = function(x) median(x, na.rm = TRUE))
    
    
    
    raster::writeRaster(chirilu_gridded, 
                        file=paste0("./data/processed/merging/Chirilu.v2",label_date,".tiff"),overwrite=TRUE)
    
  }
  
}







