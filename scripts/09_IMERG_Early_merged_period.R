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
source("./src/points_inside_pixel.R")

obs <- readRDS("./data/processed/obs/obs_data_qc_v2.rds")
sat <-readRDS("./data/processed/sat/sat_data.rds")$early


#data obs 2014-2019
obs$value <- obs$value["2014-01/2019-12"]

# remove ID's Out of limit
df_obs <- data.frame(obs$value)
df_obs<- dplyr::select(df_obs,-ID472364DC,-ID110137,-ID47257764)

#to object xts and spdf
obs$value <- xts::xts(df_obs,order.by = index(obs$value))

df_xyz <- data.frame(obs$xyz)
df_xyz<- dplyr::filter(df_xyz, CODE %in% colnames(obs$value))

obs$xyz <- sp::SpatialPointsDataFrame(coords=df_xyz[,3:4],
                                           data=df_xyz,
                                           proj4string=sp::CRS(" +proj=longlat +datum=WGS84 +no_defs 
                                                               +ellps=WGS84 +towgs84=0,0,0") )


#reproyectar  
raster::projection(obs$xyz) <- raster::projection(sat)

#merging 2014-2019

for(i in 2:nrow(obs$value)){
  
  data_obs <- obs$value[i,]
  data_xyz <- obs$xyz
  data_xyz$OBS=as.numeric(data_obs)
  
  label_date <-gsub(":", ".",as.character(index(data_obs)))
  
  # EMA-grilla
  data_obs<- make_single_point(pts = data_xyz,
                                rgrid = sat[[i]])$xyz %>%
    .[complete.cases(.@data),]
  
  colnames(data_obs@data)[9] <- "obs"
  
  
  
  
  if(sum(is.na(obs$value[i,]))==ncol(obs$value)){

    chirilu_gridded = raster::raster(nrow=13, ncol=14)
    
    raster::writeRaster(chirilu_gridded, 
                        file=paste0("./data/processed/merging/IMERG-Early_merging_",label_date,".tiff"),overwrite=TRUE)
    
    
  } else  {
    
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
                        file=paste0("./data/processed/merging/IMERG-Early_merging_",label_date,".tiff"),overwrite=TRUE)
    
    }
  
}



  
  
  








