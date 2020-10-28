rm(list = ls())

#DATA IMERG EARLY 
sat_data <- readRDS("./data/processed/sat/sat_data2.rds")
early_hr <- sat_data$early_hr
#DATA OBS 
obs_data <- readRDS("./data/processed/obs/obs_data_qc_v3.rds")

#DEM de elevación 
DEM_CHIRILU<- raster::raster("./data/shapes/DEM_CHIRILU.tif")

ridw_model_1<-matrix(nrow=250,ncol=37)
ridw_model_2<-matrix(nrow=250,ncol=37)
ridw_model_3<-matrix(nrow=250,ncol=37)

for (i in 1:ncol(obs_data$value)) {
  obs_value= obs_data$value[,-i]
  
  obs_xyz=data.frame(obs_data$xyz)[-i,]
  obs_xyz_spdf <- sp::SpatialPointsDataFrame(coords=cbind(obs_xyz$LON,obs_xyz$LAT), obs_xyz,
                                             proj4string =sp::CRS("+proj=longlat +ellps=WGS84 
                                                                       +towgs84=0,0,0,0,0,0,0 +no_defs") )
  obs_xyz <- obs_xyz_spdf[,1:5]
  
  obs_data2=list(value=obs_value,xyz=obs_xyz)
  
  event1 <- RIDW_events(events="2015-02-09 10:00/2015-02-10 07:00",obs_data=obs_data2, 
                        early_hr=early_hr,DEM = DEM_CHIRILU)
  event2 <- RIDW_events(events="2015-03-20 11:00/2015-03-22 03:00", obs_data=obs_data2, 
                        early_hr=early_hr,DEM = DEM_CHIRILU)
  event3 <- RIDW_events(events="2016-02-25 11:00/2016-02-25 23:00",obs_data=obs_data2, 
                        early_hr=early_hr,DEM = DEM_CHIRILU)
  event4 <- RIDW_events(events="2016-02-28 13:00/2016-02-29 10:00",obs_data=obs_data2, 
                        early_hr=early_hr, DEM = DEM_CHIRILU)
  event5 <- RIDW_events(events="2017-03-15 13:00/2017-03-16 03:00",obs_data=obs_data2, 
                        early_hr=early_hr,DEM = DEM_CHIRILU)
  event6 <- RIDW_events(events="2017-03-18 02:00/2017-03-20 03:00", obs_data=obs_data2, 
                        early_hr=early_hr,DEM = DEM_CHIRILU)
  event7 <- RIDW_events(events="2018-01-25 08:00/2018-01-26 02:00", obs_data=obs_data2, 
                        early_hr=early_hr,DEM = DEM_CHIRILU)
  event8 <- RIDW_events(events="2019-01-27 12:00/2019-01-30 23:00",obs_data=obs_data2, 
                        early_hr=early_hr,DEM = DEM_CHIRILU)
  
  #juntar todos los eventos interpolados en el modelo 1 ridw en un solo brick 
  
  ridw_model_1_all_events<- raster::stack(event1$RIDW_mode1,event2$RIDW_mode1,
                                          event3$RIDW_mode1,event4$RIDW_mode1,
                                          event5$RIDW_mode1,event6$RIDW_mode1,
                                          event7$RIDW_mode1,event8$RIDW_mode1)
  
  ridw_model_2_all_events<- raster::stack(event1$RIDW_mode2,event2$RIDW_mode2,
                                          event3$RIDW_mode2,event4$RIDW_mode2,
                                          event5$RIDW_mode2,event6$RIDW_mode2,
                                          event7$RIDW_mode2,event8$RIDW_mode2)
  
  ridw_model_3_all_events<- raster::stack(event1$RIDW_mode3,event2$RIDW_mode3,
                                          event3$RIDW_mode3,event4$RIDW_mode3,
                                          event5$RIDW_mode3,event6$RIDW_mode3,
                                          event7$RIDW_mode3,event8$RIDW_mode3)
  
  #extraer los valores de LOOCV
  ext_model1<-raster::extract(ridw_model_1_all_events,obs_data$xyz[i,])
  ext_model2<-raster::extract(ridw_model_2_all_events,obs_data$xyz[i,])
  ext_model3<-raster::extract(ridw_model_3_all_events,obs_data$xyz[i,])
  ridw_model_1[,i] <-  ext_model1
  ridw_model_2[,i] <-  ext_model2
  ridw_model_3[,i] <-  ext_model3
}

saveRDS(list(ridw_model1=ridw_model_1,
             ridw_model2=ridw_model_2,
             ridw_model3=ridw_model_3) ,file = "./data/processed/loocv_intp_mtds.rds" )
