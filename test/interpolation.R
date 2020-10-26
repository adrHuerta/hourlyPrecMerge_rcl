"%>%" = magrittr::`%>%`

source("./test/RIDW_test.R")

interp_methods <- function(events, obs_data, early_hr , DEM) {
  
                 #select obs in event
                 obs <- obs_data$value[events]
                 #select sat in event
                 dates_labels <- as.character(zoo::index(obs)) %>%
                                 stringr::str_replace_all(":" , ".") %>%
                                 stringr::str_replace_all("-" , ".") %>%
                                 stringr::str_replace_all(" " , ".")

                 early_hr <- early_hr[[paste0("X",dates_labels)]]
                
                 # interpolation RIDW
                 regressionIDW_mode1=list()
                 regressionIDW_mode2=list()
                 regressionIDW_mode3=list()
                 for(i in 3:nrow(obs)){ 

                 # OBS
                 value <- t(zoo::coredata(obs[i])) 
                 obs_data$xyz$pp_obs =value

                 #average in a single grid
                 gauge <- Dorado::mean_doble_Station(gauge = obs_data$xyz[6],
                                                    cov = early_hr[[i]])
                 gauge@data[gauge@data == "NaN"] <-NA 


                 #INTERPOLATION RIDW
                 sat <- early_hr[[i]]
                 regressionIDW_mode1[[i]] <- RIDW(gauge= gauge,
                                                 cov = sat,
                                                 cov2= 0,
                                                 formula = pp_obs ~ pp_sat,
                                                 num_cov = "none",
                                                 idpR = seq(0.8, 3.5, 0.1),
                                                 norm = c("sqrt"))
                 sat1 <- early_hr[[i-1]]
                 regressionIDW_mode2[[i]] <- RIDW(gauge= gauge,
                                                 cov = sat,
                                                 cov2 = list(sat1),
                                                 formula = pp_obs ~ pp_sat + sat_1,
                                                 num_cov = "one_hour_before", 
                                                 idpR = seq(0.8, 3.5, 0.1),
                                                 norm = c("sqrt"))

                 sat2 <- early_hr[[i-2]]
                 regressionIDW_mode3[[i]] <- RIDW(gauge= gauge,
                                                cov = sat,
                                                cov2 = list(sat1,sat2, DEM),
                                                formula = pp_obs ~ pp_sat + sat_1 + sat_2 + elev,
                                                num_cov = "two_hours_before", 
                                                idpR = seq(0.8, 3.5, 0.1),
                                                norm = c("sqrt"))


                 }
                 #return
                 ridw_mode1 <- raster::stack(regressionIDW_mode1[3:nrow(obs)])
                 ridw_mode2 <- raster::stack(regressionIDW_mode2[3:nrow(obs)])
                 ridw_mode3 <- raster::stack(regressionIDW_mode3[3:nrow(obs)])
                 return(list(RIDW_mode1=ridw_mode1,
                             RIDW_mode2=ridw_mode2,
                             RIDW_mode3=ridw_mode3))
              }



