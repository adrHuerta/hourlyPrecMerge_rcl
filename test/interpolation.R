rm(list = ls())
"%>%" = magrittr::`%>%`

source("./test/RIDW_test.R")

#DATA IMERG EARLY 
sat_data <- readRDS("./data/processed/sat/sat_data2.rds")
early_hr <- sat_data$early_hr
#DATA OBS 
obs_data <- readRDS("./data/processed/obs/obs_data_qc_v3.rds")


RIDW_events <- function(events, obs_data, early_hr ) {
  
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
                 gauge <- Dorado::mean_doble_Station(gauge = obs_data$xyz[9],
                                                    cov = early_hr[[i]])
                 gauge@data[gauge@data == "NaN"] <-NA 


                 #INTERPOLATION RIDW
                 sat <- early_hr[[i]]
                 regressionIDW_mode1[[i]] <- RIDW(gauge= gauge,
                                                 cov = sat,
                                                 cov2=0,
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
                                                cov2 = list(sat1,sat2),
                                                formula = pp_obs ~ pp_sat + sat_1 + sat_2,
                                                num_cov = "two_hours_before", 
                                                idpR = seq(0.8, 3.5, 0.1),
                                                norm = c("sqrt"))


                 }
                 #return
                 return(regressionIDW_mode1, regressionIDW_mode2, regressionIDW_mode3)
              }


event1 <- RIDW_events(events="2015-02-09 07:00/2015-02-10 07:00",
                      obs_data=obs_data, 
                      early_hr=early_hr)




# events <- c("2015-02-09 10:00/2015-02-10 07:00",
#             "2015-03-20 11:00/2015-03-22 03:00",
#             "2016-02-25 11:00/2016-02-25 23:00",
#             "2016-02-28 13:00/2016-02-29 10:00",
#             "2017-03-15 13:00/2017-03-16 03:00",
#             "2017-03-18 02:00/2017-03-20 03:00",
#             "2018-01-25 08:00/2018-01-26 02:00",
#             "2019-02-21 12:00/2019-02-23 03:00")