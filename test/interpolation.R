rm(list = ls())

source("./test/RIDW_test.R")
#require(stats)

#DATA IMERG EARLY 
sat_data <- readRDS("./data/processed/sat/sat_data2.rds")
early_hr <- sat_data$early_hr
#DATA OBS 
obs_data <- readRDS("./data/processed/obs/obs_data_qc_v3.rds")


regressionIDW_mode1=list()
regressionIDW_mode2=list()
regressionIDW_mode3=list()

#testing
for(i in 3:20){ 

# OBS
value <- t(zoo::coredata(obs_data$value[i])) 
obs_data$xyz$pp_obs =value

#average in a single grid
gauge <- Dorado::mean_doble_Station(gauge = obs_data$xyz[9],cov = early_hr[[i]])
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
regressionIDW_mode2[[i]] <- RIDW(gauge= gauge,
                                 cov = sat,
                                 cov2 = list(sat1,sat2),
                                 formula = pp_obs ~ pp_sat + sat_1 + sat_2,
                                 num_cov = "two_hours_before", 
                                 idpR = seq(0.8, 3.5, 0.1),
                                 norm = c("sqrt"))

#INTERPOLATION IDW
}
