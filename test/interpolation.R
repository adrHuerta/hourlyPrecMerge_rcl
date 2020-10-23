
source("./src/RIDW.R")
require(stats)

#DATA IMERG EARLY 
sat_data <- readRDS("./data/processed/sat/sat_data.rds")
early_hr <- sat_data$early_hr
#DATA OBS 
obs_data <- readRDS("./data/processed/obs/obs_data_qc_v3.rds")


regressionIDW_mode1=list()
regressionIDW_mode2=list()
regressionIDW_mode3=list()

#probando con 100 horas 
for(i in 1:100){ 

# OBS
value <- t(zoo::coredata(obs_data$value[i])) 
obs_data$xyz$pp_obs =value

#average in a single grid
gauge <- mean_doble_Station(gauge = obs_data$xyz[9],cov = early_hr[[i]])
gauge@data[gauge@data == "NaN"] <-NA 

# SAT
sat <- early_hr[[i]]


#INTERPOLATION RIDW
regressionIDW_mode1[[i]] <- RIDW(gauge= gauge,
                                 cov = sat,
                                 formula = pp_obs ~ pp_sat,
                                 idpR = seq(0.8, 3.5, 0.1),
                                 norm = c("sqrt"))
sat1=early_hr[[i-1]]
regressionIDW_mode2[[i]] <- RIDW(gauge= gauge,
                                 cov = sat,
                                 cov2 = list(sat1),
                                 formula = pp_obs ~ pp_sat + sat_1,
                                 num_cov = "one", 
                                 idpR = seq(0.8, 3.5, 0.1),
                                 norm = c("sqrt"))

#INTERPOLATION IDW
}
