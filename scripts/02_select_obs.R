rm(list = ls())
require(xts)

# selection of stations observed

#obs_data$value
obs_data <- readRDS("./data/processed/obs/obs_data_qc_v2.rds")

# remove ID's Out of limit
df_obs <- data.frame(obs_data$value)
obs_data_select <- dplyr::select(df_obs,-ID472364DC,-ID110137,-ID47257764)

# remove by climatology montly
obs_data_select <- dplyr::select(obs_data_select,-ID472542FE,-ID47A0A5A6,-ID4721725E)

# remove by climatology hourly
obs_data_select <- dplyr::select(obs_data_select,-ID4726B574)

# remove by QC
obs_data_select  <- dplyr::select(obs_data_select,-ID472A0766,-ID47E33064)

#to object xts 
obs_data_select <- xts(obs_data_select,order.by = index(obs_data$value))

#selected periods
ts1=obs_data_select["2014-11/2015-03"]
ts2=obs_data_select["2015-11/2016-03"]
ts3=obs_data_select["2016-11/2017-03"]
ts4=obs_data_select["2017-11/2018-03"]
ts5=obs_data_select["2018-11/2019-03"]

#obs_data$value final
obs_data$value <- rbind(ts1,ts2,ts3,ts4,ts5)

#obs_data$xyz final 
df_xyz <- data.frame(obs_data$xyz)
df_xyz<- dplyr::filter(df_xyz, CODE %in% colnames(obs_data$value))

obs_data$xyz <- sp::SpatialPointsDataFrame(coords=df_xyz[,3:4],
                data=df_xyz, proj4string=sp::CRS(" +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") )
#save
saveRDS(obs_data, 
        file = "./data/processed/obs/obs_data_qc_v3.rds")


