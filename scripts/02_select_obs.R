rm(list = ls())
"%>%" = magrittr::`%>%`

# selection of stations observed
# detalla mejor, define la connotación paquete::funcion, que no se entiende muy bien 

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
obs_data$value <- xts::xts(obs_data_select,order.by = index(obs_data$value))

#obs_data$xyz
df_xyz <- data.frame(obs_data$xyz)
df_xyz<- dplyr::filter(df_xyz, CODE %in% colnames(obs_data$value))

obs_data$xyz <- sp::SpatialPointsDataFrame(coords=df_xyz[,3:4],
                data=df_xyz, proj4string=sp::CRS(" +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") )

saveRDS(obs_data, file = "./data/processed/obs/obs_data_qc_v3.rds")