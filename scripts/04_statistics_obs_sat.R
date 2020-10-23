rm(list = ls())
require(xts)

rm(list = ls())
source('./src/calculate_metrics.R')

#read data
data_sat <-readRDS("./data/processed/sat/sat_data2.rds")
data_obs <-readRDS("./data/processed/obs/obs_data_qc_v3.rds")

# type of validation
metrics_spatial <- calculate_metrics(obs=data_obs$value,
                                     sat=data_sat$value,
                                     type_of_validation="spatial")

metrics_temporal<- calculate_metrics(obs=data_obs$value,
                                     sat=data_sat$value,
                                     type_of_validation="temporal")
 
rownames(metrics_spatial) <- index(data_obs$value)
#save
saveRDS(object=list(spatial = metrics_spatial, 
                    temporal = metrics_temporal), 
        file = "./data/output/metrics.rds")

