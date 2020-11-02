rm(list = ls())
require(xts)
"%>%" = magrittr::`%>%`

source('./src/calculate_metrics2.R')

#read data
data_merging <-readRDS("./data/processed/merging/merging_grid_CV.RDS")
data_obs <-readRDS("./data/processed/obs/obs_data_qc_v4.rds")
obs <- data_obs$value

# events
obs_E1 <- obs["2015-02-09 10:00:00/2015-02-10 07:00:00"]
obs_E2 <- obs["2015-03-20 11:00:00/2015-03-22 03:00:00"]
obs_E3 <- obs["2016-02-25 11:00:00/2016-02-25 23:00:00"]
obs_E4 <- obs["2016-02-28 13:00:00/2016-02-29 10:00:00"]
obs_E5 <- obs["2017-03-15 13:00:00/2017-03-16 03:00:00"]
obs_E6 <- obs["2017-03-18 02:00:00/2017-03-20 03:00:00"]
obs_E7 <- obs["2018-01-25 08:00:00/2018-01-26 02:00:00"]
obs_E8 <- obs["2019-01-27 12:00:00/2019-01-30 23:00:00"]

data_obs_events<-list(obs_E1,obs_E2,obs_E3,obs_E4,
                      obs_E5,obs_E6,obs_E7,obs_E8)

# 
#i=Method
#j=evets

temporal <-rep(list(list()), 11)
spatial <-rep(list(list()), 11)
for (i in 1:11) {
  for (j in 1:8) {
    
    obs_select <- data.frame(data_obs_events[[j]]) %>%
                   dplyr::select(contains(colnames(data_merging[[i]][[j]])))

    spatial[[i]][[j]]<- calculate_metrics(obs = obs_select,
                                          sat = data.frame(data_merging[[i]][[j]]),
                                          type_of_validation = "spatial")

    temporal[[i]][[j]]<- calculate_metrics(obs = obs_select,
                                           sat = data.frame(data_merging[[i]][[j]]),
                                           type_of_validation="temporal")
  }
}

saveRDS(object = list(metrics_spatial=spatial,
                      metrics_temporal=temporal),
          file = "./data/output/metrics_cv.rds")

