rm(list = ls())
"%>%" = magrittr::`%>%`

library(xts)
library(raster)

obs <- readRDS("./data/processed/obs/obs_data_qc_v4.rds")
sat <-readRDS("./data/processed/sat/sat_data2.rds")

# example event 

E3 <- obs$value["2016-02-25 11:00:00/2016-02-25 23:00:00"]
E3 <- E3[, colSums(is.na(E3)) != nrow(E3)]

obs$xyz@data[match(colnames(E3), obs$xyz$CODE), ] %>% dim()
