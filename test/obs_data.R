obs_data <- readRDS("./data/raw/obs/obs_data.rds")
plot(obs_data$value["2019-02-15/2019-02-16"])
obs_data$value["2019-02-15 19:00:00"]

# deleting negative values
# extremes values
# spatial coherence

obs_data$xyz@data$value <- as.numeric(obs_data$value["2019-02-15 21:00:00"])
sp::spplot(obs_data$xyz[,"value"])
