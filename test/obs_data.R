# agregando script

library(xts)

obs <- read.csv("./data/raw/obs/DR_Lima_QC1.csv")
head(obs)
matplot(obs[,2])
