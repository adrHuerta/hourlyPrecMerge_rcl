rm(list = ls())

#plot temporal Coeficient Correlations
metrics=readRDS("./data/output/metrics.rds")
metrics=metrics$spatial

#read time series
Time  <- row.names(metrics)
Time <- as.POSIXct(Time)
ts_cor <- xts::xts(metrics$corr,order.by =Time)

n_hours1=length(ts_cor["2014-11/2015-03"])
n_hours2=length(ts_cor["2015-11/2016-03"])


# joins df
ts_cor=data.frame(zoo::coredata(ts_cor))
ts_cor$index = c(1:n_hours1,1:n_hours2,rep (1:n_hours1,3))
labels=c("2014-2015","2015-2016",
         "2016-2017","2017-2018","2018-2019")
ts_cor$period<-rep(labels,c(n_hours1,n_hours2,rep(n_hours1,3))) 
colnames(ts_cor)<-c("cor","index","period")

# remove cor<0
ts_cor[ts_cor< 0] <- NA

# plot
png("./data/output/plots/plot_ts_cor.png",width=900,height=800, res = 110)

lattice::xyplot(cor ~ index| period, ts_cor, layout=c(1,5),
                ylab =c("Coeficiente de Correlación"),xlab=c("Horas"),
                grid = TRUE, group = period,type = c("l"),
                par.settings = list(strip.background=list(col="gray")),
                scales = list(x = list(at=seq(1,n_hours2,720),
                                       labels=c("Nov","Dic","Ene","feb","Mar","Abr"))))

dev.off()

