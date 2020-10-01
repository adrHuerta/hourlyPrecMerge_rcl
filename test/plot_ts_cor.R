#plot temporal Coeficient Correlations
metrics=readRDS("./data/output/metrics.rds")
metrics=metrics$spatial

#read time series
Time  <- seq(as.POSIXct("2014-01-01 01:00"), as.POSIXct("2020-01-01 00:00"), by='hour')
ntime <- length(Time)
ts_cor <- xts::xts(metrics$corr,order.by =Time)

#select time series
xts1=list()
for (i in 1:5) {
  years= as.character(2014:2019)
  xts1[[i]] <- ts_cor[paste0(years[i],"-11/",years[i+1],"-03")]
  
 }

#modify year 2016, period 2015-2016
ExcludeDates <- function(x, exclude) {
  idx <- index(x)
  x[idx[!format(idx, "%Y-%m-%d") %in% paste(exclude)]]
}
xts1[[2]]<-ExcludeDates(xts1[[2]],"2016-02-29")

# length hours
n_hours=length(xts1[[2]])

# joins df
ts_cor=data.frame(cor=unlist(xts1))
ts_cor$index = rep (1:n_hours,5)
labels=c("2014-2015","2015-2016",
         "2016-2017","2017-2018","2018-2019")
ts_cor$period = rep(labels,each=n_hours)

# remove cor<0
ts_cor[ts_cor< 0] <- NA

# plot
png("./data/output/plots/plot_ts_cor.png",width=900,height=800, res = 110)

lattice::xyplot(cor ~ index| period, ts_cor, layout=c(1,5),
                ylab =c("Coeficiente de Correlación"),xlab=c("Horas"),
                grid = TRUE, group = period,type = c("l"),
                par.settings = list(strip.background=list(col="gray")),
                scales = list(x = list(at=seq(1,n_hours,720),
                labels=c("Nov","Dic","Ene","feb","Mar","Abr"))))

dev.off()

