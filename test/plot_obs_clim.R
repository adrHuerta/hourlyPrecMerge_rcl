library(xts)
library(dplyr)

obs_data <- readRDS("./data/raw/obs/obs_data.rds")

#estaciones
border <- raster::shapefile("./data/shapes/DEPARTAMENTOS.shp")
basins <- raster::shapefile("./data/shapes/CHIRILU.shp")
r <- raster::raster("./data/shapes/chirilu_ProjectRaster.tif")

plot.a=spplot(r,col.regions =grey.colors(20), colorkey=FALSE, margin=F, xlab="Longitud(°)",ylab="Latitud(°)",
       sp.layout=list(list(border)),  main=list(label="(a)",cex=0.9),
       scales = list(y = list(tck=c(-1, -1)),x = list(tck=c(-1, -1))),
       xlim=c(-77.4,-76.0),ylim=c(-12.32,-11.0))+ 
       latticeExtra::layer(sp.points(obs_data$xyz,pch =19,lwd=3,col="blue"))+ 
       latticeExtra::layer(sp.polygons(basins)) 

#gráfico climático
path_raster="./data/shapes/pisco/"
ras=list.files(path=path_raster,pattern=".tif")
obs_data=readRDS('./data/processed/obs/obs_data_qc_v2.rds')
Pisco.prec.brick=stack(paste0(path_raster,ras))

order.alt.xyz =as.data.frame(obs_data$xyz) %>%
              arrange(ALT) 
obs_data$xyz = SpatialPointsDataFrame(coords=order.alt.xyz[,3:4],
               data=order.alt.xyz, proj4string=CRS(" +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") )         

point <- obs_data$xyz[,c("CODE","LAT","LON")]
df1 <- extract(Pisco.prec.brick, point, df=TRUE, method='simple')

df1 <- data.frame(t(df1[,c(-1)]))
noms <- obs_data$xyz$ESTACION
colnames(df1) <- noms
df1$factor=rep(1:12)

df1=reshape::melt(df1, id.vars=c('factor'),var='est')


month <- seq(as.Date("2020-01-01"),as.Date("2020-12-01"),by = "1 month")
month_label <- format(month, format = "%b")

my.par.setting=list(superpose.symbol = list(col = magenta2green(46),pch = 19), 
     superpose.line = list(col =magenta2green(46),lwd=1))

plot.b=xyplot(value~ factor,data=df1,groups = est, superpose = TRUE,auto.key = FALSE,type = c("b"),  
              par.settings = my.par.setting,    
              xlim =c(1,12),xlab=c("meses"),ylab =c("Precipitación(mm/mes)"), main=list(label="(b)",cex=0.9),
       scales = list(y = list(tck=c(-1, -1)),x = list(at=seq(1,12,1), labels=month_label,rot=45,tck=c(-1, -1))))

#plot hourly
obs_data$value=select(data.frame(coredata(obs_data$value)), obs_data$xyz$CODE)
data=obs_data$value[1:52584,-c(33,25,3,32)]
hora=seq(from=as.POSIXct("2014-01-01 01:00:00"),to=as.POSIXct("2020-01-01 00:00:00"), by="hour")
datafinal=xts::xts(data,order.by=hora)

datafinal$factor=rep(1:24,2191)
datados=as.data.frame(coredata(datafinal))

promediar= datados %>% group_by(factor) %>%
  summarise_each(funs(mean(.,na.rm=TRUE))) 

df2=reshape::melt(as.data.frame(promediar), id.vars=c('factor'),var='est')

labels=c("01:00","06:00","12:00","18:00","00:00")
plot.c=xyplot(value~ factor,data=df2,groups = est, superpose = TRUE,auto.key = FALSE,type = "b",pch=19,xlim =c(1,24),par.settings = my.par.setting,
       xlab=c("Horas"),ylab =c("Precipitación(mm/hr)"), main=list(label="(d)",cex=0.9),
       scales = list(y = list(tck=c(-1, -1)),x = list(at=c(1,6,12,18,24), labels=labels, tck=c(-1, -1))))
#plot boxplot

pp_humedo=df1 %>%
  arrange(factor)  %>%
  filter(factor<4 | factor>10)   

pp_humedo$tipo=rep("P.Húmedo",230)

pp_seco=df1 %>%
  arrange(factor)  %>%
  filter(factor<11 & factor>3)   

pp_seco$tipo=rep("P.Seco",322)
pp_montly=rbind(pp_seco,pp_humedo)

my.panel <- function(..., box.ratio) {
  panel.violin(..., col=c("gray"),border = FALSE,alpha = 0.8,
               varwidth = FALSE, box.ratio = box.ratio)
  panel.bwplot(..., col="red",
               cex=0.9, pch='|', 
               fill=c("mediumspringgreen","tomato1"), box.ratio = .25)
  panel.stripplot(..., col=c("blue"), alpha=0.5,
                  jitter.data = TRUE, pch = 19, cex=0.5)
}
# Violin plot

plot.d=bwplot(value ~ tipo,  data = pp_montly, ylab=c("Precipitación(mm/mes)"),
             panel=my.panel, main=list(label="(c)",cex=0.9),
              par.settings = list(box.rectangle=list(col='black'),
                                  plot.symbol = list(pch='.', cex = 0.1)),
             scales=list(x=list( cex=0.9)))

#width=900,height=650 , res = 150
png("./data/output/plots/plot_obs_clim.png")

gridExtra::grid.arrange(plot.a,plot.b,plot.d,plot.c,ncol=2,nrow=2)

dev.off()
