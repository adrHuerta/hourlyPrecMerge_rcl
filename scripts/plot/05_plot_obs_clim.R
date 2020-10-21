rm(list = ls())

require(xts)
"%>%" = magrittr::`%>%`

####PLOT STATIONS IN CHIRILU
obs_data <- readRDS("./data/processed/obs/obs_data_qc_v3.rds")
sat_data <- readRDS("./data/processed/sat/sat_data.rds")

#shp
r <- raster::raster("./data/shapes/chirilu_ProjectRaster.tif")
border <- raster::shapefile("./data/shapes/DEPARTAMENTOS.shp")
basins <- raster::shapefile("./data/shapes/CHIRILU.shp")


plot.a <- sp::spplot(r,col.regions =grey.colors(20), colorkey=FALSE,
                     margin=F, xlab="Longitud(°)",ylab="Latitud(°)",
                     main=list(label="(a)",cex=0.9),
                     sp.layout=list(list(border)),  
                     scales = list(y = list(tck=c(-1, -1)),
                                   x = list(tck=c(-1, -1))),
                     xlim=c(-77.45,-75.86),ylim=c(-12.3,-10.95))+ 
  latticeExtra::layer(sp::sp.points(obs_data$xyz,
                                    pch =19,lwd=3,col="blue"))+ 
  latticeExtra::layer(sp::sp.polygons(basins)) 

####PLOT PISCO CLIMATOLOGY
monthly.pisco <- list.files(path="./data/shapes/pisco/",pattern=".tif")
Pisco.prec.brick <- raster::stack(paste0("./data/shapes/pisco/",monthly.pisco))

#select data in ALT
order.alt.xyz <- as.data.frame(obs_data$xyz) %>%
  dplyr::arrange(ALT)

obs_data$xyz <- sp::SpatialPointsDataFrame(coords=order.alt.xyz[,3:4],data=order.alt.xyz, 
                                           proj4string=sp::CRS(" +proj=longlat +datum=WGS84 
                                                      +no_defs +ellps=WGS84 +towgs84=0,0,0"))         
#extract point
point <- obs_data$xyz[,c("CODE","LAT","LON")]

pisco_extract <- raster::extract(Pisco.prec.brick, point, df=TRUE, method='simple')
pisco_extract <- data.frame(t(pisco_extract[,c(-1)]))

#labels months as factor
noms <- obs_data$xyz$ESTACION
colnames(pisco_extract) <- noms
pisco_extract$factor <- rep(1:12)
pisco_extract <- reshape::melt(pisco_extract, id.vars=c('factor'),var='est')

month_label <- format(seq(as.Date("2020-01-01"),as.Date("2020-12-01"),
                          by = "1 month"), format = "%b")

#ramps color
my.par.setting <- list(superpose.symbol = list(col = colorRamps::magenta2green(46),pch =19), 
                       superpose.line = list(col = colorRamps::magenta2green(46),lwd=1))

plot.b <- lattice::xyplot(value~ factor,data=pisco_extract,
                          groups = est,superpose = TRUE,auto.key = FALSE,
                          type =c("b"),par.settings = my.par.setting, 
                          main =list(label="(b)",cex=0.9),   
                          xlim =c(1,12),xlab=c("meses"),ylab =c("Precipitación(mm/mes)"),
                          scales = list(y = list(tck=c(-1, -1)),
                                        x = list(at=seq(1,12,1), 
                                                 labels=month_label,
                                                 rot=45,tck=c(-1, -1))))


####PLOT DATA EARLY
n_hour <- nrow(obs_data$value)/24
sat_data$value$factor <- rep(1:24,n_hour)
data_early <- data.frame(sat_data$value)

# extract negative values
data_early[data_early < 0] <- NA

early_group <- data_early %>% 
  dplyr::group_by(factor) %>%
  dplyr::summarise_each(dplyr::funs(mean(.,na.rm=TRUE))) 

early_group <- reshape::melt(as.data.frame(early_group), 
                             id.vars=c('factor'),var='est')

labels <- c("01:00","06:00","12:00","18:00","00:00")

plot.c <- lattice::xyplot(value~ factor,data=early_group,groups = est, 
                          superpose = TRUE,auto.key = FALSE,type = "b",
                          pch=19,xlim =c(1,24),par.settings = my.par.setting,
                          xlab=c("Horas"),ylab =c("Precipitación(mm/hr)"), 
                          main=list(label="(c)",cex=0.9),
                          scales = list(y = list(tck=c(-1, -1)),
                                        x = list(at=c(1,6,12,18,24), 
                                                 labels=labels, tck=c(-1, -1))))
####PLOT DATA HOURLY
obs_data$value$factor <- rep(1:24,n_hour)
data_obs <- as.data.frame(obs_data$value)

obs_group <- data_obs %>% 
  dplyr::group_by(factor) %>%
  dplyr::summarise_each(dplyr::funs(mean(.,na.rm=TRUE))) 

obs_group <- reshape::melt(as.data.frame(obs_group), 
                           id.vars=c('factor'),var='est')

plot.d <- lattice::xyplot(value~ factor,data=obs_group,groups = est, 
                          superpose = TRUE,auto.key = FALSE,type = "b",
                          pch=19,xlim =c(1,24),par.settings = my.par.setting,
                          xlab=c("Horas"),ylab =c("Precipitación(mm/hr)"), 
                          main=list(label="(d)",cex=0.9),
                          scales = list(y = list(tck=c(-1, -1)),
                                        x = list(at=c(1,6,12,18,24), 
                                                 labels=labels, tck=c(-1, -1))))
#plot
png("./data/output/plots/plot_obs_clim.png",width=900,height=850, res = 100)

gridExtra::grid.arrange(plot.a,plot.b,
                        plot.d,plot.c,
                        ncol=2,nrow=2)

dev.off()
