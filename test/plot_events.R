rm(list = ls())
require(xts)
"%>%" = magrittr::`%>%`

obs_data <- readRDS("./data/processed/obs/obs_data_qc_v4.rds")
sat_data <- readRDS("./data/processed/sat/sat_data2.rds")
obs <- obs_data$value

#date sat single point pixcel
early_hr <- sat_data$early_hr
sat_data <- t(raster::extract(early_hr,obs_data$xyz))
Time  <- index(obs)
sat <- xts::xts(sat_data, order.by = Time)
colnames(sat) <- colnames(obs)

#heigth gauges
alt_obs <- obs_data$xyz$ALT


plot_events<- function(events,n_event,obs,sat,alt_obs){

            obs <- obs[events]
            sat <- sat[events]

            dates <- index(obs)
            dates <-rep(dates,ncol(obs))

            alt <-rep(alt_obs,each=nrow(obs))

            obs_df<-coredata(obs)
            pp_obs<-as.vector(obs_df)
 
            sat_df<-coredata(sat)
            pp_sat     <-as.vector(sat_df)
 
            estacions <- rep(colnames(obs),each=nrow(obs))

            df1 <- data.frame(dates,pp=pp_obs,estacions,alt)
            
            df1 <- df1 %>% dplyr::arrange(alt)

            df2 <- data.frame(dates,pp=pp_sat,estacions,alt)
            df2 <- df2 %>% dplyr::arrange(alt)
 
            ind <- rep(c("obs","sat"), each=nrow(df1))
 
            df <-plyr::rbind.fill(df1,df2)
            df$data <-ind
            
            my.par.setting <- list(superpose.symbol =
                                      list(col =viridis::viridis_pal(option="D")(ncol(obs)),pch=19), 
                                   superpose.line =
                                      list(col =viridis::viridis_pal(option="D")(ncol(obs)),lwd=1))
            
            
            plot_obs1 <-lattice::xyplot(pp~ dates|ind,data=df,groups = estacions,layout=c(1,2),
                                        superpose = TRUE,auto.key = FALSE,type =c("b"), 
                                        grid=TRUE,par.settings =  my.par.setting,
                                        main =list(label=n_event,cex=0.9),xlab=c("horas"),
                                        ylab =c("Precipitación(mm/hr)"))
 
            return(print(plot_obs1))
            }

dates_events <- c("2015-02-09 10:00/2015-02-10 07:00",
                  "2015-03-20 11:00/2015-03-22 03:00",
                  "2016-02-25 11:00/2016-02-25 23:00",
                  "2016-02-28 13:00/2016-02-29 10:00",
                  "2017-03-15 13:00/2017-03-16 03:00",
                  "2017-03-18 02:00/2017-03-20 03:00",
                  "2018-01-25 08:00/2018-01-26 02:00",
                  "2019-01-27 12:00/2019-01-30 23:00")

for(i in 1:length(dates_events)){
  png(paste0("./data/output/plots/plot_event_",i,".png"),width=1000,height=1400, res = 150)
  plot_events(events = dates_events[i],
              n_event = paste0("(E",i,")"),
              obs = obs,
              sat = sat,
              alt_obs = alt_obs)
  dev.off()
  
}










