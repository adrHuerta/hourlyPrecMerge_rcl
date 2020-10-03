library(dplyr)
library(lattice)



obs_data <- readRDS("./data/processed/obs/obs_data_qc_v4.rds")
data_sat <-readRDS("./data/processed/sat/sat_data2.rds")

obs=obs_data$value
sat=data_sat$value

ts=merge(obs,sat,join='left') 
ts=data.frame(ts)


box_pp_categoric <-  function( ts, valor1, valor2, label)
  {
    source('./src/pod_far_csi.R')
    gauge=colnames(ts)
    corr = list()
    
    for(i in 1:37){ 
      est <-  ts %>% 
        filter((  valor1 <=ts[,i] & ts[,i] < valor2 )|(valor1 <=ts[,i+37] & ts[,i+37] < valor2) ) %>%
        dplyr::select(contains(gauge[i]))
      
      
      if(nrow(est)== 0){
        corr[[i]] =NA
      } else {
        corr[[i]] <- POD_FAR_CSI(SAT = est[,2],
                                 OBS = est[,1])
      }
    } 
    corr=Filter(function(a) any(!is.na(a)), corr)
    df_estd=dplyr::bind_rows(corr) %>%
             rapply(c) 
    
    type=rep(c("POD","FAR","CSI"),rep(length(corr),3))
    category=rep(label,length(df_estd))
    df=data.frame(df_estd,type,category)
    return(df)
  }
 
#catg1=box_pp_categoric(ts,valor1=0,valor2 = 0.1,"0 - .1")
catg2=box_pp_categoric(ts,valor1=0.1,valor2 = 0.25,".1 - .25")
catg3=box_pp_categoric(ts,valor1=0.25,valor2 = 0.5,".25 - .5")
catg4=box_pp_categoric(ts,valor1=0.5,valor2 = 1.0,".5 - 1")
catg5=box_pp_categoric(ts,valor1=1.0,valor2 = 5.0,"1 - 5")
#catg6=box_pp_categoric(ts,valor1=2.0,valor2 = 5.0,"2-5")
catg6=box_pp_categoric(ts,valor1=5.0,valor2 = 18.9,"<5")
#colMax <- function(ts) sapply(ts, max, na.rm = TRUE)

tabla_box=dplyr::bind_rows(catg2,catg3,catg4,catg5,catg6)


# my.panel <- function(..., box.ratio) {
#   panel.stripplot(..., col=c("blue"), alpha=0.2,
#                   jitter.data = TRUE, pch = 19, cex=0.5)
# } range(bp$stats)
#png("./data/output/plots/plot_categoric_pp2.png",width=850,height=500, res = 110)
trellis.device(new=FALSE, col=FALSE)
bwplot(df_estd ~ factor(category) | type, data = tabla_box, 
       layout = c(3,1), ylab=c("Valor del índice"),xlab=c("Categoria de Precipitación (mm/hr)"),
       # panel = my.panel,
       prepanel = function(x, y) {
         bp <- boxplot(split(y, x), plot = FALSE)
         ylim <- c(0:1)
         list(ylim = ylim) },
       scales = list(y = list(relation = "free"),x=list(rot=45)),
       do.out = T)     
#dev.off()
