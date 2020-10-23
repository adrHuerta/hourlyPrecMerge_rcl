rm(list = ls())

require(xts)
"%>%" = magrittr::`%>%`

obs_data <- readRDS("./data/processed/obs/obs_data_qc_v3.rds")
data_sat <- readRDS("./data/processed/sat/sat_data2.rds")

obs <- obs_data$value
sat <- data_sat$value

#join ts obs sat 
ts_merge <- merge(obs,sat,join='left') 
ts_merge <- data.frame(ts_merge)

#calculate categoric pp 
box_pp_categoric <-  function( ts_merge, value1, label)
  {
    source('./src/pod_far_csi.R')
    
    gauge_label <- colnames(ts_merge)
    metrics <- list()
    
    for(i in 1:(ncol(ts_merge)/2)){ 
      ts_merge1 <- ts_merge %>% 
                   dplyr::select(contains(gauge_label[i]))
      
      if(nrow(ts_merge1)== 0){
        metrics[[i]] =NA
      } else {
        metrics[[i]] <- POD_FAR_CSI(SAT = ts_merge1[,2],
                                    OBS = ts_merge1[,1],
                                    tresh = value1)
      }
    } 
    metrics <-Filter(function(a) any(!is.na(a)), metrics)
    df_metrics <- dplyr::bind_rows(metrics) %>%
                  rapply(c) 
    type <- rep(c("POD","FAR","CSI"),rep(length(metrics),3))
    category <- rep(label,length(df_metrics))
    df_final <- data.frame(df_metrics,type,category)
    return(df_final)
  }

# 5 categoric 
catg1 <- box_pp_categoric(ts_merge,value1=0.1,label="0.1")
catg2 <- box_pp_categoric(ts_merge,value1=0.2,label="0.2")
catg3 <- box_pp_categoric(ts_merge,value1=0.5,label="0.5")
catg4 <- box_pp_categoric(ts_merge,value1=0.7,label="0.7")
catg5 <- box_pp_categoric(ts_merge,value1=1.0,label="1.0")
catg6 <- box_pp_categoric(ts_merge,value1=1.5,label="1.5")


tabla_box=dplyr::bind_rows(catg1, catg2, catg3, catg4, catg5, catg6)


png("./data/output/plots/plot_categoric_pp.png",width=850,height=500, res = 110)

lattice::trellis.device(new=FALSE, col=FALSE)
lattice::bwplot(df_metrics ~ factor(category) | type, data = tabla_box, layout = c(3,1),
                ylab=c("Valor del índice"),xlab=c("Categoria de Precipitación (mm/hr)"),
                prepanel = function(x, y) {
                           bp <- boxplot(split(y, x), plot = FALSE)
                           list(ylim = c(0:1)) },
                scales = list(y = list(relation = "free"),
                              x = list(rot=45)),do.out = T)     
dev.off()
