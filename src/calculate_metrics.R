calculate_metrics <- function(obs,sat,type_of_validation)
{
  source('./src/pod_far_csi.R')

  #calculate
  metrics_calculate <- function(x,y){
              corr <- hydroGOF::rPearson(y, x, na.rm=TRUE)
              bias <- hydroGOF::pbias(y, x,na.rm=TRUE)
              rmsE <- hydroGOF::rmse(y,x,na.rm=TRUE)
              mE   <- hydroGOF::me(y,x,na.rm=TRUE)
              maE  <- hydroGOF::mae(y,x,na.rm=TRUE)
              merge_xt <-cbind(x, y)
              merge_xt <- na.omit(merge_xt)
              pod.far  <- POD_FAR_CSI(SAT = merge_xt[,2],
                                      OBS = merge_xt[,1],
                                      tresh = 0.1)
              table <- data.frame(corr,rmsE,mE,maE,bias,pod.far)
              }
  #statistics
  ls.metrics <- list()
  
  #type of validation
  if(type_of_validation == "temporal"){ 
    for(i in 1:ncol(obs)){
      x = zoo::coredata(obs[,i])
      y = zoo::coredata(sat[,i])
      ls.metrics[[i]] <- metrics_calculate(x,y)
    }
    df_metrics <- dplyr::bind_rows(ls.metrics)
    return(df_metrics)
  }
  else if(type_of_validation == "spatial"){
      for(i in 1:nrow(obs)){
      x = t(obs[i,])
      y = t(sat[i,])
      ls.metrics[[i]] <- metrics_calculate(x,y)
      }
    df_metrics <- dplyr::bind_rows(ls.metrics)
    return(df_metrics)
  }
     
}


