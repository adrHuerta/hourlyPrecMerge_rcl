calculate_metrics <- function(obs,sat,type_of_validation)
{


  #calculate
  metrics_calculate <- function(x,y){
              corr <- hydroGOF::rPearson(y, x, na.rm=TRUE)
              bias <- hydroGOF::pbias(y, x, na.rm=TRUE)
              rmsE <- hydroGOF::rmse(y,x, na.rm=TRUE)
              maE  <- hydroGOF::mae(y,x, na.rm=TRUE)
              MAD  <- median(abs(x-y),na.rm=TRUE)
              bias <- c(bias/100)
              table <- data.frame(corr,bias,MAD,rmsE,maE)
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


