POD_FAR_CSI= function(SAT, OBS, tresh)
  {
  
  
  res = data.frame(SAT, OBS)
  res = res[complete.cases(res), ]
  colnames(res) = c("sat", "obs")
  
  res <- transform(res, 
                   GD.RAIN = ifelse(sat > tresh, 1, 0),
                   OBS.RAIN = ifelse(obs > tresh, 1 , 0))
  
  res <- transform(res, HIT = ifelse(OBS.RAIN == 1 & GD.RAIN == 1, 1, 0), 
                        MISS = ifelse(OBS.RAIN == 1 & GD.RAIN == 0, 1, 0), 
                        FALS.ALARM = ifelse(OBS.RAIN == 0 & GD.RAIN == 1, 1, 0),
                        CORR.NEG = ifelse(OBS.RAIN == 0 & GD.RAIN == 0, 1, 0))
  
  POD = sum(res$HIT) /sum(sum(res$HIT), sum(res$MISS))
  FAR = sum(res$FALS.ALARM) /sum(sum(res$HIT), sum(res$FALS.ALARM))
  CSI = sum(res$HIT)/sum(sum(res$HIT), sum(res$MISS),sum(res$FALS.ALARM))
  return(data.frame(POD = round(POD, 2), FAR = round(FAR, 2),CSI = round(CSI, 2)))
  
  }

