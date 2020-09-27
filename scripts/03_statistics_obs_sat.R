
require(hydroGOF)

source('./src/pod_far_csi.R')
data_sat <-readRDS("./data/processed/sat/sat_data.rds")
data_obs <-readRDS("./data/processed/obs/obs_data_qc_v2.rds")

estd <- data.frame()

statistical <- function(obs,sat,n){ 
    for(i in 1:n){
    x = obs[,i]
    y = sat[,i]
    
    merge_xt = cbind(x, y)
    colnames(merge_xt)<-c("obs","sat")
    
    merge_xt[is.na(merge_xt)]=0
    corr <- cor(merge_xt,method ="spearman")
    corr <- corr[2,1]
    bias <- pbias(y, x)
    rmsE <- rmse(y,x)
    mE  <- me(y,x)
    maE  <- mae(y,x)
    
    all(time(merge_xt$sat) == time(merge_xt$obs))
    pod.far <-  POD_FAR_CSI(SAT = coredata(merge_xt$sat), OBS = coredata(merge_xt$obs))
    
    #quantitative
    estd[i,1] <- corr
    estd[i,2] <- rmsE
    estd[i,3] <- mE
    estd[i,4] <- maE
    estd[i,5] <- round((bias/100),2)
    #qualitative
    estd[i,6] <- pod.far[,1]
    estd[i,7] <- pod.far[,2] 
    estd[i,8] <- pod.far[,3] 
  }
  estd
}

#data obs
data.obs= data_obs$value[,-c(34,16,36,27)] #IDS remove

#summary 
estd.e <- statistical(data_sat,data.obs,ncol(data.obs))
mean_e <- round(apply(estd.e,2,mean,na.rm=TRUE),2)
names(mean_e) <- c("CC","RMSE","ME","MAE","Bias","POD","FAR","CSI")
mean_e
