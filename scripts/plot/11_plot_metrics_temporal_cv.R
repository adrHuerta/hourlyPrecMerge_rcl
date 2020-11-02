rm(list = ls())
require(xts)
"%>%" = magrittr::`%>%`

metrics <- readRDS('./data/output/metrics_cv.rds')
data_merging <-readRDS("./data/processed/merging/merging_grid_CV.RDS")
temporal <- metrics$metrics_temporal

n_events <- 8
events <- list()
metodos <- list()
methods <- c("SAT", 
          "IDW", "OK",
          "GDA", "GRA",
          "RIDW", "RIDW2","CM_IDW",
          "RK", "RK2", "CM_OK")

########plot temporal all methods
for (j in 1:length(methods)) {
  for(i in 1:n_events){ 
    events[[i]]=round(apply(temporal[[j]][[i]],2,mean,na.rm=TRUE),2)
  }
  
  estd_event=data.frame(dplyr::bind_rows(events))
  estd_event$event=c("E1","E2","E3","E4","E5","E6","E7","E8")
  estd_event$methods=rep(methods[j],8)
  metodos[[j]]=estd_event
}


estd_temporal=data.frame(dplyr::bind_rows(metodos))

#count NAs
count.na <- data.frame()
for (j in 1:length(methods)) {
  for (i in 1:n_events) {
    count.na[j,i]=sum(is.na(data_merging[[j]][[i]]))
  }
}
count.na <-t(count.na)

n_date <- n_events*length(methods)
dim(count.na)=c(n_date,1)
estd_temporal$count.na=count.na

cor =estd_temporal[,c("corr","event","methods")]
cor$metrica=rep("CC",n_date)
labels=c("valor","evento","metodo","metrica")
colnames(cor)=labels

bias =estd_temporal[,c("bias","event","methods")]
bias$metrica=rep("bias",n_date)
colnames(bias)=labels

mad =estd_temporal[,c("MAD","event","methods")]
mad$metrica=rep("MAD",n_date)
colnames(mad)=labels

rmse =estd_temporal[,c("rmsE","event","methods")]
rmse$metrica=rep("RMSE",n_date)
colnames(rmse)=labels

mae =estd_temporal[,c("maE","event","methods")]
mae$metrica=rep("MAE",n_date)
colnames(mae)=labels

valores.na =estd_temporal[,c("count.na","event","methods")]
valores.na$metrica=rep("valores.NA",n_date)
colnames(valores.na)=labels

estd_temporal_final=plyr::rbind.fill(cor,bias,mad,rmse,mae,valores.na)

#re-order
estd_temporal_final$metodo=factor(estd_temporal_final$metodo, 
                                  levels=methods, order=TRUE)
estd_temporal_final$metrica=factor(estd_temporal_final$metrica, 
                                  levels=c("CC","MAE","valores.NA",
                                           "bias","RMSE","MAD"), order=TRUE)



myColours <- c("#00CC66","#99CCFF","#66B2FF","#3399FF","#0080FF",
               "#0066CC","#004C99","#003366","#330066","#6600CC","#9933FF")

my.settings <- list(
  superpose.polygon=list(col=myColours, border="transparent"),
  strip.background=list(col="gray15"),
  strip.border=list(col="black")
)


png("./data/output/plots/plot_metrics.cv_temporal2.png",width=1353,height=694, res=96)

lattice::barchart(valor ~ evento | metrica, groups=metodo, origin=0,
         box.width=0.9,
         data=estd_temporal_final, 
         xlab="Eventos", ylab=c("valor","mm/hr"),
         scales=list(y="free"),  #x=free                
         auto.key=list(space="top", columns=4, 
                       points=FALSE, rectangles=TRUE,
                       title="Método", cex.title=1),
         par.settings = my.settings,
         par.strip.text=list(col="white", font=2),
         panel=function(x,y,...){
           lattice::panel.grid(h=-1, v=0); 
           lattice::panel.barchart(x,y,...)
         })

dev.off()

####plot temporal only 9 methods

estd_temporal_final$metodo <- as.character(estd_temporal_final$metodo)
estd_temporal_final<- dplyr::filter(estd_temporal_final, metodo!="GDA" & metodo!="GRA")
estd_temporal_final$metodo=factor(estd_temporal_final$metodo,levels=methods[-c(4,5)], order=TRUE)

my.settings <- list(
  superpose.polygon=list(col=myColours[-c(4:5)], border="transparent"),
  strip.background=list(col="gray15"),
  strip.border=list(col="black"))

png("./data/output/plots/plot_metrics.cv_temporal.png",width=1353,height=694, res=96)

lattice::barchart(valor ~ evento | metrica, groups=metodo, origin=0,
         box.width=0.9,
         data=estd_temporal_final, 
         xlab="Eventos", ylab=c("valor","mm/hr"),
         scales=list(y="free"),  #x=free                
         auto.key=list(space="top", columns=4, 
                       points=FALSE, rectangles=TRUE,
                       title="Técnica", cex.title=1),
         par.settings = my.settings,
         par.strip.text=list(col="white", font=2),
         panel=function(x,y,...){
           lattice::panel.grid(h=-1, v=0); 
           lattice::panel.barchart(x,y,...)
         })

dev.off()

