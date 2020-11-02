## plot ts metrics cv spatial
rm(list = ls())

metrics <- readRDS("./data/output/metrics_cv.rds")
spatial <- metrics$metrics_spatial

ls.cor.event <-list()
ls.ts <- list()

for(i in 1:8){
    for( j in 1:11){ 
     ts =as.matrix(spatial[[j]][[i]][-3])
     n_dates=nrow(ts)
     count=n_dates*4
     dim(ts)=c(count,1)
    ls.ts[[j]] =ts
     }
    methods=c("SAT", "IDW", "OK","GDA", 
              "GRA","RIDW", "RIDW2","CM_IDW",
              "RK", "RK2", "CM_OK")

    values=unlist(ls.ts)
    estad=rep(rep(c("CC","bias","RMSE","MAE"),each=n_dates),11)
    metod=rep(methods,each=count)
    dates=rep(rep(rownames(spatial[[2]][[i]]),4),11)
    dates=substr(dates, start = 9, stop = 16)
    
    df=data.frame(dates,values,estad,metod)
    
    
    df$metod=factor(df$metod,levels=methods, order=TRUE)
    df$estad=factor(df$estad,levels=c("bias","RMSE","CC","MAE"), order=TRUE)
    
    df_event=df; df_event$event=rep(paste0("E",i),nrow(ls.ts[[1]]))
    ls.cor.event[[i]] =df_event
    
    
    my_colors=c(RColorBrewer::brewer.pal(11,"Paired")[1:10],"#0a0a0a")
    my.par.setting <- list(superpose.symbol = list(col = my_colors,pch =19), 
                           superpose.line = list(col = my_colors,lwd=2),
                           strip.background=list(col="gray"))
    #plot all methods
    png(paste0("./data/output/plots/plot_metrics.cv_spatial",i,".png"),width=950,height=800, res = 110)
    plot=lattice::xyplot(values ~ dates| estad, data=df,superpose=TRUE,
                         auto.key=list(space="top", columns=3,points=FALSE,lines=TRUE),
                         ylab =c("mm/hr","valor"),xlab=paste0("Horas"," (E",i,")"),
                         grid = TRUE, group = metod,type = c("l"),
                         par.settings = my.par.setting,
                         scales = list(x = list(at=seq(1,n_dates,8),rot=17),y="free"))
    print(plot)
    dev.off()
  }

#plot only 9 methods
df1 <- ls.cor.event[[8]]
df1$metod <- as.character(df1$metod)
df1<- dplyr::filter(df1, metod!='GDA' & metod!='GRA')
df1$metod <- factor(df1$metod,levels=methods[-c(4,5)], order=TRUE)

my_colors=c(RColorBrewer::brewer.pal(11,"Paired")[c(1:3,6:10)],"#0a0a0a")
my.par.setting <- list(superpose.symbol = list(col = my_colors,pch =19), 
                       superpose.line = list(col = my_colors,lwd=2),
                       strip.background=list(col="gray"))

png(paste0("./data/output/plots/plot_metrics.cv_spatial.png"),width=950,height=800, res = 110)
plot2=lattice::xyplot(values ~ dates|estad, data=df1,superpose=TRUE,
                     auto.key=list(space="top", columns=3,points=FALSE,lines=TRUE),
                     ylab =c("mm/hr","valor"),xlab=paste0("Horas"," (E",i,")"),
                     grid = TRUE, group = df1$metod, type = c("l"),
                     par.settings = my.par.setting,
                     scales = list(x = list(at=seq(1,n_dates,8),rot=17),y="free"))
print(plot2)

dev.off()



####bloxplot 9 methods
df2 = plyr::rbind.fill(ls.cor.event)
df2=dplyr::filter(df2, estad=='CC')

df2$event=factor(df2$event,levels=c("E7","E8","E5","E6",
                                    "E3","E4","E1","E2"), order=TRUE)

png(paste0("./data/output/plots/plot_boxplot_cv_CC.png"),width=950,height=800, res = 110)

lattice::trellis.device(new=FALSE, col=FALSE)
lattice::bwplot(values~metod|event, data = df2, layout = c(2,4),
                ylab="Valor",xlab="Método",
                prepanel = function(x, y) {
                            bp <- boxplot(split(y, x), plot = FALSE)
                            ylim <- range(bp$stats)
                            list(ylim = ylim) },
                scales = list(y = list(relation = "free"),
                              x = list(rot=45)),do.out = F)
dev.off()

df3=dplyr::filter(df2, metod!='GDA' & metod!='GRA')

# boxplot all methodss
png(paste0("./data/output/plots/plot_boxplot_cv_CC2.png"),width=950,height=800, res = 110)

lattice::trellis.device(new=FALSE, col=FALSE)
lattice::bwplot(values~metod|event, data = df3, layout = c(2,4),
                ylab="Valor", xlab="Método",
                prepanel = function(x, y) {
                           bp <- boxplot(split(y, x), plot = FALSE)
                           ylim <- range(bp$stats)
                           list(ylim = ylim) },
                scales = list(y = list(relation = "free"),
                              x=list(rot=45)),do.out = F)
dev.off()



