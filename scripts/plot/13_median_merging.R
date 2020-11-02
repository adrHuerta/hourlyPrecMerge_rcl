rm(list = ls())
"%>%" = magrittr::`%>%`

library(raster)

merging_events <- readRDS("./data/processed/merging/merging_grid.RDS")
# names of methods
names(merging_events) <- c("SAT", 
                           "IDW", "OK",
                           "GDA", "GRA",
                           "RIDW", "RIDW2","CM_IDW",
                           "RK", "RK2", "CM_OK")
# names of events
merging_events <- Map(function(x, y) setNames(x, paste("E", 1:8, sep = "")),
                      merging_events, names(merging_events))


median_value <- lapply(merging_events[c("IDW", "OK",
                                        "RIDW", "RIDW2","CM_IDW",
                                        "RK", "RK2", "CM_OK")],
                       function(x){
                         x$E3
                       })

(1:raster::nlayers(median_value$IDW)) %>%
  lapply(function(x){
           raster::calc(raster::brick(median_value$IDW[[x]],
                                median_value$OK[[x]],
                                median_value$RIDW[[x]],
                                median_value$RIDW2[[x]],
                                median_value$CM_IDW[[x]],
                                median_value$RK[[x]],
                                median_value$RK2[[x]],
                                median_value$CM_OK[[x]]),
                        function(z) median(z, na.rm = TRUE))
         }) -> median_value_E3

res <- raster::brick(median_value_E3)
names(res) <- names(merging_events$SAT$E3)
pal <- c(rev(colorRampPalette(RColorBrewer::brewer.pal(7, "Greys"))(12)), 
         colorRampPalette(RColorBrewer::brewer.pal(7, "YlGnBu"))(12))


png(paste0("./data/output/plots/plot_event_median.png"),width=1000,height=1400, res = 150)
spplot(res[[1:12]], col.regions = pal, at = seq(0, 11, .5))
dev.off()
