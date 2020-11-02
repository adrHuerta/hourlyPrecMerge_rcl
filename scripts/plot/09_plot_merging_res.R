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

# E1
merging_events$SAT$E1 %>% sp::spplot(layout = c(8, 3), main = "SAT")
merging_events$IDW$E1 %>% spplot(layout = c(8, 3))
merging_events$OK$E1 %>% spplot(layout = c(8, 3))
merging_events$GDA$E1 %>% spplot(layout = c(8, 3))
merging_events$GRA$E1 %>% spplot(layout = c(8, 3))
merging_events$IDW$E1 %>% spplot(layout = c(8, 3))

pdf("./data/output/plots/E1.pdf", onefile = TRUE, paper = "a4r", width = 20, height = 10)

for(i in names(merging_events)){
  merging_events[[i]]$E1 %>%
    setNames(names(.) %>% substr(., 2, 14)) %>% 
    spplot(layout = c(6, 4), main = paste("E1", i, sep = "-"), par.strip.text = list(cex=.8)) %>% print()
  }

dev.off()

pdf("./data/output/plots/E2.pdf", onefile = TRUE, paper = "a4r", width = 20, height = 10)

for(i in names(merging_events)){
  merging_events[[i]]$E2 %>%
    setNames(names(.) %>% substr(., 2, 14)) %>% 
    spplot(layout = c(9, 5), main = paste("E2", i, sep = "-"), par.strip.text = list(cex=.8)) %>% print()
}

dev.off()

pdf("./data/output/plots/E3.pdf", onefile = TRUE, paper = "a4r", width = 20, height = 10)

for(i in names(merging_events)){
  merging_events[[i]]$E3 %>%
    setNames(names(.) %>% substr(., 2, 14)) %>% 
    spplot(layout = c(4, 4), main = paste("E3", i, sep = "-"), par.strip.text = list(cex=.8)) %>% print()
}

dev.off()


pdf("./data/output/plots/E4.pdf", onefile = TRUE, paper = "a4r", width = 20, height = 10)

for(i in names(merging_events)){
  merging_events[[i]]$E4 %>%
    setNames(names(.) %>% substr(., 2, 14)) %>% 
    spplot(layout = c(6, 4), main = paste("E4", i, sep = "-"), par.strip.text = list(cex=.8)) %>% print()
}

dev.off()


pdf("./data/output/plots/E5.pdf", onefile = TRUE, paper = "a4r", width = 20, height = 10)

for(i in names(merging_events)){
  merging_events[[i]]$E5 %>%
    setNames(names(.) %>% substr(., 2, 14)) %>% 
    spplot(layout = c(5, 3), main = paste("E5", i, sep = "-"), par.strip.text = list(cex=.8)) %>% print()
}

dev.off()

pdf("./data/output/plots/E6.pdf", onefile = TRUE, paper = "a4r", width = 20, height = 10)

for(i in names(merging_events)){
  merging_events[[i]]$E6 %>%
    setNames(names(.) %>% substr(., 2, 14)) %>% 
    spplot(layout = c(10, 5), main = paste("E6", i, sep = "-"), par.strip.text = list(cex=.8)) %>% print()
}

dev.off()

pdf("./data/output/plots/E7.pdf", onefile = TRUE, paper = "a4r", width = 20, height = 10)

for(i in names(merging_events)){
  merging_events[[i]]$E7 %>%
    setNames(names(.) %>% substr(., 2, 14)) %>% 
    spplot(layout = c(7, 3), main = paste("E7", i, sep = "-"), par.strip.text = list(cex=.8)) %>% print()
}

dev.off()

pdf("./data/output/plots/E8.pdf", onefile = TRUE, paper = "a4r", width = 20, height = 10)

for(i in names(merging_events)){
  merging_events[[i]]$E8 %>%
    setNames(names(.) %>% substr(., 2, 14)) %>% 
    spplot(layout = c(12, 7), main = paste("E8", i, sep = "-"), par.strip.text = list(cex=.8)) %>% print()
}

dev.off()