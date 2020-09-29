rm(list = ls())
"%>%" = magrittr::`%>%`

library(foreach)


cl <- parallel::makeCluster(8)
doParallel::registerDoParallel(cl)

files_imerg <- dir("./data/raw/early", full.names = TRUE, recursive = TRUE)


foreach(i = 1:length(files_imerg),
        .export = "files_imerg") %dopar%{
  
  raster::raster(files_imerg[i]) %>%
                  raster::crop(raster::extent(c(-77.4, -76.0, -12.32, -11.0))) %>%
                  raster::writeRaster(filename= file.path(".", "data", "raw","early", "early_cut",
                                                          strsplit(files_imerg[i], "/.")[[1]][7]),
                                      format = "CDF",
                                      overwrite = TRUE)
  }

parallel::stopCluster(cl)
