rm(list = ls())
"%>%" = magrittr::`%>%`

source("./src/points_inside_pixel.R")

library(xts)
library(raster)

obs <- readRDS("./data/processed/obs/obs_data_qc_v3.rds")
sat <-readRDS("./data/processed/sat/sat_data2.rds")

# which point-stations are inside a pixel?
sp::spplot(pointcount(sat$early_hr[[1]],
                      obs$xyz))

sp::spplot(pointcount(sat$early_hr[[1]],
                      make_single_point(pts = obs$xyz,
                                        rgrid = sat$early_hr[[1]])$xyz))

# getting single points 
new_xyz <- make_single_point(pts = obs$xyz, rgrid = sat$early_hr[[1]])$xyz

# points to be merged
id_to_merge <- make_single_point(pts = obs$xyz, rgrid = sat$early_hr[[1]])$no_single_points

# single ts
new_value <- obs$value[, -match(unlist(id_to_merge), colnames(obs$value))]

# merging no-single ts
id_to_merge <- id_to_merge %>%
  setNames(id_to_merge %>% lapply(function(x) paste(x, collapse = "_"))) %>%
  lapply(function(x){
    res <- obs$value[, x] %>% apply(1, mean, na.rm = TRUE)
    res[is.nan(res)] <- NA
    res
    }) %>%
  do.call("cbind", .)

id_to_merge <- xts::xts(id_to_merge, time(obs$value))

# single + new single ts
new_value <- cbind(new_value, id_to_merge)

# are ID same?
colnames(new_value) == new_xyz@data$CODE


saveRDS(list(value = new_value,
             xyz = new_xyz),
        file = "./data/processed/obs/obs_data_qc_v4.rds")