qc_internal_consistency_check <- function(spatial_point)
{
  
  obs_value <- spatial_point@data[, "OBS"]
  obs_value <- ifelse(obs_value < 0 | obs_value > 305, NA,  obs_value)
  spatial_point@data[, "OBS"] <- obs_value
  spatial_point#[complete.cases(spatial_point@data), ]
  
}

qc_extreme_check <- function(spatial_point, 
                             daily_monthly_limits)
{
  
  IDs <- spatial_point@data$CODE
  MONTH <- strsplit(unique(spatial_point@data$DATE), "-")[[1]][2]
  daily_monthly_limits <- daily_monthly_limits[match(IDs, rownames(daily_monthly_limits)), as.numeric(MONTH)]
  
  obs_value <- spatial_point@data[, "OBS"]
  obs_value <- ifelse(obs_value > daily_monthly_limits, NA,  obs_value)
  spatial_point@data[, "OBS"] <- obs_value
  spatial_point#[complete.cases(spatial_point@data), ]
  
}

qc_spatial_consistency <- function(spatial_point,
                                   threshold = 125,
                                   neighborhood = 0.5)
{
  
  # creating gridded area
  rgrid <- raster::raster(ncol = 50, nrow = 50, 
                          xmx = extent(spatial_point)[2], 
                          xmn = extent(spatial_point)[1], 
                          ymn = extent(spatial_point)[3], 
                          ymx = extent(spatial_point)[4])
  raster::values(rgrid) <- NA
  raster::projection(rgrid) <- raster::projection(spatial_point)
  
  # only in non NA values
  spatial_point_cc <- spatial_point[complete.cases(spatial_point@data), ]
  rownames(spatial_point_cc@data) <- NULL
  
  # distance between point (only data less than 50km ~ 0.5)
  dist_matrix <- rgeos::gDistance(spatial_point_cc, byid = TRUE)
  dist_matrix[dist_matrix > neighborhood] <- NA
  
  
  # 1st comparison
  to_validate <- sapply(1:dim(spatial_point_cc)[1], 
                        function(x){
                          
                          to_validate <- spatial_point_cc[x,]
                          gsO <- gstat::gstat(formula = OBS ~ 1, locations = spatial_point_cc[-x, ])
                          
                          idw <- raster::interpolate(rgrid, gsO)
                          round(raster::extract(idw, to_validate), 1)
                          
                        })
  
  # computing error in data
  errors <- ((to_validate + 1)/(spatial_point_cc@data$OBS + 1))*100 - 100
  
  target_station <- which(errors > threshold | errors < -threshold)
  
  if(length(target_station) != 0){
    
    sapply(target_station, function(z){
      
      neighbor_station <- dist_matrix[, z][order(dist_matrix[, z])]
      neighbor_station <- as.numeric(names(neighbor_station[!is.nan(neighbor_station)])[2:3])
      
      to_validate2 <- sapply(neighbor_station,
                             function(x){
                               
                               to_validate <- spatial_point_cc[z,]
                               gsO <- gstat::gstat(formula = OBS ~ 1,
                                                   locations = spatial_point_cc[-c(x, z), ])
                               
                               idw <- raster::interpolate(rgrid, gsO)
                               round(extract(idw, to_validate), 1)
                               
                             })
      
      error2 <- ((to_validate2 + 1)/(spatial_point_cc@data$OBS[z] + 1))*100 - 100
      
      to_delete <- ifelse(all(error2 > threshold | error2 < -threshold), z,
                          neighbor_station[which(!(error2 > threshold | error2 < -threshold))])
      
      spatial_point_cc@data$CODE[to_delete]
      
    }) -> to_delete
    
    
    spatial_point@data[match(to_delete, spatial_point@data$CODE), c("OBS")] <- NA
    return(spatial_point)
    
  } else {
    
    return(spatial_point)
    
    
  }
  
  
}