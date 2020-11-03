idw_opt <- function(gauge_points, 
                    gridded_location,
                    idpR = seq(0.8, 3.5, 0.1))
{
  
  if(all(gauge_points@data[, "to_interpol"] == 0)){
    
    response <- gridded_location
    raster::values(response) <- 0
    
    return(response)
    
  } else {
   
    # Searching of best parameter 
    idpRange <- idpR
    mse <- rep(NA, length(idpRange))
    for (i in 1:length(idpRange)) {
      mse[i] <- mean(gstat::krige.cv(to_interpol ~ 1, gauge_points, nfold = nrow(gauge_points),
                                     nmax = Inf, set = list(idp = idpRange[i]), verbose = F)$residual^2)
    }
    
    # Best parameter 
    poss <- which(mse %in% min(mse))
    bestparam <- idpRange[poss]
    
    # IDW
    gs <- gstat::gstat(formula = to_interpol ~ 1, locations = gauge_points, nmax = Inf, set = list(idp = bestparam))
    response <- raster::interpolate(gridded_location, gs)
    
    return(response)
    
  }
}

ok_aut <- function(gauge_points,
                   gridded_location)
{
  
  if(all(gauge_points@data[, "to_interpol"] == 0)){
    
    response <- gridded_location
    raster::values(response) <- 0
    
    return(response)
    
  } else {
   
    # No coordinates projection
    gridded_location2 <- gridded_location
    raster::projection(gridded_location2) <- NA
    
    gauge_points2 <- gauge_points
    raster::projection(gauge_points2) <- NA
    
    # Defining grid
    gridded_location2 <- as(gridded_location2, 'SpatialGrid')
    
    # Automatic variogram fit
    variogram_fit <- automap::autofitVariogram(to_interpol ~ 1, input_data = gauge_points2)
    
    # Ordinary Kriging
    gs <- gstat::gstat(formula = to_interpol ~ 1, locations = gauge_points2, model = variogram_fit$var_model)
    kp <- predict(gs, gridded_location2)
    response <- raster::brick(kp)
    raster::projection(response) <- raster::projection(gridded_location)
    
    return(response[[1]])
    
    
  }
}

GDA <- function(gauge_points,
                gridded_cov)
{
  
  # Getting data
  obs <- gauge_points[, "obs"]
  obs@data["cov"] <- raster::extract(gridded_cov, gauge_points)
  
  # Transformation
  obs@data <- sqrt(obs@data)
  obs@data["to_interpol"] <- obs@data["obs"] - obs@data["cov"]
  
  # IDW
  to_interpol <- idw_opt(gauge_points = obs, gridded_location = gridded_cov)
  response <- to_interpol + gridded_cov
  
  # Inverse-Transformation
  response <- response^2
  response[response < 0] <- 0
  response <- round(response, 1)
  
  return(response)
}

GRA <- function(gauge_points,
                gridded_cov)
{
  
  obs <- gauge_points[, "obs"]
  obs@data["cov"] <- raster::extract(gridded_cov, gauge_points)
  
  # Transformation
  obs@data <- sqrt(obs@data)
  obs@data["to_interpol"] <- (obs@data["obs"] + .1 )/(obs@data["cov"] + .1)
  
  # IDW
  to_interpol <- idw_opt(gauge_points = obs, gridded_location = gridded_cov)
  response <- to_interpol*gridded_cov
  
  # Inverse-Transformation
  response <- response^2
  response[response < 0] <- 0
  response <- round(response, 1)
  
  return(response)
  
}

RIDW <- function(gauge_points,
                 gridded_cov)
{
  
  obs <- gauge_points[, "obs"]
  
  # Transformation
  obs@data <- sqrt(obs@data)
  gridded_cov <- sqrt(gridded_cov)
  
  # Making names to model
  names(gridded_cov) <- paste("cov", 1:length(names(gridded_cov)), sep = "")
  
  # LM
  obs@data[names(gridded_cov)] <- raster::extract(gridded_cov, gauge_points)
  
  make_formula <- as.formula(paste("obs", "~", paste(names(gridded_cov), collapse = "+")))
  linear_model <- lm(make_formula, data = obs@data)
  obs@data["to_interpol"] <- linear_model$residuals
  
  # IDW
  to_interpol <- idw_opt(gauge_points = obs, gridded_location = raster::mean(gridded_cov))
  
  if(raster::nlayers(gridded_cov) == 1){
    response_model <- linear_model$coefficients[1] + linear_model$coefficients[-1]*gridded_cov
  } else {
    response_model <- linear_model$coefficients[1] + sum(linear_model$coefficients[-1]*gridded_cov)
    
  }
  response <- response_model + to_interpol
  
  # Inverse-Transformation
  response <- response^2
  response[response < 0] <- 0
  response <- round(response, 1)
  
  return(response)
}

RK <- function(gauge_points,
               gridded_cov)
{
  
  obs <- gauge_points[, "obs"]
  
  # Transformation
  obs@data <- sqrt(obs@data)
  gridded_cov <- sqrt(gridded_cov)
  
  # Making names to model
  names(gridded_cov) <- paste("cov", 1:length(names(gridded_cov)), sep = "")
  
  # LM
  obs@data[names(gridded_cov)] <- raster::extract(gridded_cov, gauge_points)
  
  make_formula <- as.formula(paste("obs", "~", paste(names(gridded_cov), collapse = "+")))
  linear_model <- lm(make_formula, data = obs@data)
  obs@data["to_interpol"] <- linear_model$residuals
  
  # OK
  to_interpol <- ok_aut(gauge_points = obs, gridded_location = raster::mean(gridded_cov))
  
  if(raster::nlayers(gridded_cov) == 1){
    response_model <- linear_model$coefficients[1] + linear_model$coefficients[-1]*gridded_cov
  } else {
    response_model <- linear_model$coefficients[1] + sum(linear_model$coefficients[-1]*gridded_cov)
    
  }
  response <- response_model + to_interpol
  
  # Inverse-Transformation
  response <- response^2
  response[response < 0] <- 0
  response <- round(response, 1)
  
  return(response)
}

OK <- function(gauge_points,
               gridded_cov)
{
  
  obs <- gauge_points[, "obs"]
  
  # Transformation
  obs@data <- sqrt(obs@data)
  obs@data["to_interpol"] <- obs@data$obs
  
  # OK
  response <- ok_aut(gauge_points = obs, gridded_location = gridded_cov)
  
  # Inverse-Transformation
  response <- response^2
  response[response < 0] <- 0
  response <- round(response, 1)
  
  return(response)
}

IDW <- function(gauge_points,
                gridded_cov)
{
  
  obs <- gauge_points[, "obs"]
  
  # Transformation
  obs@data <- sqrt(obs@data)
  obs@data["to_interpol"] <- obs@data$obs
  
  # IDW
  response <- idw_opt(gauge_points = obs, gridded_location = gridded_cov)
  
  # Inverse-Transformation
  response <- response^2
  response[response < 0] <- 0
  response <- round(response, 1)
  
  return(response)
}

CM_IDW <- function(gauge_points,
                    gridded_cov)
{
  
  # Transformation and gridded
  obs <- gauge_points[, "obs"]
  obs@data["to_interpol"] <- sqrt(obs@data)
  obs <- idw_opt(gauge_points = obs, gridded_location = gridded_cov)
  
  gridded_cov <- sqrt(gridded_cov)
  
  # Making names to model
  names(gridded_cov) <- paste("cov", 1:length(names(gridded_cov)), sep = "")
  
  # LM
  obs_cov <- data.frame(obs = raster::getValues(obs))
  obs_cov[names(gridded_cov)] <- raster::getValues(gridded_cov)
  
  make_formula <- as.formula(paste("obs", "~", paste(names(gridded_cov), collapse = "+")))
  linear_model <- lm(make_formula, data = obs_cov)
  raster::values(obs) <- linear_model$residuals

  if(raster::nlayers(gridded_cov) == 1){
    response_model <- linear_model$coefficients[1] + linear_model$coefficients[-1]*gridded_cov
  } else {
    response_model <- linear_model$coefficients[1] + sum(linear_model$coefficients[-1]*gridded_cov)
    
  }
  
  response <- response_model + obs
  # Inverse-Transformation
  response <- response^2
  response[response < 0] <- 0
  response <- round(response, 1)
  
  return(response)
  
}

CM_OK <- function(gauge_points,
                  gridded_cov)
{
  
  # Transformation and gridded
  obs <- gauge_points[, "obs"]
  obs@data["to_interpol"] <- sqrt(obs@data)
  obs <- ok_aut(gauge_points = obs, gridded_location = gridded_cov)
  
  gridded_cov <- sqrt(gridded_cov)
  
  # Making names to model
  names(gridded_cov) <- paste("cov", 1:length(names(gridded_cov)), sep = "")
  
  # LM
  obs_cov <- data.frame(obs = raster::getValues(obs))
  obs_cov[names(gridded_cov)] <- raster::getValues(gridded_cov)
  
  make_formula <- as.formula(paste("obs", "~", paste(names(gridded_cov), collapse = "+")))
  linear_model <- lm(make_formula, data = obs_cov)
  raster::values(obs) <- linear_model$residuals
  
  if(raster::nlayers(gridded_cov) == 1){
    response_model <- linear_model$coefficients[1] + linear_model$coefficients[-1]*gridded_cov
  } else {
    response_model <- linear_model$coefficients[1] + sum(linear_model$coefficients[-1]*gridded_cov)
    
  }
  
  response <- response_model + obs
  # Inverse-Transformation
  response <- response^2
  response[response < 0] <- 0
  response <- round(response, 1)
  
  return(response)
  
}