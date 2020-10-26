idw_opt <- function(gauge_points, 
                    gridded_location,
                    idpR = seq(0.8, 3.5, 0.1))
{
  
  gstat::gstat(formula = gauge~1, locations = gridded_location)
  
  idpRange <- idpR
  mse <- rep(NA, length(idpRange))
  for (i in 1:length(idpRange)) {
    mse[i] <- mean(gstat::krige.cv(as.formula(formula), gauge, nfold = nrow(gauge),
                                   nmax = Inf, set = list(idp = idpRange[i]), verbose = F)$residual^2)
  }
  
}