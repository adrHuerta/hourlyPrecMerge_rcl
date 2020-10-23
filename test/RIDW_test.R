#' Regression IDW Optimizing inverse distance weighting power

 require(dplyr)
 require(gstat)

RIDW <- function(gauge, cov, cov2, formula, num_cov, idpR, norm) {
          
           if(is.nan(mean(gauge@data$pp_obs,na.rm=TRUE))) {
           Ridw <- NA
           }else { 
           #normalization
           if(norm=="sqrt"){ 
           gauge@data[,1]<-sqrt(gauge@data[,1])
           cov<-sqrt(cov)
           } 
    
           ext<-raster::extract(cov,gauge,cellnumber=F,sp=T)
           gauge=cbind(gauge@data,gauge@coords)
           gauge<-na.omit(gauge) 
           station <- sp::SpatialPointsDataFrame(coords=gauge[,2:3], data.frame(gauge$pp_obs),
                                                  proj4string =sp::CRS("+proj=longlat +ellps=WGS84 
                                                                       +towgs84=0,0,0,0,0,0,0 +no_defs") )
  
           linear <- na.omit(ext@data) %>% 
                                  tbl_df %>% 
                    mutate_all(as.character) %>%
                     mutate_all(as.numeric)
           
           names(linear)<-c("pp_obs","pp_sat")
  
           if (num_cov == "one_hour_before"){
                     cov2 <- sqrt(cov2[[1]])
                     ext2 <- raster::extract(cov2,gauge[,2:3],cellnumber=F,sp=T)
           linear$sat_1 <- ext2 
           }
           else if(num_cov == "two_hours_before"){
           cov_2<-sqrt(cov2[[1]])
           cov_3<-sqrt(cov2[[2]])
           ext2<-raster::extract(cov_2,gauge[,2:3],cellnumber=F,sp=T)
           ext3<-raster::extract(cov_3,gauge[,2:3],cellnumber=F,sp=T)
           linear$sat_1 <- ext2 
           linear$sat_2 <-  ext3
           }
  
           # regresions ---------------------------------------------------------------
           llm <- lm(formula,linear)
           station$residuals <- llm$residuals
  
  
           # Define Grid -------------------------------------------------------------
           point <- raster::rasterToPoints(cov) %>% data.frame
           sp::coordinates(point) <- ~x + y
           raster::projection(point) <- raster::projection(cov)
  
           # Estimate Best Parameter -------------------------------------------------
           idpRange<-idpR
           mse<- rep(NA, length(idpRange))
           for (i in 1:length(idpRange)){
           mse[i] =mean(krige.cv(residuals~1, station, nfold = nrow(station),
                          nmax = Inf, set = list(idp = idpRange[i]),verbose = F)$residual**2)	 
           }
           poss <- which(mse %in% min(mse))
           bestparam <- idpRange[poss]
           residual.best <- krige.cv(residuals ~ 1, station, nfold = nrow(station), set = list(idp = idpRange[poss]), verbose = F)$residual
  
           # Interpolation ----------------------------------------------------------
  
           idwError <- idw(residuals ~ 1, station, point, idp = bestparam)
           idwError <- idwError["var1.pred"]
           sp::gridded(idwError) <- TRUE
           mapa <- raster::raster(idwError)
           namesF <- unlist(strsplit(as.character(formula), " "))
           max_k <- floor(length(namesF)/2) + 1
           name_cov = namesF[!namesF %in% c("~", "+", "-", "*", "/")][2:max_k]
           names(cov)=name_cov[1]
  
           OBSp <- sum(raster::stack(mapply(function(i) cov[[i]] * llm$coefficients[i + 1],
                           1:raster::nlayers(cov)))) + llm$coefficients[1]
           Ridw <- OBSp + mapa
           # Save Data ---------------------------------------------------------------
           #  list(Interpol = Ridw, params = list(bestp = bestparam, rmse = sqrt(mean(residual.best^2)),
           #                                   linear_Model = llm))
           Ridw
        } 
    }