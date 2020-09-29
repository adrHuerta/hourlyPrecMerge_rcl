check_imerg_file <- function(location,
                             data_ini,
                             data_end)
  {
  Time  <- seq(as.POSIXct(data_ini), as.POSIXct(data_end), by='30 mins')
  ntime <- length(Time)
  
  # Verificar datos
  files <- list.files(location, pattern='.nc$', recursive=TRUE, full.names=TRUE)
  nfile <- length(files)
  
  if(nfile == ntime){
    message('It is ok!')
  }else{
    if(nfile > ntime){
      message('ERROR: There are duplicated files!')
      file.remove(list.files(location, pattern=').nc$', recursive=TRUE, full.names=TRUE))
    }
    if(nfile < ntime){
      message('ERROR: There are missing files, please download!')
    }
  }
  }