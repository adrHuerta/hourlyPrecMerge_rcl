# Verificar los archivos en caso de faltantes o duplicados
rm(list=ls())
cat('\f')

# Datos de entrada 
location <- './data/raw/early/early_cut'
data_ini <- '2014-01-01 05:00' #UTC
data_end <- '2020-01-01 04:30'
by_step  <- '30 mins'  # half hours
  
# Directorio de trabajo
setwd(location)

# Serie de tiempo
Time  <- seq(as.POSIXct(data_ini), as.POSIXct(data_end), by=by_step)
ntime <- length(Time)

# Verificar datos
files <- list.files(location, pattern='.tif$', recursive=TRUE, full.names=TRUE)
nfile <- length(files)
if(nfile == ntime){
  message('It is ok!')
}else{
  if(nfile > ntime){
    message('ERROR: There are duplicated files!')
    file.remove(list.files(location, pattern=').tif$', recursive=TRUE, full.names=TRUE))
  }
  if(nfile < ntime){
    message('ERROR: There are missing files, please download!')
  }
}
