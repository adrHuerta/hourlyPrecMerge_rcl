make_imerg_path <- function(date_imerg, dir_path, lag_time = 2)
{
  
  date_imerg = list(date_imerg + 60*60*((lag_time-1)/2),
                    date_imerg + 60*60*((lag_time-lag_time)/2),
                    date_imerg - 60*60*((lag_time-1)/2) ,
                    date_imerg - 60*60*(lag_time-1))
  return(lapply(date_imerg,
                function(time_z){
                  paste(
                    c("3B-HHR-E.MS.MRG.3IMERG.", 
                      format(time_z, "%Y"), 
                      format(time_z, "%m"),
                      format(time_z, "%d"),
                      "-S",
                      format(time_z, "%H"),
                      format(time_z, "%M"),
                      format(time_z, "%S"),
                      "-E",
                      format(time_z + 60*29 + 59, "%H"),
                      format(time_z + 60*29 + 59, "%M"),
                      format(time_z + 60*29 + 59, "%S"),
                      ".",
                      get_minutes_from_00(time_z),
                      ".",
                      "V06B.tif"),
                    collapse = "") -> file_imerg_a
                  
                  file.path(dir_path, format(time_z, "%Y"), format(time_z, "%m"), file_imerg_a)
                  
                  
                  
                })
  )
}

get_minutes_from_00 <- function(x)
{
  
  formatC(as.numeric(difftime(as.POSIXct(format(x, "%H:%M"), format = '%H:%M'),
                              as.POSIXct('00:00', format = '%H:%M'), units = 'min')
  ),
  width = 4, flag = "0")
}