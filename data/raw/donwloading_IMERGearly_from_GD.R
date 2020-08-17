#library("googledrive")
#library("dplyr")

googledrive::drive_auth(email = "adrhuerta@gmail.com")

path_googledrive <- sapply(2016:2019, function(x){
  sapply(c(1,2,3,11,12), function(y){
    file.path("early",
              x,
              formatC(y, flag = "0", width = 2))
    }) 
  }) %>% 
  c() 

for(path_gd in path_googledrive){
  
  local_path <- file.path(".", "data", "raw", path_gd)
  
  dir.create(local_path, recursive = TRUE)
  
  repeat{
    data_googledrive <- googledrive::drive_ls(path = path_gd)
    if(dim(data_googledrive)[1] >= 1344){
      break
    }
  }
  
  Sys.sleep(30)
  
  for(file in 1:dim(data_googledrive)[1]){
    Sys.sleep(1)
    googledrive::drive_download(file = googledrive::as_id(data_googledrive[file,]), 
                                path = file.path(local_path, data_googledrive$name[file]),
                                overwrite = FALSE)
  }
  
  }




