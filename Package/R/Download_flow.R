

Download_flow <- function(regime){
  # Download netcdf flow
  URL_readme <- "https://ndownloader.figshare.com/files/10620898"
  download.file(URL_readme, destfile="../Flow data/Readme.txt",method="libcurl")

  # Download urls
  URLav  <- "https://ndownloader.figshare.com/files/10598158" # url for average flow
  URLmax <- "https://ndownloader.figshare.com/files/10598674" # url for max flow
  URLmin <- "https://ndownloader.figshare.com/files/10609456" # url for min flow

  # file names and folder to save to
  Filename_av  <- "FLO1K.ts.1960.2015.qav.nc"
  Filename_max <- "FLO1K.ts.1960.2015.qma.nc"
  Filename_min <- "FLO1K.ts.1960.2015.qmi.nc"
  Folder <- "../Flow data/"

  # check if files are already downloaded
  exist=FALSE
  for(i in list.files("../Flow data")){
    if(regime=="av" & i==Filename_av){
      exist=TRUE
      break}
    if(regime=="max" & i==Filename_max){
      exist=TRUE
      break}
    if(regime=="min" & i==Filename_min){
      exist=TRUE
      break}
  }

  if(regime=="av" & exist==FALSE)
    download.file(URLav, destfile=paste0(Folder,Filename_av),method="libcurl") # av
  if(regime=="max" & exist==FALSE)
    download.file(URLmax, destfile=paste0(Folder,Filename_max),method="libcurl") # max
  if(regime=="min" & exist==FALSE)
    download.file(URLmin, destfile=paste0(Folder,Filename_min),method="libcurl") # min
}
