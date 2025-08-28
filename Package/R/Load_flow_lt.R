

Load_flow_lt <-function(regimes,flow_dir,pts){

  for (i in regimes) {
    if (i=="av" | i=="max" | i=="min") {stop=F}else{stop=T}
    if (stop) {stop("incorrect regime, select: av, max, min")}
  }
  raster_list <- list()

  # file names and folder to save to
  Filenames = list.files(flow_dir,pattern="*.tif$",full.names=TRUE)


  for (i in regimes) {

    if(i=="av") i_grep = "qav"
    if(i=="max") i_grep = "qma"
    if(i=="min") i_grep = "qmi"
    Filename_load  <- grep(i_grep,Filenames,value=TRUE)
    cat(i,i_grep,Filename_load,"\n")

    # load raster using regime
    raster1 = raster(Filename_load)

    # crop raster using extent pts
    raster2<-crop(raster1, extent(min(pts$x)-1,max(pts$x)+1,min(pts$y)-1,max(pts$y)+1))

    # remove NaNs and NAs
    raster2[is.na(raster2)] <- 0
    raster2[is.nan(raster2)] <- 0

    raster_list[[i]] <- raster2

  }
  return(raster_list)

}
