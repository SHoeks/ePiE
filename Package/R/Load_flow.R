Load_flow <- function(year,regime){

  year_cor <-year-1959
  if (year_cor>56 | year_cor<1) {stop("incorrect year selected")}
  if (regime=="av" | regime=="max" | regime=="min") {stop=F}else{stop=T}
  if (stop) {stop("incorrect regime, select: av, max, min")}

  # file names and folder to save to
  Filename_av  <- "FLO1K.ts.1960.2015.qav.nc"
  Filename_max <- "FLO1K.ts.1960.2015.qma.nc"
  Filename_min <- "FLO1K.ts.1960.2015.qmi.nc"
  Folder <- "../Flow data/"
  options(warn=-1)

  # load raster using regime and year
  if(regime=="av")
    raster1<-raster(file.path("../Flow data", Filename_av), band = year_cor)
  if(regime=="max")
    raster1<-raster(file.path("../Flow data", Filename_max), band = year_cor)
  if(regime=="min")
    raster1<-raster(file.path("../Flow data", Filename_min), band = year_cor)
  options(warn=0)

  # crop raster using extent pts
  raster2<-crop(raster1, extent(min(pts$x)-1,max(pts$x)+1,min(pts$y)-1,max(pts$y)+1))

  # remove NaNs and NAs
  raster2[is.na(raster2)] <- 0
  raster2[is.nan(raster2)] <- 0

  return(raster2)

}
