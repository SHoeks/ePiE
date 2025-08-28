

Make_spatial <- function(results){
  x<-c("sp","raster")
  suppressMessages(lapply(x, require, character.only = TRUE))
  suppressMessages(library(rgdal))
  coordinates(results)<-~x+y
  projection(results) <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
  return(results)
}
