

Write_spatial_file <- function(file,wd,chem,basin_id,flow_raster){
  td <- file.path(wd,"../Results",sep="");
  options(warn=-1)
  writeOGR(spatial_results, td, paste(Sys.Date(),"_",substr(flow_raster@file@name, 36, 49),"_",chem$API[1],"_Basin_",basin_id,sep=""),
           driver="ESRI Shapefile",overwrite_layer=TRUE)
  options(warn=0)
}
