

Write_spatial_file_parallel <- function(file,dir,chem,regime){
  options(warn=-1)
  writeOGR(file, dir, paste(Sys.Date(),"_","flow_",regime,"_",chem$API[1],sep=""),
           driver="ESRI Shapefile",overwrite_layer=TRUE)
  options(warn=0)
}
