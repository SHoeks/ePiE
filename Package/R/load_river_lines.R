

load_river_lines <- function(basin_id,folder){

  options(warn=-1)
  for (i in 1:length(basin_id)) {
    if (length(ogrFIDs(path.expand(paste0(folder,"/Basin_",basin_id[i],"/","Shapefiles_forplot")),paste0("Rivers_",basin_id[i])))!=0) {
      if (!exists("shp")) {
        shp <- readOGR(path.expand(paste0(folder,"/Basin_",basin_id[i],"/","Shapefiles_forplot")),paste0("Rivers_",basin_id[i]), stringsAsFactors=FALSE)
      } else if (exists("shp")) {
        shp2 <- readOGR(path.expand(paste0(folder,"/Basin_",basin_id[i],"/","Shapefiles_forplot")),paste0("Rivers_",basin_id[i]), stringsAsFactors=FALSE)
        shp <- rbind(shp,shp2)
      }
    }
  }
  options(warn=0)

  if (exists("shp")) {return(shp)}
}
