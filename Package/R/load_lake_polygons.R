

load_lake_polygons <- function(basin_id,folder){


  options(warn=-1)
  for (i in 1:length(basin_id)) {
    if (length(ogrFIDs(path.expand(paste0(folder,"/Basin_",basin_id[i],"/","Shapefiles_forplot")),paste0("Lakes_",basin_id[i])))!=0) {
      if (!exists("lakes_shp")) {
        lakes_shp <- readOGR(path.expand(paste0(folder,"/Basin_",basin_id[i],"/","Shapefiles_forplot")),paste0("Lakes_",basin_id[i]), stringsAsFactors=FALSE)
      } else if (exists("lakes_shp")) {
        lakes_shp2 <- readOGR(path.expand(paste0(folder,"/Basin_",basin_id[i],"/","Shapefiles_forplot")),paste0("Lakes_",basin_id[i]), stringsAsFactors=FALSE)
        lakes_shp <- rbind(lakes_shp,lakes_shp2)
      }
    }
  }
  options(warn=0)

  if (exists("lakes_shp")) {return(lakes_shp)}
}
