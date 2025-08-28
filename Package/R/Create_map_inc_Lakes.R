

Create_map_inc_Lakes <- function(results,lakes_shp){
  # remove lakes points from point results
  results_pts<-results[[1]][!is.na(results[[1]]$C_w),]

  library(leaflet)
  C<-results_pts$C_w

  # points plot colors
  med<-median(C[C!=0], na.rm=T)
  Ccolor<-ifelse(C == 0,"green",0)
  Ccolor<-ifelse(C < med & C > 0,"orange",Ccolor)
  Ccolor<-ifelse(Ccolor == 0,"red",Ccolor)
  Ccolor<-ifelse(is.na(Ccolor),"blue",Ccolor)

  # points plot ID
  results_pts$C2<-as.character(ifelse(is.na(results_pts$C_w),"NA..",results_pts$C_w))
  results_pts$ID_map<-paste("C_w =",results_pts$C2,"<br>",results_pts$ID,"->",results_pts$ID_nxt)

  # match concentrations results[[2]] with lakes polygons
  lakes_shp@data$C_w<-results[[2]]$C_w[match(lakes_shp@data$Hylak_id,results[[2]]$Hylak_id)]

  # remove lake polygons with NAs (lakes not in network)
  lakes_shp<-lakes_shp[!is.na(lakes_shp@data$C_w),]

  # convert coordinates to lat long
  lakes_shp<-spTransform(lakes_shp,CRS("+proj=longlat"))

  # lakes plot colors
  C.l<-lakes_shp@data$C_w
  Ccolor.l<-ifelse(C.l == 0,"green",0)
  Ccolor.l<-ifelse(C.l < med & C.l > 0,"orange",Ccolor.l)
  Ccolor.l<-ifelse(Ccolor.l == 0,"red",Ccolor.l)
  Ccolor.l<-ifelse(is.na(Ccolor.l),"blue",Ccolor.l)

  # lake plot ID
  lakes_shp@data$C2<-as.character(ifelse(is.na(lakes_shp@data$C_w),"NA..",lakes_shp@data$C_w))
  lakes_shp@data$ID_map<-paste("ID =",lakes_shp@data$Hylak_id," <br>  C_w =",lakes_shp@data$C2)

  # plot points and lakes
  m <- leaflet(results_pts) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircles(lng = ~x, lat = ~y, weight = 1,radius = 100, popup = ~ID_map, color = Ccolor)

  m2 <- m %>% addPolygons(data=lakes_shp, opacity = 0.4, weight = 3, color = Ccolor.l, popup = ~ID_map,
                          highlightOptions = highlightOptions(color = "black", weight = 2))
  m2
}

# load_lake_polygons <- function(basin_id){
#
#   options(warn=-1)
#   for (i in 1:length(basin_id)) {
#     if (length(ogrFIDs(path.expand(paste0("../Basin data/Basin_",basin_id[i],"/","Shapefiles_forplot")),paste0("Lakes_",basin_id[i])))!=0) {
#       if (!exists("lakes_shp")) {
#         lakes_shp <- readOGR(path.expand(paste0("../Basin data/Basin_",basin_id[i],"/","Shapefiles_forplot")),paste0("Lakes_",basin_id[i]), stringsAsFactors=FALSE)
#       } else if (exists("lakes_shp")) {
#         lakes_shp2 <- readOGR(path.expand(paste0("../Basin data/Basin_",basin_id[i],"/","Shapefiles_forplot")),paste0("Lakes_",basin_id[i]), stringsAsFactors=FALSE)
#         lakes_shp <- rbind(lakes_shp,lakes_shp2)
#       }
#     }
#   }
#   options(warn=0)
#
#   if (exists("lakes_shp")) {return(lakes_shp)}
# }

