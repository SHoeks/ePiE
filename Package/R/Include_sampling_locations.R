
#'
Include_sampling_locations <- function(sl,Basin_select,pts){

  # Download URLs for geographical data (wind speed, temperature)
  URL_wind <- "https://www.esrl.noaa.gov/psd/thredds/fileServer/Datasets/ncep.reanalysis.derived/surface/vwnd.sig995.mon.ltm.nc"
  URL_temp <- "https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.01/cruts.1709081022.v4.01/tmp/cru_ts4.01.2011.2016.tmp.dat.nc.gz"

  #check if netCDF files are already downloaded
  Filename_wind <- "Wind_speed.nc"
  Filename_temp <- "Temperature.nc"

  if (!(Filename_wind%in%list.files("../Geographical data"))) {
    download.file(URL_wind,destfile=paste0("../Geographical data/",Filename_wind),method="libcurl")
  }

  if (!(Filename_temp%in%list.files("../Geographical data"))) {
    download.file(URL_temp,destfile=paste0("../Geographical data/",Filename_temp),method="libcurl")
  }

  #load wind raster and correct projection
  options(warn=-1)
  Wind <- raster(paste0("../Geographical data/",Filename_wind),band=1)
  for (i in 2:12){
    Wind <- Wind+raster(paste0("../Geographical data/",Filename_wind),band=i)
  }
  Wind <- Wind/12
  projection <- "+proj=laea +lat_0=50 +lon_0=11 +x_0=5000000 +y_0=3200000
  +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  projection(Wind) <- projection
  Wind <- unrotate(Wind)
  options(warn=0)

  #load temperature raster and correct projection
  options(warn=-1)
  Temp <- raster(paste0("../Geographical data/",Filename_temp),band=1)
  for (i in 2:72) {
    Temp <- Temp+raster(paste0("../Geographical data/",Filename_temp),band=i)
  }
  Temp <- Temp/72
  projection(Temp) <- projection
  options(warn=-1)

  #load slope (no online download possible)
  slope_dir <- '../Geographical data/PAGER_mean_slope/PAGER_mean_slope.tif'
  slope <- raster(slope_dir)
  projection(slope) <- CRS(projection)

  #load multiple networks
  for (i in 1:length(Basin_select)) {
    if (i==1) {
      network <- readOGR(path.expand(paste0('../Basin data/Basin_',
                                            Basin_select[i],'/Shapefiles_forplot')),paste0("Rivers_",Basin_select[i]), stringsAsFactors=FALSE)
      network@data$basin_id <- Basin_select[i]
    } else {
      network2 <- readOGR(path.expand(paste0('../Basin data/Basin_',
                                            Basin_select[i],'/Shapefiles_forplot')),paste0("Rivers_",Basin_select[i]), stringsAsFactors=FALSE)
      network2@data$basin_id <- Basin_select[i]
      network <- rbind(network,network2)
    }
  }

  rownames(network@data) <- c(1:length(network))

  #adding distance to next point to all points for which dist_nxt==NA
  pts$dist_nxt <- ifelse(is.na(pts$dist_nxt) & !is.na(pts$Dist_down),0,pts$dist_nxt)
  nodistd <- which(is.na(pts$Dist_down) & is.na(pts$dist_nxt) & pts$Pt_type!="MOUTH")

  while (any(is.na(pts$Dist_down[nodistd]))) {
    for (i in nodistd) {
      if (!is.na(pts$Dist_down[pts$ID_nxt %in% pts$ID[i] & pts$basin_id %in% pts$basin_id[i]])) {
        pts$Dist_down[i] <- pts$Dist_down[pts$ID_nxt %in% pts$ID[i] & pts$basin_id %in% pts$basin_id[i]]
      }
    }
  }

  for (i in nodistd) {
    pts$dist_nxt[i] <- pts$Dist_down[pts$ID_nxt %in% pts$ID[i] & pts$basin_id %in% pts$basin_id[i]]  - pts$Dist_down[pts$ID %in% pts$ID_nxt[i] & pts$basin_id %in% pts$basin_id[i]]
  }

  pts$dist_nxt[which(pts$ID_nxt %in% pts$ID[pts$Pt_type=="MOUTH"])] <- pts$Dist_down[which(pts$ID_nxt %in% pts$ID[pts$Pt_type=="MOUTH"])]

  #plot sampling locations
  plot(sl,col="red",pch=2,axes=T,cex=0.7)
  network<-spTransform(network, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
  plot(network,add=T,col="blue",lty="dotted",lwd=1)
  pts_sp<-SpatialPointsDataFrame(pts[,c("x", "y")], pts,proj4string = CRS(projection(network)))
  plot(pts_sp[pts_sp@data$Pt_type=="WWTP" | pts_sp@data$Pt_type=="Agglomerations",],add=T,col="yellow",pch=20)
  suppressWarnings(snap_sl<-snapPointsToLines(points=sl,lines=network))
  plot(snap_sl,add=T,col="black",pch=1)
  idx_seg_points<-which(pts_sp@data$ID%in%network@data$linkId[snap_sl@data$nearest_line_id] &
                          pts_sp@data$basin_id %in% network@data$basin_id[snap_sl@data$nearest_line_id])
  plot(pts_sp[idx_seg_points,],col="purple",add=T,pch=2)

  pts_sp<-spTransform(pts_sp, CRS(projection))
  sl<-spTransform(sl, CRS(projection))
  snap_sl <- spTransform(snap_sl, CRS(projection))

  snap_sl@data$ID <- as.character(snap_sl@data$a)
  snap_sl@data$ID_nxt <- snap_sl@data$Down_type <- snap_sl@data$dist_nxt <- snap_sl@data$Dist_down <- snap_sl@data$basin_id <- NA
  snap_sl@data$lake_out <- snap_sl@data$lake_in <- snap_sl@data$HL_ID_new <- 0
  snap_sl@data$Pt_type <- "MONIT"
  snap_sl@data$up <- NA

  # change this as done in build-basins code: multiple points on same line segment
  for(i in 1:length(snap_sl)){
    idx<-which(pts_sp@data$ID%in%network@data$linkId[snap_sl@data$nearest_line_id[i]] &
                 pts_sp@data$basin_id%in%network@data$basin_id[snap_sl@data$nearest_line_id[i]])
    Di_nxt<-pts_sp@data$dist_nxt[idx]
    Di_upst.point_to_monitpoint<-gDistance(pts_sp[idx,], snap_sl[i,], byid=TRUE)
    if(Di_nxt>Di_upst.point_to_monitpoint){ # if it is the correct upstream network point
      snap_sl@data$ID_nxt[i] <- pts_sp@data$ID_nxt[idx]
      snap_sl@data$Down_type[i] <- pts_sp@data$Down_type[idx]
      snap_sl@data$dist_nxt[i] <- Di_nxt - Di_upst.point_to_monitpoint
      snap_sl@data$Dist_down[i] <- pts_sp@data$Dist_down[idx] - Di_upst.point_to_monitpoint
      snap_sl@data$basin_id[i] <- pts_sp@data$basin_id[idx]
      snap_sl@data$up[i] <- pts_sp@data$ID[idx]

    }else{ # if there is for example a WWTP after the upstream network point, WWTP is upstream network point
      # still requires minor improvements not sure if it works correctly in all scenarios
      check<-T
      while(check==T) {
        idx <- which(pts_sp@data$ID%in%pts_sp@data$ID_nxt[idx] & pts_sp@data$basin_id%in%pts_sp@data$basin_id[idx])
        Di_nxt <- pts_sp@data$dist_nxt[idx]
        Di_upst.point_to_monitpoint<-gDistance(pts_sp[idx,], snap_sl[i,], byid=TRUE)
        if(Di_nxt>Di_upst.point_to_monitpoint){
          snap_sl@data$ID_nxt[i] <- pts_sp@data$ID_nxt[idx]
          snap_sl@data$Down_type[i] <- pts_sp@data$Down_type[idx]
          snap_sl@data$dist_nxt[i] <- Di_nxt - Di_upst.point_to_monitpoint
          snap_sl@data$Dist_down[i] <- pts_sp@data$Dist_down[idx] - Di_upst.point_to_monitpoint
          snap_sl@data$basin_id[i] <- pts_sp@data$basin_id[idx]
          snap_sl@data$up[i] <- pts_sp@data$ID[idx]

          check <- F
        }
      }
    }
  }

  idx <- which(pts$ID%in%snap_sl@data$up & pts$basin_id%in%snap_sl@data$basin_id)

  for (i in idx) {
    sp_id <- which(snap_sl@data$up%in%pts$ID[i] & snap_sl@data$basin_id %in% pts$basin_id[i])
    if (length(sp_id)==1) {
      pts$ID_nxt[i] <- as.character(snap_sl@data$ID[sp_id])
      pts$dist_nxt[i] <- pts$dist_nxt[i] - snap_sl@data$dist_nxt[sp_id]
      pts$Down_type[i] <- "MONIT"
    } else {
    sp_id_hl <- sp_id[order((snap_sl@data$dist_nxt[sp_id]))]
      for (j in 1:length(sp_id)) {
        if (j==1) {
          pts$ID_nxt[i] <- as.character(snap_sl@data$ID[sp_id_hl[j]])
          pts$dist_nxt[i] <- pts$dist_nxt[i] - snap_sl@data$dist_nxt[sp_id_hl[j]]
          pts$Down_type[i] <- "MONIT"
        } else {
          snap_sl@data$ID[sp_id_hl[j-1]] <- as.character(snap_sl@data$ID[sp_id_hl[j]])
          snap_sl$dist_nxt[sp_id_hl[j-1]] <- snap_sl@data$dist_nxt[sp_id_hl[j-1]] - snap_sl@data$dist_nxt[sp_id_hl[j]]
          snap_sl$Down_type[sp_id_hl[j-1]] <- "MONIT"
        }
      }
    }
  }

  #adapt the network points, also when multiple points per line segment
  # add slope, wind, temperature
  snap_sl <- spTransform(snap_sl, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  snap_sl@data$slope<-extract(slope, coordinates(snap_sl))
  snap_sl@data$Wind<-extract(Wind, coordinates(snap_sl))
  snap_sl@data$Wind<-ifelse(is.na(snap_sl@data$Wind),
                                median(snap_sl@data$Wind, na.rm = T),
                                snap_sl@data$Wind)
  snap_sl@data$T_AIR <-extract(Temp, coordinates(snap_sl))
  snap_sl@data$T_AIR<-ifelse(is.na(snap_sl@data$T_AIR),
                            median(snap_sl@data$T_AIR, na.rm = T),
                            snap_sl@data$T_AIR)

  #Add monitoring points to pts and change relevant parameters
  snap_pts <- data.frame(snap_sl@data)
  snap_pts$a <- NULL
  snap_pts$line_node <- paste("P_",snap_sl@data$nearest_line_id,sep="")
  snap_pts$nearest_line_id <- NULL
  snap_pts[c("x","y")] <- snap_sl@coords
  pts <- rbind.fill(pts,snap_pts)
  pts$X <- NULL

  pts<<-pts


}
