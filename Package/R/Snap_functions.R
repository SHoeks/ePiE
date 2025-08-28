

# nearest point on segment
nearestPointOnSegment <- function(s, p){
  ap = c(p[1] - s[1,1], p[2] - s[1,2])
  ab = c(s[2,1] - s[1,1], s[2,2] - s[1,2])
  t = sum(ap*ab) / sum(ab*ab)
  t = ifelse(t<0,0,ifelse(t>1,1,t))
  x = s[1,1] + ab[1] * t
  y = s[1,2] + ab[2] * t
  c(x, y, (x-p[1])^2 + (y-p[2])^2)  # Return nearest point and distance
}



# nearest point on line
nearestPointOnLine <- function(coordsLine, coordsPoints){
  nearest_points = vapply(2:nrow(coordsLine),
                          function(x)
                            nearestPointOnSegment(coordsLine[(x-1):x,], coordsPoints),
                          FUN.VALUE=c(0,0,0))

  # Return coordinates of the nearest point in this line
  nearest_points[1:2, which.min(nearest_points[3,])]
}



# snap point(s) to line function
snapPointsToLines <- function( points, lines, maxDist=NA, withAttrs=TRUE) {

  require("rgeos")

  if (!is.na(maxDist)) {
    w = gWithinDistance(points, lines, dist=maxDist, byid=TRUE)
    validPoints = apply(w,2,any)
    validLines = apply(w,1,any)
    points = points[validPoints,]
    lines =  lines[validLines,]
  }

  d = gDistance(points, lines, byid=TRUE)
  nearest_line_index = apply(d, 2, which.min) # Position of each nearest line in lines object

  coordsLines = coordinates(lines)
  coordsPoints = coordinates(points)

  # Get coordinates of nearest points lying on nearest lines
  mNewCoords = vapply(1:length(points),
                      function(x)
                        nearestPointOnLine(coordsLines[[nearest_line_index[x]]][[1]],
                                           coordsPoints[x,]), FUN.VALUE=c(0,0))

  # Recover lines' Ids (Ids and index differ if maxDist is given)
  if (!is.na(maxDist)) nearest_line_id = as.numeric(rownames(d)[nearest_line_index])+1
  else nearest_line_id = nearest_line_index

  # Create data frame and sp points
  if (withAttrs) df = cbind(points@data, nearest_line_id)
  else df = data.frame(nearest_line_id, row.names=names(nearest_line_index))

  SpatialPointsDataFrame(coords=t(mNewCoords), data=df,
                         proj4string=CRS(proj4string(points)))
}



Set_monit <- function(pts,upID,D_upID,sl){

  closest<-upID
  dist<-D_upID
  monit_name<-sl@data$a

  pts$monitID1<-pts$monitID2<-pts$monitID3<-NA
  pts$D1<-pts$D2<-pts$D3<-NA

  for(ii in 1:length(closest)){
    if(is.na(pts$monitID1[pts$ID_new==closest[ii]])){
      pts$monitID1[pts$ID_new==closest[ii]]<-paste(monit_name[ii])
      pts$D1[pts$ID_new==closest[ii]]<-dist[ii]
    }else{
      pts$monitID2[pts$ID_new==closest[ii]]<-paste(monit_name[ii])
      pts$D2[pts$ID_new==closest[ii]]<-dist[ii]
    }
  }

  pts$monitID3<-paste(pts$ID_new,"_points",sep="_")
  pts$D3<-0.1
  return(pts)
}



Set_eqdist <- function(pts_cl,pts,id){

  packages<-c("sp","raster","rgeos","rgdal","geosphere")
  suppressMessages(lapply(packages, require, character.only = TRUE))

  closest<-pts_cl@data$ID_new
  pts$monitID1<-pts$monitID1<-pts$monitID3<-NA
  pts$monitID1[pts$ID_new%in%closest]<-paste(id,1,sep="_")
  pts$monitID2[pts$ID_new%in%closest]<-paste(id,2,sep="_")
  pts$monitID3[pts$ID_new%in%closest]<-paste(id,3,sep="_")
  pts$D1<-pts$D2<-pts$D3<-NA
  pts$D1[pts$ID_new%in%closest]<-0.1
  pts$D2[pts$ID_new%in%closest]<-1
  pts$D3[pts$ID_new%in%closest]<-10
  return(pts)
}




# Snap and plot sampling locations
Locate_sampling_locations <- function(sl,network,pts2,plot=T){

  if(plot){plot(sl,col="red",pch=2,axes=T,cex=0.7)}
  network<-spTransform(network, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
  if(plot){plot(network,add=T,col="blue",lty="dotted",lwd=1)}
  pts_sp<-SpatialPointsDataFrame(pts2[,c("x", "y")], pts,proj4string = CRS(projection(network)))
  #plot(pts_sp,add=T,col="blue",pch=20)

  # Find distances to pts upsteam
  suppressWarnings(snap_sl<-snapPointsToLines(sl,network,100))
  if(plot){plot(snap_sl,add=T,col="black",pch=1)} # plot snapped points
  if(plot){plot(network[snap_sl@data$nearest_line_id,],add=T,col="orange",lwd=4)} # relevent segments
  idx_seg_points<-which(pts_sp@data$ID_new%in%network@data[snap_sl@data$nearest_line_id,])
  if(plot){plot(pts_sp[idx_seg_points,],col="purple",add=T,pch=2)} # relevant upstream points (of selected segment)

  projection <- "+proj=laea +lat_0=50 +lon_0=11 +x_0=5000000 +y_0=3200000
  +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  pts_sp<-spTransform(pts_sp, CRS(projection))
  sl<-spTransform(sl, CRS(projection))

  monit.D<-c()
  monit.upID<-c()
  for(i in 1:length(sl)){
    idx<-which(pts_sp@data$ID_new%in%network@data[snap_sl@data$nearest_line_id[i],])
    Di_nxt<-pts_sp@data$dist_nxt[idx]
    Di_upst.point_to_monitpoint<-gDistance(pts_sp[idx,], sl[i,], byid=TRUE)
    if(Di_nxt>Di_upst.point_to_monitpoint){ # if it is the correct upstream network point
      monit.D<-c(monit.D,Di_upst.point_to_monitpoint)
      monit.upID<-c(monit.upID,pts_sp@data$ID_new[idx])
    }else{ # if there is for example a WWTP after the upstream network point, WWTP is upstream network point
      # still requires minor improvements not sure if it works correctly in all scenarios
      monit.upID<-c(monit.upID,pts_sp@data$ID_DOWN[idx])
      monit.D<-c(monit.D,Di_upst.point_to_monitpoint-Di_nxt)
    }
  }
  monit.D<<-monit.D
  monit.upID<<-monit.upID
}


