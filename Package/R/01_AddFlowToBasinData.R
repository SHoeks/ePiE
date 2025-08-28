AddFlowToBasinData = function(basin_data,flow_rast){

  # extract pts
  pts = basin_data$pts

  # add flow to pts
  pts = Add_new_flow_fast(pts=pts,flow_raster=flow_rast)

  # Set hydrology
  pts = Select_hydrology_fast2(pts)

  # return pts list
  basin_data$pts = pts
  return(basin_data)
}

Get_LatLong_crs = function(){
  return("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
}

Add_new_flow_fast = function(pts, flow_raster){

  # set projection
  crs = Get_LatLong_crs()

  # project pts
  p = sf::st_as_sf(pts,coords=c("x","y"),crs=crs)

  # add flow
  imported_raster = flow_raster
  terra::crs(imported_raster) = crs
  Q__NEW = terra::extract(imported_raster, p)
  pts$Q__NEW = Q__NEW[,2]

  # assign Q of line rather than point to monitoring points that are upstream of junctions
  loop_indices = which(grepl("MONIT|WWTP|Agglomerations",pts$Pt_type))
  for (i in loop_indices) {
    if(pts$Down_type[i]=="JNCT"){
      idx = which(pts$ID == pts$line_node[i] & pts$basin_id == pts$basin_id[i])
      pts$Q__NEW[i] = pts$Q__NEW[idx]
    }
  }

  # return data
  return(pts)
}

Select_hydrology_fast2 = function(pts) {

  if(length(unique(pts$basin_id))==1){
    pts2 = list()
    pts2[[1]] = pts
  }else{
    pts2 <- split(pts,f=pts$basin_id)
  }
  pts3 <- c()



  for (b in 1:length(pts2)) {
    pts <- pts2[[b]]

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

    # xlim = c(pts$x[i]-0.2,pts$x[i]+0.2)
    # ylim = c(pts$y[i]-0.2,pts$y[i]+0.2)
    # plot(pts$x,pts$y,xlim=xlim,ylim=ylim)
    # points(pts$x[i],pts$y[i],col="red")
    for (i in nodistd) {
      # if(length(which(pts$ID %in% pts$ID_nxt[i]))==0){
      #   pts$dist_nxt[i] = 0
      #   pts$Pt_type[i] = "MOUTH"
      # }else{
      pts$dist_nxt[i] <- pts$Dist_down[pts$ID_nxt %in% pts$ID[i] & pts$basin_id %in% pts$basin_id[i]]  - pts$Dist_down[pts$ID %in% pts$ID_nxt[i] & pts$basin_id %in% pts$basin_id[i]]
      # }
    }

    pts$dist_nxt[which(pts$ID_nxt %in% pts$ID[pts$Pt_type=="MOUTH"])] <- pts$Dist_down[which(pts$ID_nxt %in% pts$ID[pts$Pt_type=="MOUTH"])]

    pts$Q <- pts$Q__NEW
    pts$Q__NEW <- NULL

    #adding Q of upstream point to any point in river that has Q of 0 or NA
    pts$Q[is.na(pts$Q)] <- 0
    nf <- which(pts$Q==0)
    f <- length(nf)
    while (length(nf) > 0) {
      for (i in nf) {
        if (pts$Q[i] == 0 & !any(pts$Q[which(pts$ID_nxt == pts$ID[i])] == 0) & pts$Pt_type[i]!="START") {
          pts$Q[i] <- sum(pts$Q[which(pts$ID_nxt == pts$ID[i])])
          f <- f - 1
        }
      }
      if (f == length(nf)) {break} #if no updates were possible anymore, break
      nf <- which(pts$Q==0) #update vector with zero flow nodes

    }

    #check whether any nodes in network are still without flow because there is a start point with zero flow;
    #fill those with downstream flow
    while (length(nf) > 0) {
      for (i in nf) {
        if (pts$Q[i]==0 & any(pts$Q[which(pts$ID==pts$ID_nxt[i])] != 0) & pts$Pt_type[i]!="MOUTH") {
          pts$Q[i] <- pts$Q[which(pts$ID==pts$ID_nxt[i])]
          f <- f - 1
        }
      }
      if (f == length(nf)) {break} #if no updates were possible anymore, break
      nf <- which(pts$Q==0) #update vector with zero flow nodes
    }

    #if there are any nodes with zero flow left, these are complete branches (START to MOUTH)
    #final option is to check whether any junctions are present in that branch of which the upstream flow of other branch can be used to fill up
    while (length(nf) > 0) {
      for (i in nf) {
        if (pts$Q[i]==0 & any(pts$Q[which(pts$ID_nxt == pts$ID[i])] != 0) & pts$Pt_type[i]!="START") {
          pts$Q[i] <- sum(pts$Q[which(pts$ID_nxt==pts$ID[i])])
          f <- f - 1
        } else if (pts$Q[i]==0 & any(pts$Q[which(pts$ID==pts$ID_nxt[i])] != 0) & pts$Pt_type[i]!="MOUTH") {
          pts$Q[i] <- pts$Q[which(pts$ID==pts$ID_nxt[i])]
          f <- f - 1
        }
      }
      if (f == length(nf)) {break} #if no updates were possible anymore, break
      nf <- which(pts$Q==0) #update vector with zero flow nodes
    }

    #still nodes without flow? error message that calculation is not possible
    try(if(any(pts$Q==0)) stop(paste0("Prediction not possible due to insufficient flow data for ",pts$basin_id[1])))

    #same as above, now for slope
    pts$slope[is.na(pts$slope)] <- 0
    ns <- which(pts$slope==0)
    s <- length(ns)
    index_next_point = list()
    for (i in ns) index_next_point[[i]] = which(pts$ID[i]==pts$ID_nxt)

    while (length(ns) > 0) {

      for (i in ns) {

        #// SLOW CODE
        index_next_point2 = index_next_point[[i]]
        if (pts$slope[i] == 0 & !any(pts$slope[index_next_point2] == 0) & pts$Pt_type[i]!="START") {
          pts$slope[i] <- mean(pts$slope[index_next_point2])
          s <- s - 1
        }


      }
      if (s == length(ns)) {break} #if no updates were possible anymore, break
      ns <- which(pts$slope==0) #update vector with zero slope nodes
    }

    index_prev_point = list()
    for (i in ns) index_prev_point[[i]] = which(pts$ID_nxt[i]==pts$ID)

    while (length(ns) > 0) {
      for (i in ns) {

        #// SLOW CODE
        index_prev_point2 = index_prev_point[[i]]
        if (pts$slope[i]==0 & any(pts$slope[index_prev_point2] != 0) & pts$Pt_type[i]!="MOUTH") {
          pts$slope[i] <- pts$slope[index_prev_point2]
          s <- s - 1
        }

      }
      if (s == length(ns)) {break} #if no updates were possible anymore, break
      ns <- which(pts$slope==0) #update vector with zero slope nodes

    }

    while (length(ns) > 0) {
      for (i in ns) {
        if (pts$slope[i]==0 & any(pts$slope[which(pts$ID_nxt == pts$ID[i])] != 0) & pts$Pt_type[i]!="START") {
          pts$slope[i] <- mean(pts$slope[which(pts$ID_nxt==pts$ID[i])])
          s <- s - 1
        } else if (pts$slope[i]==0 & any(pts$slope[which(pts$ID==pts$ID_nxt[i])] != 0) & pts$Pt_type[i]!="MOUTH") {
          pts$slope[i] <- pts$slope[which(pts$ID==pts$ID_nxt[i])]
          s <- s - 1
        }
      }
      if (s == length(ns)) {break} #if no updates were possible anymore, break
      ns <- which(pts$slope==0) #update vector with zero slope nodes
    }

    try(if(any(pts$slope==0)) stop(paste0("Prediction not possible due to insufficient slope data for ",pts$basin_id[1])))

    #Calculation local hydrology
    #Manning's roughness coefficient (s*m-1/3), 0.045 as proposed by Pistocchi and Pennington (2006)
    n <- 0.045
    #Slope of river (m/m)
    slope_m <- tan(pts$slope * pi / 180)
    #River river width (m), from Pistocchi and Pennington (2006)
    W <- 7.3607 * pts$Q ^ 0.52425
    #Flow velocity (m/s), Manning-Strickler equation adapted according to Pistocchi and Pennington (2006)
    pts$V <- n ^ (-3/5) * pts$Q ^ (2/5) * W ^ (-2/5) * slope_m ^ (3/10)
    #River depth (m), power equation as derived by Pistocchi and Pennington (2006)
    pts$H <- pts$Q / (pts$V * W)
    #Shear velocity over distance D (m/s)
    V_s <- sqrt(9.80665 * pts$H * slope_m)
    #Lateral dispersion coefficient (m2/s)
    Dy <- 0.6 * pts$H * V_s
    #Vertical mixing coefficient (m2/s)
    Ez <- 0.07 * pts$H * V_s
    #Length of vertical mixing zone for discharges into a stream from its side (m) (Sajer 2013)
    D_MIX_vert  <- (0.4 * pts$V * pts$H ^ 2) / Ez
    #Length of transverse mixing zone for discharges into a stream from its side (m) (Sajer 2013)
    D_MIX_trans <- (0.4 * W ^ 2 * pts$V) / Dy

    #Average flow velocity over distance to next point
    #set Q_down: if next point is not sea, lake or junction, then Q_down==Q__NEW, else Q_down == Q__NEW of next point
    pts$V_NXT <- pts$V

    #// SLOW CODE
    loop_idx = which(!is.na(pts$ID_nxt) & pts$Down_type != "Hydro_Lake" & pts$Down_type != "JNCT")
    idx_next = match(pts$ID_nxt,pts$ID)
    V_nxt_tmp = pts$V[idx_next]
    pts$V_NXT[loop_idx] = ( pts$V[loop_idx] + V_nxt_tmp[loop_idx] ) / 2


    pts3 <- rbind(pts3,pts)
  }

  return(pts3)


}
