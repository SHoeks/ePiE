

Select_basin_pts3 = function(basin_id,folder,version="3"){

  # testing
  # basin_id = c(204206,124863)
  # folder = "../Basin data/"

  return(Select_basin_pts3_v2(basin_id,folder,"pts",version))
}



Select_basin_HL = function(basin_id,folder,version=""){

  # testing
  # basin_id = c(204206,124863)
  # folder = "../Basin data/"

  return(Select_basin_pts3_v2(basin_id,folder,"HL",version))
}



Select_basin_pts3_v2 = function(basin_id,folder,data_select,version=""){



  if(data_select=="pts"){
    pts <- c()
    pts_file = paste0(data_select,version,".csv")
    for (i in 1:length(basin_id)) {
      ll = list.files(paste0(folder,"Basin_",basin_id[i],"/"))
      if(is.na(match(pts_file,ll))) {
        pts_file = "pts.csv"
      }
      print(paste("loading",pts_file))
      pts3 <- read.csv(paste0(folder,"Basin_",basin_id[i],"/",pts_file),stringsAsFactors=FALSE)    # River network
      pts3$basin_id <- basin_id[i]
      pts <- rbind.fill(pts,pts3)
    }
    pts$basin_id = as.numeric(as.character(pts$basin_id))
    return(pts)
  }else{
    HL <- c()
    HL_file = paste0(data_select,version,".csv")
    for (i in 1:length(basin_id)) {
      ll = list.files(paste0(folder,"Basin_",basin_id[i],"/"))
      if(is.na(match(HL_file,ll))) HL_file = "HL.csv"
      print(paste("loading",HL_file))
      HL2  <- read.csv(paste0(folder,"Basin_",basin_id[i],"/",HL_file),stringsAsFactors=FALSE)     # Hydro_lakes
      if (nrow(HL2)!=0) HL2$basin_id <- basin_id[i]
      HL <- rbind.fill(HL,HL2)
    }
    HL$basin_id <- as.numeric(as.character(HL$basin_id))
    return(HL)
  }
}
