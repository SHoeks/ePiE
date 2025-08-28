InteractiveResultMap = function(results, basin_id){

  # subset and format river data
  river = results$pts
  river = river[river$basin_ID==basin_id,]
  river$Log10_Concentration_ngL = log10(river$C_w*1000)
  river$Log10_Concentration_ngL[is.infinite(river$Log10_Concentration_ngL)] = NA
  lake_points = river[river$Pt_type=="Hydro_Lake",]
  nlakes = length(unique(lake_points$Hylak_id))
  river = river[river$Pt_type!="Hydro_Lake",]

  # generate approximation of lake shapes
  if(nlakes>0) {
    lake_conc = results$hl
    Hylak_id = strsplit(lake_points$ID,"_|-")
    Hylak_id = lapply(Hylak_id,\(x)x[[2]])
    Hylak_id = unlist(Hylak_id)
    lake_points$Hylak_id = as.numeric(Hylak_id)
    lake_conc = lake_conc[lake_conc$Hylak_id %in% lake_points$Hylak_id,]
    lakes_apprx = list()
    for(i in 1:nrow(lake_conc)){
      Hid = lake_conc$Hylak_id[i]
      Hp = lake_points[lake_points$Hylak_id==Hid,]
      Hpsf = sf::st_as_sf(Hp, coords = c("x", "y"), crs = 4326)
      mcp_geom = sf::st_convex_hull(sf::st_union(Hpsf))
      mcp_sf = sf::st_sf(Hylak_id = Hid, C_w = lake_conc$C_w[i], geometry = mcp_geom)
      lakes_apprx[[i]] = mcp_sf
    }
    lakes_apprx = do.call(rbind,lakes_apprx)
    lakes_apprx$Log10_Concentration_ngL = log10(lakes_apprx$C_w*1000)
    lakes_apprx$Log10_Concentration_ngL[is.infinite(lakes_apprx$Log10_Concentration_ngL)] = NA
  }

  # Convert to spatial object
  river_sf = sf::st_as_sf(river, coords = c("x", "y"), crs = 4326)

  # Color scale based on concentration (log10)
  pal = colorRampPalette(c("blue", "red"))
  npal = unique(river$Log10_Concentration_ngL)
  npal = npal[!is.na(npal)]
  npal = length(npal)
  if(nlakes>0){
    npallake = unique(lakes_apprx$Log10_Concentration_ngL)
    npallake = npallake[!is.na(npallake)]
    npallake = length(npallake)
  }


  # Create the map with circle size and color representing concentration
  map = mapview::mapview(
    river_sf,
    zcol = "Log10_Concentration_ngL",
    cex = 2,
    col.regions = pal(npal),
    legend = TRUE
  )
  if (nlakes > 0) {
    map = map + mapview::mapview(
      lakes_apprx,
      zcol = "Log10_Concentration_ngL",
      cex = 2,
      col.regions = pal(npallake),
      legend = FALSE
    )
  }


  # View the map
  return(map)
}



