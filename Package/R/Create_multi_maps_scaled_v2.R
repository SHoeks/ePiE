CreateHTMLMaps = function(results_flow_per_regime,regime,dir){

  #set plot colors
  color_plot = colorNumeric(c("#10E500","#03E53F","#07E68E","#0BE7DB","#0EAAE8","#1262E9","#161DE9","#5A1AEA","#A21EEB","#E922EC","#ED26AB"),
                              domain=c(-8,2),na.color = "blue")

  for (i in unique(results_flow_per_regime$API)) {
    # remove lakes points from point results
    results_pts <- results_flow_per_regime[!is.na(results_flow_per_regime$C_w)&results_flow_per_regime$API==i,]
    C<-log10(results_pts$C_w)
    C[is.infinite(C)] <- -999
    options(warn=-1)
    Ccolor<-ifelse(C < -8,"#10E500",ifelse(C > 2,"#ED26AB",color_plot(C[C >= -8 & C <= 2])))

    # points plot ID
    results_pts$C2 <- as.character(ifelse(is.na(results_pts$C_w),NA,formatC(results_pts$C_w,format="e",digits=2)))
    results_pts$ID_map<- paste("ID =", results_pts$ID, "<br>", "Pt type =", results_pts$Pt_type, "<br>", "C =",results_pts$C2, "ug/L")

    # if (!is.null(results_lakes)) {

      # results_lakes_sub <- results_lakes[!is.na(results_lakes$C_w) & results_lakes$API==i,]
      # C.l <- log10(results_lakes_sub$C_w)
      # C.l[is.infinite(C.l)] <- -999

      # # match concentrations results_lake_sub with lakes polygons
      # lakes_shp@data$C_w<-results_lakes_sub$C_w[match(lakes_shp@data$Hylak_id,results_lakes_sub$Hylak_id)]
      # # remove lake polygons with NAs (lakes not in network)
      # lakes_shp<-lakes_shp[!is.na(lakes_shp@data$C_w),]
      # # convert coordinates to lat long
      # lakes_shp<-spTransform(lakes_shp,CRS("+proj=longlat"))

      # lakes plot colors
      # Ccolor.l <- ifelse(C.l < -8,"#10E500",ifelse(C.l > 2,"#ED26AB",color_plot(C.l[C.l >= -8 & C.l <= 2])))

      # # lake plot ID
      # lakes_shp@data$C2<-as.character(ifelse(is.na(lakes_shp@data$C_w),NA,formatC(lakes_shp@data$C_w,format="e",digits=2)))
      # lakes_shp@data$ID_map<-paste("ID = lake",lakes_shp@data$Hylak_id," <br>  C =",lakes_shp@data$C2, "ug/L")
    # }

    # plot points and lakes
    m <- leaflet::leaflet(results_pts) %>%
      leaflet::addProviderTiles(providers$CartoDB.Positron) %>%
      leaflet::addCircles(lng = ~x, lat = ~y, weight = 1,radius = 100, popup = ~ID_map, color = Ccolor)

    # if (!is.null(results_lakes)) {
    #   m2 <- m %>% leaflet::addPolygons(data=lakes_shp, opacity = 0.4, weight = 3, color = Ccolor.l, popup = ~ID_map,
    #                           highlightOptions = leaflet::highlightOptions(color = "black", weight = 2))
    # } else {
      m2 <- m
    # }

    options(warn=0)
    saveWD = getwd()
    setwd(dir)
    htmlwidgets::saveWidget(m2, file=paste0(Sys.Date(),"_Results_flow_",regime,"_",i,".html"),selfcontained=TRUE)
    setwd(saveWD)
  }
}
