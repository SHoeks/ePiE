

Create_save_multi_maps <- function(results_com,results_lakes,lakes_shp,flow_raster){
  library(leaflet)
  library(htmlwidgets)

  for (i in unique(results_com$API)) {
    # remove lakes points from point results
    results_pts <- results_com[!is.na(results_com$C_w)&results_com$API==i,]
    C<-results_pts$C_w
    results_lakes_sub <- results_lakes[!is.na(results_lakes$C_w) & results_lakes$API==i,]
    C.l <- results_lakes_sub$C_w

    options(warn=-1)
    color_plot <- colorQuantile(c("gold","red"),
                           domain=C[C!=0],probs=seq(0,1,0.01),
                           na.color = "blue")

    Ccolor<-ifelse(C == 0,"forestgreen",color_plot(C))


    # points plot ID
    results_pts$C2 <- as.character(ifelse(is.na(results_pts$C_w),NA,formatC(results_pts$C_w,format="e",digits=2)))
    results_pts$ID_map<- paste("ID =", results_pts$ID, "<br>", "C =",results_pts$C2, "ug/L")

    # match concentrations results_lake_sub with lakes polygons
    lakes_shp@data$C_w<-results_lakes_sub$C_w[match(lakes_shp@data$Hylak_id,results_lakes_sub$Hylak_id)]

    # remove lake polygons with NAs (lakes not in network)
    lakes_shp<-lakes_shp[!is.na(lakes_shp@data$C_w),]

    # convert coordinates to lat long
    lakes_shp<-spTransform(lakes_shp,CRS("+proj=longlat"))

    # lakes plot colors
    C.l<-lakes_shp@data$C_w
    Ccolor.l<-ifelse(C.l == 0,"forestgreen",color_plot(C.l))

    # lake plot ID
    lakes_shp@data$C2<-as.character(ifelse(is.na(lakes_shp@data$C_w),NA,formatC(lakes_shp@data$C_w,format="e",digits=2)))
    lakes_shp@data$ID_map<-paste("ID = lake",lakes_shp@data$Hylak_id," <br>  C =",lakes_shp@data$C2, "ug/L")

    # plot points and lakes
    m <- leaflet(results_pts) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircles(lng = ~x, lat = ~y, weight = 1,radius = 100, popup = ~ID_map, color = Ccolor)

    m2 <- m %>% addPolygons(data=lakes_shp, opacity = 0.4, weight = 3, color = Ccolor.l, popup = ~ID_map,
                            highlightOptions = highlightOptions(color = "black", weight = 2))
    m2
    options(warn=0)
    savewd<-getwd(); setwd("../Results")
    saveWidget(m2, file=paste0(Sys.Date(),"_Results_",
                               substr(flow_raster@file@name, 36, 49),results_pts$API[1],".html"),selfcontained=TRUE)
    setwd(savewd)
  }
}
