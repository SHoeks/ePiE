

Create_map <- function(results){
  library(leaflet)
  C<-results$C_w
  med<-median(C[C!=0], na.rm=T)
  Ccolor<-ifelse(C == 0,"green",0)
  Ccolor<-ifelse(C < med & C > 0,"orange",Ccolor)
  Ccolor<-ifelse(Ccolor == 0,"red",Ccolor)
  Ccolor<-ifelse(is.na(Ccolor),"blue",Ccolor)


  results$C2<-as.character(ifelse(is.na(results$C_w),"NA..",results$C_w))
  results$ID_map<-paste(results$C2,"-",results$ID,"->",results$ID_nxt)

  leaflet(results) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircles(lng = ~x, lat = ~y, weight = 1,radius = 100, popup = ~ID_map, color = Ccolor)

}
