# shiny::shinyApp(
#     ui = shiny::fluidPage(shiny::leafletOutput("map",height=700),
#                    actionButton("do",label="Select basins & Quit"),
#                    actionButton("quit",label="Quit")
#     ),

#     server = function(input,output) {

#       #create empty vector for basins selected
#       Basins <- reactiveValues(basin_id=NULL)

#       #create base map
#       output$map <- renderLeaflet({
#         leaflet(Basin_available) %>%
#           addProviderTiles(providers$CartoDB.Positron) %>%
#           addPolygons(data=Basin_available, opacity = 0.4, weight = 3, color = "grey",
#                       highlightOptions = highlightOptions(color = "black", weight = 2), layerId = ~BASIN_ID) %>%
#           addDrawToolbar(
#             polylineOptions=FALSE,
#             markerOptions = FALSE,
#             polygonOptions = FALSE,
#             rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
#                                                                                   ,color = 'white'
#                                                                                   ,weight = 1)),
#             circleOptions = FALSE,
#             editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions()))

#       })

#       #upon click, turn basins green and add their IDs to the Basins vector; or deselect on 2nd click
#       observeEvent(input$map_shape_click, {

#         if(input$map_shape_click$id %in% Basins$basin_id) {
#           leafletProxy("map") %>%
#             removeShape(input$map_shape_click$id) %>%
#             addPolygons(data=Basin_available[Basin_available$BASIN_ID%in%input$map_shape_click$id,], opacity = 0.4, weight = 3, color = "grey",
#                         highlightOptions = highlightOptions(color = "black", weight = 2),layerId = ~BASIN_ID)

#           Basins$basin_id <- Basins$basin_id[Basins$basin_id!=input$map_shape_click$id]

#         } else {
#           leafletProxy("map") %>%
#             removeShape(input$map_shape_click$id) %>%
#             addPolygons(data=Basin_available[Basin_available$BASIN_ID%in%input$map_shape_click$id,], opacity = 0.4, weight = 3, color = "green",
#                         highlightOptions = highlightOptions(color = "black", weight = 2),layerId = ~BASIN_ID)

#           Basins$basin_id <- c(Basins$basin_id,input$map_shape_click$id)

#         }
#       })

#       observeEvent(input$map_draw_new_feature, {
#         #create spatial polygons from drawn shape
#         coords <- input$map_draw_new_feature$geometry$coordinates[[1]]
#         p <- Polygon(do.call(rbind,lapply(coords,function(x){c(x[[1]][1],x[[2]][1])})))
#         ps <- Polygons(list(p),1)
#         sps <- SpatialPolygons(list(ps))
#         proj4string(sps) <- crs(projection(Basin_available))

#         # list of basin IDs that are within shape
#         Basin_shape <- Basin_available@data$BASIN_ID[gContains(sps,Basin_available,byid = TRUE)]

#         #first make sure that Basin_shape is not empty (at least one basins is inside shape)
#         if (length(Basin_shape)!=0) {
#           #check if all basins within shape were already selected; if yes: deselect all
#           if (all(Basin_shape %in% Basins$basin_id)) {
#             leafletProxy("map") %>%
#               removeShape(Basin_shape) %>%
#               addPolygons(data=Basin_available[Basin_available$BASIN_ID%in%Basin_shape,], opacity = 0.4, weight = 3, color = "grey",
#                           highlightOptions = highlightOptions(color = "black", weight = 2),layerId =  ~BASIN_ID)

#             Basins$basin_id <- Basins$basin_id[!(Basins$basin_id%in%Basin_shape)]

#           #then check if none of the basins within shape were already selected; if yes: select all
#           } else if (!any(Basin_shape%in% Basins$basin_id)) {
#             leafletProxy("map") %>%
#               removeShape(Basin_shape) %>%
#               addPolygons(data=Basin_available[Basin_available$BASIN_ID%in%Basin_shape,], opacity = 0.4, weight = 3, color = "green",
#                           highlightOptions = highlightOptions(color = "black", weight = 2),layerId =  ~BASIN_ID)

#             Basins$basin_id <- c(Basins$basin_id,Basin_shape)
#             #means that some shapes are within shape, others not; select all that are not
#           } else {
#             for (id in Basin_shape) {
#               if (!(id %in% Basins$basin_id)) {
#                 leafletProxy("map") %>%
#                   removeShape(id) %>%
#                   addPolygons(data=Basin_available[Basin_available$BASIN_ID%in%id,], opacity = 0.4, weight = 3, color = "green",
#                               highlightOptions = highlightOptions(color = "black", weight = 2),layerId = ~BASIN_ID)
#                 Basins$basin_id <- c(Basins$basin_id,id)

#               }

#             }

#           }

#         }

#         #ugly workaround to remove the shapes drawn
#         leafletProxy("map") %>%
#           removeDrawToolbar(clearFeatures=TRUE) %>%
#           addDrawToolbar(
#             polylineOptions=FALSE,
#             markerOptions = FALSE,
#             polygonOptions = FALSE,
#             rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
#                                                                                   ,color = 'white'
#                                                                                   ,weight = 1)),
#             circleOptions = FALSE,
#             editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions()))

#       })

#       observeEvent(input$do, {
#         Basin_select <<- as.numeric(Basins$basin_id)
#         stopApp()
#       })

#       observeEvent(input$quit, {stopApp()})

#     }

#   )


