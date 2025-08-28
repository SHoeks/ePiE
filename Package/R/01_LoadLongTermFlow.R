LoadLongTermFlow = function(flow_scenario="average"){

  # check input
  avBool = grepl("average|mean|av",flow_scenario)
  maBool = grepl("ma|max|maximum|high|x",flow_scenario)
  miBool = grepl("mi|min|minimum|low|ini",flow_scenario)

  # load package data
  flow_values = ePiE::flow_values
  flow_index = ePiE::flow_index

  # create template raster based on flow_index
  rast_template = rast(nrows=flow_index$nrows,ncols=flow_index$ncols,
                       xmin=flow_index$xmin,xmax=flow_index$xmax,
                       ymin=flow_index$ymin,ymax=flow_index$ymax)
  rast_template[] = NA
  names(rast_template) = "mean"

  # insert flow values into raster
  if(avBool){
    print("Loading average flow...")
    rast_template[flow_values[["cell"]]] = flow_values[["Qav"]]
  }
  if(maBool){
    print("Loading maximum flow...")
    rast_template[flow_values[["cell"]]] = flow_values[["Qma"]]
  }
  if(miBool){
    print("Loading minimum flow...")
    rast_template[flow_values[["cell"]]] = flow_values[["Qmi"]]
  }


  return(rast_template)

}

# LoadLongTermFlow = function(flow_scenario="average"){
#   libPath = ePiEPath()
#   flow_dir = "flow_lt"
#   tif_files = list.files(glue::glue("{libPath}/{flow_dir}"),pattern="*.tif")
#   avBool = grepl("average|mean|av",flow_scenario)
#   maBool = grepl("ma|max|maximum|high|x",flow_scenario)
#   miBool = grepl("mi|min|minimum|low|ini",flow_scenario)
#   if(avBool){
#     print("Loading average flow...")
#     tif_files = grep("qav",tif_files,value=TRUE)
#   }
#   if(maBool){
#     print("Loading maximum flow...")
#     tif_files = grep("qma",tif_files,value=TRUE)
#   }
#   if(miBool){
#     print("Loading minimum flow...")
#     tif_files = grep("qmi",tif_files,value=TRUE)
#   }
#   tif_files_full = glue::glue("{libPath}/{flow_dir}/{tif_files}")
#   tifs = lapply(tif_files_full,terra::rast)
#   tif_combined = tifs[[1]]
#   for(i in 2:length(tifs)){
#     tif_combined = terra::merge(tif_combined,tifs[[i]])
#   }
#   return(tif_combined)
# }

ePiEPath = function() {
  lib_name = 'ePiE'
  lib_paths = .libPaths()
  libPath = paste0(lib_paths[grep(lib_name,lapply(lib_paths,list.files))][1],'/',lib_name)
  return(libPath)
}
