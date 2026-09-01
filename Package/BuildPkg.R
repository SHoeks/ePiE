setwd("/Users/osx/Documents/GitHub/ePiE/Package")

if(FALSE){
  detach("package:ePiE", unload=TRUE)
  remove.packages("ePiE")
  devtools::document()

  # load packages
  packages = c("Rcpp","terra","sf","mapview") # add jsonlite?
  for(x in packages) usethis::use_package(x, min_version = TRUE)

  # add data to pkg
  if(FALSE){

    # pts and hl
    pts = readRDS("../Inputs/2025_08_29/data_export_2025_08_15/rds/pts_all_basins.rds")
    hl = readRDS("../Inputs/2025_08_29/data_export_2025_08_15/rds/hl_all_basins.rds")
    #cbind(hl$Res_time,hl$HRT_sec/3600/24)
    usethis::use_data(pts, overwrite = TRUE)
    usethis::use_data(hl, overwrite = TRUE)
    rm(pts)
    rm(hl)

    # basin shp file
    basins = readRDS("../Inputs/2025_08_29/data_export_2025_08_15/rds/basin_shp_sf.rds")
    usethis::use_data(basins, overwrite = TRUE)
    rm(basins)

    # flow
    flow_values = readRDS("../Inputs/2025_08_29/data_export_2025_08_15/rds/flo1k_6015_rastValues.rds")
    flow_index = readRDS("../Inputs/2025_08_29/data_export_2025_08_15/rds/flo1k_6015_rastPropertie.rds")
    usethis::use_data(flow_values, overwrite = TRUE)
    usethis::use_data(flow_index, overwrite = TRUE)
    rm(flow_values)
    rm(flow_index)

    # add eu wide most relevant basins
    eu_basin_ids = readRDS("../Inputs/2025_08_29/relevant_basin_ids_europe.rds")
    usethis::use_data(eu_basin_ids, overwrite = TRUE)
    rm(eu_basin_ids)

  }

  # build package
  usethis::use_build_ignore(c("inst/test","inst/flow_lt","inst/basin_db"))
  devtools::document()
  devtools::build()
  devtools::build(binary = TRUE, args = c('--preclean'))

  # install for testing
  rpkg_ext = "*.tar.gz"
  if(Sys.info()["sysname"]=="Darwin"){
    rpkg_ext = "*.tgz"
  }
  if(Sys.info()["sysname"]=="Windows"){
    rpkg_ext = "*.zip"
  }
  pkg_files = list.files("../",pattern=rpkg_ext,full.names=TRUE)
  pkg_files = grep("ePiE",pkg_files,value=TRUE)
  print(pkg_files)
  print(pkg_files[length(pkg_files)])
  install.packages(pkg_files[length(pkg_files)],repos = NULL)
}

# load function for testing
if(FALSE){
  devtools::load_all()
}




