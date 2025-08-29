setwd("C:/Users/Selwyn Hoeks/Documents/GitHub/ePiE_Rpackage/Package")

if(FALSE){
  detach("package:ePiE", unload=TRUE)
  remove.packages("ePiE")
  devtools::document()

  # Load packages
  packages = c("Rcpp","terra","sf","mapview")
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

  }

  # build package
  usethis::use_build_ignore(c("inst/test","inst/flow_lt","inst/basin_db"))
  devtools::document()
  devtools::build()
  devtools::build(binary = TRUE, args = c('--preclean'))

  # install for testing
  if(Sys.info()["sysname"]!="Windows"){
    pkg_files = list.files("../",pattern="*.tar.gz",full.names=TRUE)
    pkg_files = grep("ePiE",pkg_files,value=TRUE)
    print(pkg_files)
    print(pkg_files[length(pkg_files)])
  }else{
    pkg_files = list.files("../",pattern="*.zip",full.names=TRUE)
    pkg_files = grep("ePiE",pkg_files,value=TRUE)
    print(pkg_files)
    print(pkg_files[length(pkg_files)])
  }
  install.packages(pkg_files[length(pkg_files)],repos = NULL)
}

# load function for testing
if(FALSE){
  devtools::load_all()
}




