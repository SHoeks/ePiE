

Download_basins <- function(){
  # Download URL basins Europe
  URL_basins <- "http://earlywarning.usgs.gov/hydrodata/sa_shapefiles_zip/eu_bas_15s_beta.zip"

  #check if unzipped file is already present
  if(!("eu_bas_15s_beta"%in%list.files("../Geographical data"))) {
    #check if zip file already downloaded
    if(!("eu_bas_15s_beta.zip"%in%list.files("../Geographical data"))) {
      download.file(URL_basins, destfile="../Geographical data/eu_bas_15s_beta.zip",method="libcurl")
    }
    #unzip
    unzip(zipfile = "../Geographical data/eu_bas_15s_beta.zip", exdir = "../Geographical data/eu_bas_15s_beta")
  }

  Basin_all <- readOGR(dsn="../Geographical data/eu_bas_15s_beta",layer="eu_bas_15s_beta",stringsAsFactors=FALSE)
  Basin_available <<- Basin_all[Basin_all@data$BASIN_ID%in%List_basins()$Basin_IDs,]


}

