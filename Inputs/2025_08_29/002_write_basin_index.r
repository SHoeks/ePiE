library(ePiE)
library(zip)
library(mapview)
library(sf)
library(terra)
library(fst)
sf_use_s2(FALSE)

# wd
wd = ("C:/Users/Selwyn Hoeks/Documents/GitHub/ePiE_Rpackage/Inputs/2025_08_29")
setwd(wd)


# create export dir
# copy contents to resources of ePiE app
export_dir = "data_export_2025_08_15"
dir.create(export_dir,showWarnings = FALSE)

# basin index
csvfiles = list.files(file.path(export_dir,"csv"),pattern="*.csv",full.names=TRUE)
csvfiles = grep("pts_",csvfiles,value=TRUE)
x = csvfiles[1]
basin_index = lapply(csvfiles,function(x){
  df = read.csv(x,stringsAsFactors=FALSE)
  basin_id = unique(df$basin_id)
  df = data.frame(
    basin_id = basin_id,
    file = basename(x),
    stringsAsFactors = FALSE
  )
  return(df)
})
str(basin_index)
length(basin_index)
basin_index = do.call(rbind, basin_index)
basin_index$file_index = stringr::str_split(basin_index$file,pattern="_",simplify=TRUE)[,2]
basin_index$file_index = stringr::str_split(basin_index$file_index,pattern="[.]",simplify=TRUE)[,1]
basin_index$file = NULL
head(basin_index)
nrow(basin_index)

# write to disk
str(basin_index)
basin_index$basin_id = as.character(basin_index$basin_id)
basin_index$file_index = as.character(basin_index$file_index)
write.csv(basin_index,file=file.path(export_dir,"csv","BasinIndex.csv"),row.names=FALSE)
