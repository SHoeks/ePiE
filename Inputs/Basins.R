rm(list=ls())
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("/vol/milkunG/shoeks/Repositories/ePiEPackage/ePiE/")
library(sf)
sf_use_s2(FALSE)
library(terra)
library(stringr)
library(glue)
library(plyr)
library(fst)
source("R/Select_basin_pts3_v2.R")
source("R/Set_upstream_points_v2.R")

# read all basin border from shp file
list.files("/vol/milkunarc/roldenka/iPiE_archive/EEF/Build New Basins/Data/basin")
bb = st_read("/vol/milkunarc/roldenka/iPiE_archive/EEF/Build New Basins/Data/basin/eu_bas_15s_beta.shp")

# list files
basin_dir = "/vol/milkunarc/roldenka/iPiE_archive/EEF/Basin data/"
basin_dirs = grep("Basin_",list.dirs(basin_dir,recursive=FALSE),value=TRUE)
basin_dirs = basin_dirs[grep("124863 07.29.26|124863_doubt|notfinished",basin_dirs,invert=TRUE)]
basin_ids = str_split(basin_dirs,"Basin_",simplify=TRUE)[,2]
basin_ids = as.numeric(basin_ids)
Basins_selection = 124863 # Rhine

# read files pts & hl
pts_list = list()
HL_list = list()
pts_counter = 1
HL_counter = 1
for(Basins_selection in basin_ids) {
  # exclude these
  if(Basins_selection==100990) next
  if(Basins_selection==163878) next
  if(Basins_selection==276336) next
  if(Basins_selection==277423) next
  if(Basins_selection==320749) next
  if(Basins_selection==119737) next
  if(Basins_selection==120934) next
  if(Basins_selection==121333) next
  if(Basins_selection==330208) next
  if(Basins_selection==330493) next
  if(Basins_selection==112975) next
  print(Basins_selection)
  pts = Select_basin_pts3(Basins_selection,basin_dir,"3")
  HL = Select_basin_HL(Basins_selection,basin_dir,"")
  pts = Set_upstream_points_v2(pts)
  pts$basin_id = Basins_selection
  pts_list[[pts_counter]] = pts
  pts_counter = pts_counter + 1
  if(nrow(HL)>0) {
    HL$basin_id = Basins_selection
    HL_list[[HL_counter]] = HL
    HL_counter = HL_counter + 1
  }
}
# combine files
length(pts_list)
n_pts = sapply(pts_list,nrow)
basin_keep = c()
pts_all = data.frame()
for(x in 1:length(pts_list)){
  print(x)
  if(nrow(pts_list[[x]])>35) {
    basin_keep = c(basin_keep,unique(pts_list[[x]]$basin_id))
    if(nrow(pts_all)==0){
      pts_all = pts_list[[x]]
    }else{
      pts_all = rbind.fill(pts_all,pts_list[[x]])
    }
  }
}
length(HL_list)
HL_all = do.call(rbind.fill,HL_list)
nrow(HL_all)
HL_all = HL_all[HL_all$basin_id%in%basin_keep,]
nrow(HL_all)
HL_all$X = NULL
pts_all$X = NULL
pts_all$X.1 = NULL
head(pts_all)
dim(pts_all)
dim(HL_all)

# write files
if(FALSE){
  write.fst(pts_all,"pts_c75.fst",compress=75)
  write.fst(HL_all,"hl_c75.fst",compress=75)
}
#write.fst(pts_all,"pts_c50.fst",compress=50)
#write.fst(HL_all,"hl_c50.fst",compress=50)
#write.fst(pts_all,"pts_c100.fst",compress=100)
#write.fst(HL_all,"hl_c100.fst",compress=100)

# read files shapefiles
River_st_list = list()
Lake_st_list = list()
Basin_st_list = list()
Basins_selection = basin_ids[1]
for(Basins_selection in basin_ids) {

  # exclude: c(100990,163878,276336,277423,320749,119737,120934,121333,330208,330493,112975)
  if(Basins_selection==100990) next
  if(Basins_selection==163878) next
  if(Basins_selection==276336) next
  if(Basins_selection==277423) next
  if(Basins_selection==320749) next
  if(Basins_selection==119737) next
  if(Basins_selection==120934) next
  if(Basins_selection==121333) next
  if(Basins_selection==330208) next
  if(Basins_selection==330493) next
  if(Basins_selection==112975) next

  print(Basins_selection)
  basin_select_dir = paste0(basin_dir,"Basin_",Basins_selection,"/Shapefiles_forplot/")
  if(dir.exists(basin_select_dir)){
    shp_files = list.files(basin_select_dir)

    shp_file_river = shp_files[grep("^River.*\\.shp$",shp_files)]
    st_riv = st_read(file.path(basin_select_dir,shp_file_river))
    st_riv$linkId = paste0(st_riv$linkId,"_b",Basins_selection)
    River_st_list[[length(River_st_list)+1]] = st_riv

    shp_file_lake = shp_files[grep("^Lake.*\\.shp$",shp_files)]
    st_lak = st_read(file.path(basin_select_dir,shp_file_lake))
    st_lak = st_lak[,"Hylak_id"]
    if(nrow(st_lak)>0){
      st_lak$Hylak_id = paste0(st_lak$Hylak_id,"_b",Basins_selection)
      Lake_st_list[[length(Lake_st_list)+1]] = st_lak
    }

    bb_select = bb[which(Basins_selection==bb$BASIN_ID),]
    bb_select = bb_select[,"BASIN_ID"]
    Basin_st_list[[length(Basin_st_list)+1]] = bb_select

  }

}
Basin_all_st = do.call(rbind,Basin_st_list)
Lake_all_st = do.call(rbind,Lake_st_list)
River_all_st = do.call(rbind,River_st_list)
dim(Basin_all_st)
dim(Lake_all_st)
dim(River_all_st)

# write files
sf::st_write(Basin_all_st, dsn = "Basin.geojson", layer = "Basin.geojson")
sf::st_write(Lake_all_st, dsn = "Lakes.geojson", layer = "Lakes.geojson")
sf::st_write(River_all_st, dsn = "Rivers.geojson", layer = "Rivers.geojson")

# fix multipolygons and rewrite geojson file
bb_idx = which(bb$BASIN_ID%in%basin_ids)
Basin_all_st2 = bb[bb_idx,]
Basin_all_st2 = Basin_all_st2[,"BASIN_ID"]
which(Basin_all_st2$BASIN_ID==124863)

Basin_all_st2 = st_make_valid(Basin_all_st2)
Basin_all_st3 = list()
for(i in 1:nrow(Basin_all_st2)){
  multipoly = as.character(st_geometry_type(Basin_all_st2[i,])[1])=="MULTIPOLYGON"
  if(multipoly){
    Basin_all_st3[[i]] = st_cast(Basin_all_st2[i,],"POLYGON")
  }else{
    Basin_all_st3[[i]] = Basin_all_st2[i,]
  }
}
Basin_all_st3 = do.call(rbind,Basin_all_st3)
rownames(Basin_all_st3) = NULL
png();plot(Basin_all_st3);dev.off()
max(table(Basin_all_st3$BASIN_ID))
st_write(Basin_all_st3, dsn = "Basin.geojson", layer = "Basin.geojson")









