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

##--------------------------------------##

# input dirs 
eu_dir = "basins_eu"
da_dir = "basins_da"
sc_dir = "basins_sc"
flow_dir = "flowdata"

# compress data
if(FALSE){
  eu_files = list.files(eu_dir)
  for(i in seq_along(eu_files)){
    file_path = file.path(eu_dir,eu_files[i])
    if(file.exists(file_path)){
      print(paste0("Compressing ",file_path))
      zip::zip(paste0(file_path,".zip"),file_path)
      file.remove(file_path)
    }
  }
  da_files = list.files(da_dir)
  for(i in seq_along(da_files)){
    file_path = file.path(da_dir,da_files[i])
    if(file.exists(file_path)){
      print(paste0("Compressing ",file_path))
      zip::zip(paste0(file_path,".zip"),file_path)
      file.remove(file_path)
    }
  }
  sc_files = list.files(sc_dir)
  for(i in seq_along(sc_files)){
    file_path = file.path(sc_dir,sc_files[i])
    if(file.exists(file_path)){
      print(paste0("Compressing ",file_path))
      zip::zip(paste0(file_path,".zip"),file_path)
      file.remove(file_path)
    }
  }
}

# decompress files
eu_files = list.files(eu_dir,full.names=TRUE,pattern="*.zip")
sc_files = list.files(sc_dir,full.names=TRUE,pattern="*.zip")
da_files = list.files(da_dir,full.names=TRUE,pattern="*.zip")
files = c(eu_files,sc_files,da_files)
i = 1
for(i in seq_along(files)){
  file_path = files[i]
  ofile = gsub("[.]zip","",file_path)
  ofile = file.path(dirname(ofile),ofile)
  if(file.exists(ofile)) next
  if(file.exists(file_path)){
    print(paste0("Decompressing ",file_path))
    zip::unzip(file_path,exdir=dirname(file_path))
  }
}

# load pts
eu_pts = read.csv(file.path(eu_dir,"basins_eu","pts.csv"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
da_pts = read.csv(file.path(da_dir,"basins_da","pts.csv"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
sc_pts = read.fst(file.path(sc_dir,"basins_sc","pts.fst"))
eu_pts$X = NULL
da_pts$DONW_TYPE_1 = NULL

# combine pts
head(eu_pts)
newcols = names(eu_pts)[!names(eu_pts)%in%names(da_pts)]
da_pts[,newcols] = NA
da_pts$pH = 7.4 # mean(eu_pts$pH,na.rm=TRUE)
eu_pts$pH[is.na(eu_pts$pH)] = 7.4 # mean(eu_pts$pH,na.rm=TRUE)
da_pts[["uwwOtherTr" ]][da_pts$Pt_type=="WWTP"] = 0
da_pts[["uwwNRemova" ]][da_pts$Pt_type=="WWTP"] = 0
da_pts[["uwwPRemova"]][da_pts$Pt_type=="WWTP"] = 0
da_pts[["uwwUV"]][da_pts$Pt_type=="WWTP"] = 0
da_pts[["uwwChlorin"]][da_pts$Pt_type=="WWTP"] = 0
da_pts[["uwwOzonati"]][da_pts$Pt_type=="WWTP"] = 0
da_pts[["uwwSandFil"]][da_pts$Pt_type=="WWTP"] = 0
da_pts[["uwwMicroFi"]][da_pts$Pt_type=="WWTP"] = 0
da_pts[["uwwOther"]][da_pts$Pt_type=="WWTP"] = 0
da_pts[["uwwSpecifi"]][da_pts$Pt_type=="WWTP"] = 0
cols = names(da_pts)
eu_pts = eu_pts[,cols]
da_pts = da_pts[,cols]
newcols_sc = names(eu_pts)[!names(eu_pts)%in%names(sc_pts)]
sc_pts[,newcols] = NA
sc_pts$pH = 6
sc_pts[["uwwOtherTr" ]][sc_pts$Pt_type=="WWTP"] = 0
sc_pts[["uwwNRemova" ]][sc_pts$Pt_type=="WWTP"] = 0
sc_pts[["uwwPRemova"]][sc_pts$Pt_type=="WWTP"] = 0
sc_pts[["uwwUV"]][sc_pts$Pt_type=="WWTP"] = 0
sc_pts[["uwwChlorin"]][sc_pts$Pt_type=="WWTP"] = 0
sc_pts[["uwwOzonati"]][sc_pts$Pt_type=="WWTP"] = 0
sc_pts[["uwwSandFil"]][sc_pts$Pt_type=="WWTP"] = 0
sc_pts[["uwwMicroFi"]][sc_pts$Pt_type=="WWTP"] = 0
sc_pts[["uwwOther"]][sc_pts$Pt_type=="WWTP"] = 0
sc_pts[["uwwSpecifi"]][sc_pts$Pt_type=="WWTP"] = 0
sc_pts = sc_pts[,cols]
pts = rbind(eu_pts, da_pts)
pts = rbind(pts, sc_pts)
pts$basin_id |> unique()
pts$basin_id[pts$basin_id=="danube"] = 85857

# load hl (lakes)
eu_hl = read.csv(file.path(eu_dir,"basins_eu","hl.csv"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
da_hl = read.csv(file.path(da_dir,"basins_da","hl.csv"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
sc_hl = read.fst(file.path(sc_dir,"basins_sc","hl.fst"))
eu_hl$X = NULL
da_hl$X = NULL
da_hl$basin_id[da_hl$basin_id=="danube"] = 85857

# combine lakes
cols = names(eu_hl)[names(eu_hl)%in%names(da_hl)]
eu_hl = eu_hl[,cols]
da_hl = da_hl[,cols]
sc_hl = sc_hl[,cols]
hl = rbind(eu_hl, da_hl)
hl = rbind(hl, sc_hl)
hl$basin_id |> unique()

# load flow
fav = rast(file.path(flow_dir,"FLO1K.qav.long.term.19602015.tif"))
fmi = rast(file.path(flow_dir,"FLO1K.qmi.long.term.19602015.tif"))
fma = rast(file.path(flow_dir,"FLO1K.qma.long.term.19602015.tif"))

# load basin borders
rename_geometry <- function(g, name){
  current = attr(g, "sf_column")
  names(g)[names(g)==current] = name
  st_geometry(g)=name
  g
}
eu_b = st_read("basins_eu/basins_eu/Basin.geojson")
da_b = st_read("basins_da/basins_da/basin.gpkg")
da_b = st_cast(da_b,"POLYGON")
sc_b = st_read("basins_sc/basins_sc/basinBorders.gpkg")
da_b$AREA_SQKM = NULL
names(sc_b)[names(sc_b)=="basin_id"]="BASIN_ID"
eu_b = rename_geometry(eu_b, "geom")
eu_b$region = "eu"
da_b$region = "da"
sc_b$region = "sc"
#which(pts$basin_id=="85857")

# combine basin borders
b = rbind(eu_b,da_b)
b = rbind(b,sc_b)
head(b)
#mapview(b)

# add names if possible
bnames = st_read("basin_names/ePiE_eu_basin_naming.gpkg")
b$BASIN_ID[match(bnames$BASIN_ID,b$BASIN_ID)]==bnames$BASIN_ID
b$name = NA
b$name[match(bnames$BASIN_ID,b$BASIN_ID)] = bnames$Name

# remove overlapping basins
rm_ID = c(
  "26602",
  "000342_1047701_sc",
  "000179_1047478_sc",
  "000124_1047004_sc",
  "000152_1047357_sc",
  "000085_1048208_sc",
  "000162_1049273_sc",
  "000119_1050223_sc",
  "000398_1051323_sc",
  "000428_1050466_sc",
  "000466_1050318_sc",
  "000249_1049527_sc",
  "000104_1047922_sc",
  "000446_1047268_sc",
  "000043_1044207_sc",
  "000001_1022330_sc",
  "000004_1015364_sc"
)
b = b[!b$BASIN_ID%in%rm_ID,]
b = b[b$BASIN_ID%in%unique(pts$basin_id),]
# mapview(b)

# remove overlapping polygons
sc_b$area_og = st_area(sc_b) |> as.vector()
overlap = st_intersection(sc_b, eu_b, sparse = FALSE)
overlap$area = st_area(overlap) |> as.vector()
overlap$area_rel = overlap$area / overlap$area_og
rm_ID = overlap$BASIN_ID[which(overlap$area_rel>0.65)]
b2 = b[!b$BASIN_ID%in%rm_ID,]
#mapview(b2)

# remove unneeded basins from pts and hl
nrow(pts)
pts = pts[pts$basin_id %in% b2$BASIN_ID,]
nrow(pts)
nrow(hl)
hl = hl[hl$basin_id %in% b2$BASIN_ID,]
nrow(hl)

# construct basins list
basins = list(pts=pts,hl=hl)
basins$pts = basins$pts[!basins$pts$basin_id%in%c(330319,330322),]
basins$hl = basins$hl[!basins$hl$basin_id%in%c(330319,330322),]
b2 = b2[!b2$BASIN_ID%in%c(330319,330322),]
b2$name[is.na(b2$name)] = b2$BASIN_ID[is.na(b2$name)]
mapview(b2)

# add flow
basins_avg = AddFlowToBasinData(basin_data = basins, flow_rast = fav)
basins_mi = AddFlowToBasinData(basin_data = basins, flow_rast = fmi)
basins_ma = AddFlowToBasinData(basin_data = basins, flow_rast = fma)

# save environment
if(FALSE) save.image(file.path(export_dir, "env_export.RData"))
##--------------------------------------##
if(FALSE) load(file.path(export_dir, "env_export.RData"))

# get Freq (n upstream pts)
p = basins$pts
table(is.na(p$Freq))
idx = which(is.na(p$Freq))
p$IDb = paste0(p$ID,"_",p$basin_id)
p$ID_nxtb = paste0(p$ID_nxt,"_",p$basin_id)
head(p)
for(i in seq_along(idx)){
  progress = round(i/length(idx)*100, 2)
  if(i %% 100 == 0 | i == 1 | i ==length(idx)) print(paste(i,"/",length(idx), " (",progress,"%)",sep=""))
  p[idx[i],]
  IDbi = p$IDb[idx[i]]
  p$Freq[idx[i]] = length(which(IDbi==p$ID_nxtb))
}
idx = which(is.na(p$Freq))
p$IDb = NULL
p$ID_nxtb = NULL

# subset flow
flow_cols = c("ID","x","y","basin_id","Q","V","H","V_NXT")
pts_avg2 = basins_avg$pts[,flow_cols]
pts_mi2 = basins_mi$pts[,flow_cols] 
pts_ma2 = basins_ma$pts[,flow_cols]
head(pts_avg2)
dim(pts_avg2)

# split basins
n = 30 # into 30 parts
unique_basins = unique(basins$pts$basin_id)
indexing = sort(rep(1:n,length.out=length(unique_basins)))
print(table(indexing))
indexing = data.frame(basin_id=unique_basins, index=indexing)
indexing$index_pad = stringr::str_pad(indexing$index, width = 6, side = "left", pad = "0")
indexing$pts_file = paste0("pts_", indexing$index_pad, ".feather")
indexing$hl_file = paste0("hl_", indexing$index_pad, ".feather")
indexing$avg_file = paste0("avg_", indexing$index_pad, ".feather")
indexing$mi_file = paste0("mi_", indexing$index_pad, ".feather")
indexing$ma_file = paste0("ma_", indexing$index_pad, ".feather")
indexing$index_pad = NULL
head(indexing)

# write pts split
pts = basins$pts
for(i in 1:n) {
  print(i)
  basin_ids = indexing$basin_id[indexing$index == i]
  pts_subset = pts[pts$basin_id %in% basin_ids,]
  indexing_sub = indexing[indexing$basin_id%in%pts_subset$basin_id,]
  out_file = unique(indexing_sub$pts_file)[1]
  out_file = file.path(export_dir, out_file)
  out_file = gsub(".feather", ".csv", out_file)
  print(out_file)
  pts_subset$basin_id = as.character(pts_subset$basin_id)
  write.csv(pts_subset, out_file, row.names = FALSE)
}
rm(pts_subset, pts, indexing_sub)

# write hl split
hl = basins$hl
for(i in 1:n) {
  print(i)
  basin_ids = indexing$basin_id[indexing$index == i]
  hl_subset = hl[hl$basin_id %in% basin_ids,]
  indexing_sub = indexing[indexing$basin_id%in%hl_subset$basin_id,]
  out_file = unique(indexing_sub$hl_file)[1]
  out_file = file.path(export_dir, out_file)
  out_file = gsub(".feather", ".csv", out_file)
  print(out_file)
  hl_subset$basin_id = as.character(hl_subset$basin_id)
  write.csv(hl_subset, out_file, row.names = FALSE)
}

# write pts_avg2 split
pts = pts_avg2
for(i in 1:n) {
  print(i)
  basin_ids = indexing$basin_id[indexing$index == i]
  pts_subset = pts[pts$basin_id %in% basin_ids,]
  indexing_sub = indexing[indexing$basin_id%in%pts_subset$basin_id,]
  out_file = unique(indexing_sub$avg_file)[1]
  out_file = file.path(export_dir, out_file)
  out_file = gsub(".feather", ".csv", out_file)
  print(out_file)
  pts_subset$basin_id = as.character(pts_subset$basin_id)
  write.csv(pts_subset, out_file, row.names = FALSE)
}
rm(pts_subset, pts, indexing_sub)

# write pts_ma2 split
pts = pts_ma2
for(i in 1:n) {
  print(i)
  basin_ids = indexing$basin_id[indexing$index == i]
  pts_subset = pts[pts$basin_id %in% basin_ids,]
  indexing_sub = indexing[indexing$basin_id%in%pts_subset$basin_id,]
  out_file = unique(indexing_sub$ma_file)[1]
  out_file = file.path(export_dir, out_file)
  out_file = gsub(".feather", ".csv", out_file)
  print(out_file)
  pts_subset$basin_id = as.character(pts_subset$basin_id)
  write.csv(pts_subset, out_file, row.names = FALSE)
}
rm(pts_subset, pts, indexing_sub)

# write pts_mi2 split
pts = pts_mi2
for(i in 1:n) {
  print(i)
  basin_ids = indexing$basin_id[indexing$index == i]
  pts_subset = pts[pts$basin_id %in% basin_ids,]
  indexing_sub = indexing[indexing$basin_id%in%pts_subset$basin_id,]
  out_file = unique(indexing_sub$mi_file)[1]
  out_file = file.path(export_dir, out_file)
  out_file = gsub(".feather", ".csv", out_file)
  print(out_file)
  pts_subset$basin_id = as.character(pts_subset$basin_id)
  write.csv(pts_subset, out_file, row.names = FALSE)
}
rm(pts_subset, pts, indexing_sub)

# move files to subdir
setwd(export_dir)
dir.create("csv", showWarnings = FALSE)
system("mv *.csv csv/")

# write geojson
head(b2)
st_write(b2, "Basin.geojson", delete_dsn = TRUE, driver = "GeoJSON")
