library(openxlsx)
library(mapview)
library(terra)
library(sf)
sf_use_s2(FALSE)

# load data
ph = st_read("inst/pH/FOREGS_pH.xlsx")
ph$PH = as.numeric(ph$PH)
ph = ph[ph$PH>0,]
ph = ph[!is.na(ph$PH),]
head(ph)

# load basins
basins = st_read("../BasinData/hybas_eu_lev05_v1c.shp")

# load flow raster (as template)
rav = rast("../FlowData/FLO1k.lt.2000.2015.qav.tif")

# read ePiE points and lakes
d = "Z:/Repositories/ePiEPackage/ePiE/inst/basin_db/2024_07_24"
pts = fst::read.fst(glue::glue("{d}/pts_c75.fst"))
pts = st_as_sf(pts, coords = c("x", "y"), crs = crs(basins))
hl = fst::read.fst(glue::glue("{d}/hl_c75.fst"))
hl_sf = st_read("Z:/Repositories/ePiEPackage/ePiE/inst/basin_geo/Lakes.geojson")
hl_sf = hl_sf[hl_sf$Hylak_id %in% paste0(hl$Hylak_id,"_b",hl$basin_id),]
all(paste0(hl$Hylak_id,"_b",hl$basin_id)==hl_sf$Hylak_id)
hl = st_as_sf(hl, coords = c("Pour_long", "Pour_lat"), crs = crs(basins))

# plot
ph = st_as_sf(ph, coords = c("LONG", "LAT"), crs = crs(basins))
mapview(basins) + mapview(ph, zcol = "PH", col.regions = terrain.colors(10), alpha.regions = 0.5)

# get avg per basin
basins2 = st_join(basins, ph)

# remove cols not needed
basins2 = basins2[,c("HYBAS_ID", "PH")]

# plot ph per basin
mapview(basins2, zcol = "PH") + mapview(ph, zcol = "PH")

# fill NAs using value of closest basin
n = 1
n_prev = 100
while(n > 0){
  idxNA = basins2$PH |> is.na() |> which()
  n = length(idxNA)
  if(n_prev == n) break
  message("Number of NA: ", n)
  pb = txtProgressBar(min = 0, max = n, style = 3)
  for(i in 1:n){
    setTxtProgressBar(pb, i)
    basin_i = basins2[idxNA[i],]
    idxTouch = st_touches(basin_i, basins2)[[1]]
    if(all(is.na(basins2$PH[idxTouch]))) {
      next
    }else{
      basins2$PH[idxNA[i]] = mean(basins2$PH[idxTouch], na.rm = TRUE)
    }
  }
  close(pb)
  n_prev = n
}

# fill last basins manually
idxNA = basins2$PH |> is.na() |> which()
basins2$PH[basins2$HYBAS_ID==2050062920] = 6.18

basins2$PH[basins2$HYBAS_ID==2050057310] = 6.6
basins2$PH[basins2$HYBAS_ID==2050058330] = 6.6
basins2$PH[basins2$HYBAS_ID==2050059310] = 6.6
basins2$PH[basins2$HYBAS_ID==2050058320] = 6.6
basins2$PH[basins2$HYBAS_ID==2050057170] = 6.6
basins2$PH[basins2$HYBAS_ID==2050059250] = 6.6
basins2$PH[basins2$HYBAS_ID==2050057300] = 6.6
basins2$PH[basins2$HYBAS_ID==2050059260] = 6.6
basins2$PH[basins2$HYBAS_ID==2050059320] = 6.6

basins2$PH[basins2$HYBAS_ID==2050044800] = 7.754224609374999

basins2$PH[basins2$HYBAS_ID==2050048590] = 7.9
basins2$PH[basins2$HYBAS_ID==2050054000] = 7.1
basins2$PH[basins2$HYBAS_ID==2050054140] = 7.1
basins2$PH[basins2$HYBAS_ID==2050059370] = 7.1
basins2$PH[basins2$HYBAS_ID==2050059450] = 7.1

basins2$PH[basins2$HYBAS_ID==2050056710] = 7.3
basins2$PH[basins2$HYBAS_ID==2050056770] = 7.3

# try again, fill NAs using value of closest basin
n = 1
n_prev = 100
while(n > 0){
  idxNA = basins2$PH |> is.na() |> which()
  n = length(idxNA)
  if(n_prev == n) break
  message("Number of NA: ", n)
  pb = txtProgressBar(min = 0, max = n, style = 3)
  for(i in 1:n){
    setTxtProgressBar(pb, i)
    basin_i = basins2[idxNA[i],]
    idxTouch = st_touches(basin_i, basins2)[[1]]
    if(all(is.na(basins2$PH[idxTouch]))) {
      next
    }else{
      basins2$PH[idxNA[i]] = mean(basins2$PH[idxTouch], na.rm = TRUE)
    }
  }
  close(pb)
  n_prev = n
}

# plot ph per basin (after gap filling)
mapview(basins2, zcol = "PH")

# rasterize pH
phr = rasterize(basins2, rav, field = "PH")
plot(phr)

# extract ph for pts
pts$pH = extract(phr, pts, fun="mean")[,2]
plot(phr)
plot(pts[sample(1:nrow(pts),1e4),"pH"],add=TRUE)

# extract ph for lakes
hl$pH = extract(phr, hl, fun="mean")[,2]
plot(phr)
plot(hl[,"pH"],add=TRUE)

# write new pts inputs for package
dout = "Z:/Repositories/ePiEPackage/ePiE/inst/basin_db"
pts$x = st_coordinates(pts)[,1]
pts$y = st_coordinates(pts)[,2]
pts = st_drop_geometry(pts)
head(pts)
fst::write.fst(pts,glue::glue("{dout}/pts_c75.fst"))

# write new hl inputs for package
hl$Pour_long = st_coordinates(hl)[,1]
hl$Pour_lat = st_coordinates(hl)[,2]
hl = st_drop_geometry(hl)
hl$HRT = hl$Res_time * 24 * 60 * 60 # correct HRT
hl$HRT_sec = hl$HRT # correct HRT
hl$H_av = hl$Depth_avg # correct Depth_avg
head(hl)
fst::write.fst(hl,glue::glue("{dout}/hl_c75.fst"))

# fix (loss of coordinates)
if(FALSE){

  # pts
  d = "Z:/Repositories/ePiEPackage/ePiE/inst/basin_db/2024_07_24"
  pts_old = fst::read.fst(glue::glue("{d}/pts_c75.fst"))
  dout = "Z:/Repositories/ePiEPackage/ePiE/inst/basin_db"
  pts_new = fst::read.fst(glue::glue("{dout}/pts_c75.fst"))
  all(pts_old$ID==pts_new$ID)
  dim(pts_old)
  dim(pts_new)
  pts_new$x = pts_old$x
  pts_new$y = pts_old$y
  range(pts_new$x)
  range(pts_new$y)
  fst::write.fst(pts_new,glue::glue("{dout}/pts_c75.fst"))

  # hl
  d = "Z:/Repositories/ePiEPackage/ePiE/inst/basin_db/2024_07_24"
  hl_old = fst::read.fst(glue::glue("{d}/hl_c75.fst"))
  dout = "Z:/Repositories/ePiEPackage/ePiE/inst/basin_db"
  hl_new = fst::read.fst(glue::glue("{dout}/hl_c75.fst"))
  all(hl_old$Hylak_id ==hl_new$Hylak_id )
  dim(hl_old)
  dim(hl_new)
  hl_new$Pour_long = hl_old$Pour_long
  hl_new$Pour_lat = hl_old$Pour_lat
  range(hl_new$Pour_long)
  range(hl_new$Pour_lat)
  fst::write.fst(hl_new,glue::glue("{dout}/hl_c75.fst"))
}




