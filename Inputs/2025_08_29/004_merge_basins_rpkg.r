library(ePiE)
library(zip)
library(mapview)
library(sf)
library(terra)
library(fst)
library(arrow)
sf_use_s2(FALSE)

# wd
wd = ("C:/Users/Selwyn Hoeks/Documents/GitHub/ePiE_Rpackage/Inputs/2025_08_29")
outdir = "data_export_2025_08_15"
setwd(wd)

# read pts and hl
csvf = list.files(file.path(outdir,"csv"), pattern = ".csv", full.names = TRUE)
ptsf = grep("pts_", csvf, value = TRUE)
hlf = grep("hl_", csvf, value = TRUE)
p = lapply(ptsf, read.csv)
h = lapply(hlf, read.csv)
p = do.call(rbind, p)
h = do.call(rbind, h)

# check
p$aggLatit_1 = NULL
p$aggLongi_1 = NULL
p$snap_dist = NULL
head(p)
unique(p$pH)

# write
rdsdir = file.path(outdir,"rds")
dir.create(rdsdir, showWarnings = FALSE)
saveRDS(p, file = file.path(rdsdir,"pts_all_basins.rds"))
saveRDS(h, file = file.path(rdsdir,"hl_all_basins.rds"))

# read+write basin shapefile
basin_shp = st_read(file.path(outdir,"Basin.geojson"))
saveRDS(basin_shp, file = file.path(rdsdir,"basin_shp_sf.rds"))
