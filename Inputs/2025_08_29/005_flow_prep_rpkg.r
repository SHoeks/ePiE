library(terra)
library(fst)
library(sf)

# wd
wd = ("C:/Users/Selwyn Hoeks/Documents/GitHub/ePiE_Rpackage/Inputs/2025_08_29")
outdir = "data_export_2025_08_15"
setwd(wd)

# open data
rav = rast("flowdata/FLO1K.qav.long.term.19602015.tif")
rma = rast("flowdata/FLO1K.qma.long.term.19602015.tif")
rmi = rast("flowdata/FLO1K.qmi.long.term.19602015.tif")
pts = readRDS(file.path(outdir,"rds","pts_all_basins.rds"))
hl = readRDS(file.path(outdir,"rds","hl_all_basins.rds"))
bs = readRDS(file.path(outdir,"rds","basin_shp_sf.rds"))
ext = ext(bs)
ext = ext + 1

# crop to eu
window = ext
rav = crop(rav,window)
rma = crop(rma,window)
rmi = crop(rmi,window)

# extract data at pts locations
xy = data.frame(x=pts$x, y=pts$y)
rav_xy = extract(rav,xy,cells = TRUE)
rma_xy = extract(rma,xy,cells = TRUE)
rmi_xy = extract(rmi,xy,cells = TRUE)
names(rav_xy)[2] = "value"
names(rma_xy)[2] = "value"
names(rmi_xy)[2] = "value"
head(rav_xy)
rav_xy = data.frame(ID=rav_xy[["ID"]],Qav=rav_xy[["value"]],cell=rav_xy[["cell"]])
rma_xy = data.frame(ID=rma_xy[["ID"]],Qma=rma_xy[["value"]],cell=rma_xy[["cell"]])
rmi_xy = data.frame(ID=rmi_xy[["ID"]],Qmi=rmi_xy[["value"]],cell=rmi_xy[["cell"]])
rval_xy = rav_xy
all(rval_xy$ID==rma_xy$ID)
all(rval_xy$ID==rmi_xy$ID)
rval_xy$Qma = rma_xy$Qma
rval_xy$Qmi = rmi_xy$Qmi
rval_xy = rval_xy[,c("ID", "cell", "Qav", "Qma", "Qmi")]
rval_xy = rval_xy[complete.cases(rval_xy),]
head(rval_xy)
rval_xy$ID = NULL
rval_xy[["cell"]] = as.integer(rval_xy[["cell"]])
typeof(rval_xy[["cell"]])
typeof(rval_xy[["Qav"]])
head(rval_xy)
rval_xy[["Qav"]] = as.float(rval_xy[["Qav"]])
str(rval_xy)
saveRDS(rval_xy,file.path(outdir,"rds","flo1k_6015_rastValues.rds"))

# save raster properties
rp = list(nrows=nrow(rav),ncols=ncol(rav),xmin=xmin(rav),xmax=xmax(rav),ymin=ymin(rav),ymax=ymax(rav))
saveRDS(rp,file.path(outdir,"rds","flo1k_6015_rastPropertie.rds"))

# all values
# rav_vals = as.data.frame(rav)
# rma_vals = as.data.frame(rma)
# rmi_vals = as.data.frame(rmi)
# rav_vals[["CellIndex"]] = as.integer(rownames(rav_vals))
# rma_vals[["CellIndex"]] = as.integer(rownames(rma_vals))
# rmi_vals[["CellIndex"]] = as.integer(rownames(rmi_vals))
# rownames(rav_vals) = NULL
# rownames(rma_vals) = NULL
# rownames(rmi_vals) = NULL
# head(rav_vals)
# head(rma_vals)
# head(rmi_vals)

# recreate raster from values
r = rast(nrows=rp$nrows,ncols=rp$ncols,xmin=rp$xmin,xmax=rp$xmax,ymin=rp$ymin,ymax=rp$ymax)
names(r) = "mean"
r[] = NA
r[rval_xy[["cell"]]] = rval_xy[["Qav"]]
plot(log10(r))
r2 = extract(r,data.frame(x=pts$x,y=pts$y))
head(r2)
r1 = extract(rav,data.frame(x=pts$x,y=pts$y))
head(r1)
all(r1$mean==r2$mean,na.rm=TRUE)



