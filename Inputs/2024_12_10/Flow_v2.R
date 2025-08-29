library(terra)
library(fst)

# open data
rav = rast("../FlowData/FLO1k.lt.2000.2015.qav.tif")
rma = rast("../FlowData/FLO1k.lt.2000.2015.qma.tif")
rmi = rast("../FlowData/FLO1k.lt.2000.2015.qmi.tif")
pts = read.fst("inst/basin_db/pts_c75.fst")
hl = read.fst("inst/basin_db/hl_c75.fst")

# crop to eu
window = c(-11,45,35,75)
rav = crop(rav,window)
rma = crop(rma,window)
rmi = crop(rmi,window)

# extract data at pts locations
xy = data.frame(x=pts$x, y=pts$y)
rav_xy = extract(rav,xy,cells = TRUE)
rma_xy = extract(rma,xy,cells = TRUE)
rmi_xy = extract(rmi,xy,cells = TRUE)
rav_xy = data.frame(ID=rav_xy[["ID"]],Qav=rav_xy[["mean"]],cell=rav_xy[["cell"]])
rma_xy = data.frame(ID=rma_xy[["ID"]],Qma=rma_xy[["mean"]],cell=rma_xy[["cell"]])
rmi_xy = data.frame(ID=rmi_xy[["ID"]],Qmi=rmi_xy[["mean"]],cell=rmi_xy[["cell"]])
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
rval_xy[["Qav"]] = as.float(rval_xy[["Qav"]])
saveRDS(rval_xy,"inst/flow_lt/FLO1k_2000_2015_rastValues.rds")

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

# code to reconstruct raster
rp = list(nrows=nrow(rav),ncols=ncol(rav),xmin=xmin(rav),xmax=xmax(rav),ymin=ymin(rav),ymax=ymax(rav))
saveRDS(rp,"inst/flow_lt/FLO1k_2000_2015_rastProperties.rds")
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



