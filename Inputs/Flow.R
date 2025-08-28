library(terra)
rav = rast("../FlowData/FLO1k.lt.2000.2015.qav.tif")
rma = rast("../FlowData/FLO1k.lt.2000.2015.qma.tif")
rmi = rast("../FlowData/FLO1k.lt.2000.2015.qmi.tif")

window = c(-11,45,35,75)
rav = crop(rav,window)
rma = crop(rma,window)
rmi = crop(rmi,window)

tiles = aggregate(rav, fact = dim(rav)[1]/4, fun=mean, na.rm=TRUE)
plot(tiles)


makeTiles(rav, tiles, filename="inst/FLO1k_2000_2015_qav.tif",overwrite=TRUE)
makeTiles(rma, tiles, filename="inst/FLO1k_2000_2015_qma.tif",overwrite=TRUE)
makeTiles(rmi, tiles, filename="inst/FLO1k_2000_2015_qmi.tif",overwrite=TRUE)
