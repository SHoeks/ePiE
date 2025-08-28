# Set wd and install ePiE package
setwd("Z:/Repositories/ePiEPackage/ePiE/")
if(FALSE){
  detach("package:ePiE", unload=TRUE)
  remove.packages("ePiE")
  devtools::build()
  pkg_files = list.files("../",pattern="*.tar.gz",full.names=TRUE)
  pkg_files = grep("ePiE",pkg_files,value=TRUE)
  install.packages(pkg_files[length(pkg_files)])
}

# Load package
library(ePiE)
version = ePiEVersion()
library(terra)
utils::browseURL(ePiEPath())

# This overwrites the functions of the package loaded
if(FALSE){
  rf = list.files("Z:/Repositories/ePiEPackage/ePiE/R",pattern="*.R",full.names=TRUE)
  lapply(rf,source)
}

# Open API-specific data
chem = LoadExampleChemProperties()
chem2 = openxlsx::read.xlsx("Z:/Repositories/ePiEPackage/ePiE/inst/chem_input/chem_Oldenkamp2018_SI.xlsx")

# Complete missing values in chem data
chem = CompleteChemProperties(chem = chem)
chem2 = CompleteChemProperties(chem = chem2)
chem2[which(chem2$API=="Ibuprofen"),grep("alt",colnames(chem2),invert=TRUE)]
#openxlsx::write.xlsx(chem,"chem_exampleIbuprofen.xlsx")

# Load example consumption data
cons = LoadExampleConsumption()

# Load basin data, river nodes and lakes, all European basins are included
basins = LoadEuropeanBasins()
str(basins,1)
unique(basins$pts$basin_id)

# Select example basins, find a better way to select basins
basin_ids = c(124863)
basins = SelectBasins(basins_data = basins, basin_ids = basin_ids)
table(basins$pts$basin_id)
table(basins$hl$basin_id)

# Check whether consumption data are available for the WWTPs in basins
cons = CheckConsumptionData(basins$pts,chem,cons)

# Load river flow
flow_avg = LoadLongTermFlow("average")
flow_min = LoadLongTermFlow("minimum")
flow_max = LoadLongTermFlow("maximum")

# Attach flow to basin data
basins_avg = AddFlowToBasinData(basin_data = basins, flow_rast = flow_avg)
basins_min = AddFlowToBasinData(basin_data = basins, flow_rast = flow_min)
basins_max = AddFlowToBasinData(basin_data = basins, flow_rast = flow_max)

# Run ePiE for all chems, all basins, 1 flow condition
results1 = ComputeEnvConcentrations(basin_data = basins_avg, chem = chem,
                                    cons = cons, useCPP=FALSE, print=FALSE)
str(results1,1)

# Run ePiE for all chems, all basins, all flow conditions
# Parallel execution should work on all operating systems
basins_allflows = list(avg=basins_avg,min=basins_min,max=basins_max)
results2 = ComputeEnvConcFlowParallel(basins_allflows, chem = chem, cons = cons)
str(results2,2)

# Run ePiE C++
# C++ code is a translation, needs further optimization
stressTestPerf = FALSE
if(stressTestPerf){
  chem3 = rbind(chem,chem,chem)
  chem3$API = c("Ibuprofen1","Ibuprofen2","Ibuprofen3")
  cons3 = cons
  cons3[,c("Ibuprofen1","Ibuprofen2","Ibuprofen3")] = cons3$Ibuprofen
}else{
  chem3 = chem
  cons3 = cons
}
results3 = ComputeEnvConcentrationsCPP(basin_data = basins_avg, chem3, cons3)


dir = "C:/Users/SELWYN~1/AppData/Local/Temp/RtmpA3wrYV/epie_engine_tmp_13_02_24_13_07_26"
utils::browseURL(dir)

p1= "C:/Temp/pts.csv"
p2 = "C:/Temp/hl.csv"
p3 = "C:/Temp/pts_out.csv"
p4 = "C:/Temp/hl_out.csv"
cmd = glue::glue("\"C:/Users/Selwyn Hoeks/AppData/Local/R/win-library/4.3/ePiE/win_exec/epie_engine.exe\"  \"{p1}\" \"{p2}\" \"{p3}\" \"{p4}\" 12")
print(cmd)
system(cmd)

utils::browseURL("C:/Users/Selwyn Hoeks/AppData/Local/Temp/RtmpuyK7P9/epie_engine_tmp_08_02_24_16_50_24/")


# Check results, serial vs parallel
id1 = paste0(results1$pts$ID,"__",results1$pts$basin_ID)
id2 = paste0(results3$pts$ID,"__",results3$pts$basin_id)
diff=data.frame(id_r=id1[order(id1)],r=results1$pts$C_w[order(id1)],
                id_cpp=id2[order(id2)],cpp=results3$pts$C_w[order(id2)])
diff$diff = abs(diff$r-diff$cpp)
diff[which(diff$diff>1e-10),]
all.equal(diff$r,diff$cpp)
all.equal(results1,results2[[1]])

# Create output dir
time = format(Sys.time(),'%Y_%m_%d_%H%M')
OutputPath = paste0("~/Desktop/ePiE_Results/Results_",time)
dir.create(OutputPath,showWarnings = FALSE, recursive = TRUE)

# Write output as csv for all river points
write.csv(results1$pts,paste0(OutputPath,"/","Results_","flow_","avg",".csv"))

# Write output as csv for all river points and all flows
for(i in names(results2)) write.csv(results2[[i]]$pts,paste0(OutputPath,"/","Results_","flow_",i,"p.csv"))

# Create statistics overview
for(i in names(results2)) CalculateWriteEnvStats(results2[[i]]$pts,results2[[i]]$hl,i,basins_allflows[[i]]$pts,OutputPath)

# Create and save multiple HTML maps
CreateHTMLMaps(results1$pts,1,OutputPath)
for(i in names(results2)) CreateHTMLMaps(results2[[i]]$pts,i,OutputPath)
