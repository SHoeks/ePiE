# Load package
library(ePiE)
library(terra)
library(tictoc)
library(openxlsx)

# Open API-specific data
chem = LoadExampleChemProperties()

# Complete missing values in chem data
chem = CompleteChemProperties(chem = chem)

# Test SimpleTreat
SimpleTreat4_0(chem_class=chem$class[1],MW=chem$MW[1],Pv=chem$Pv[1],S=chem$S[1],pKa=chem$pKa[1],
               Kp_ps=chem$Kp_ps_n[1],Kp_as=chem$Kp_as_n[1],k_bio_WWTP=chem$k_bio_wwtp_n[1],
               T_air=285,Wind=4,Inh=1000,E_rate=1,PRIM=-1,SEC=-1)

# Load example consumption data
cons = LoadExampleConsumption()
#cons[cons$cnt  == "IT", "Ibuprofen"] = 12

# Load basin data, river nodes and lakes, all European basins are included
basins = LoadEuropeanBasins()
#pts_sf = sf::st_as_sf(basins$pts, coords = c("x", "y"), crs = 4326)
#mapview(pts_sf)

# Select example basins, find a better way to select basins
unique(basins$pts$basin_id)
# https://www.hydrosheds.org/products/hydrobasins
basin_ids = c(124863,107287) # Rhine, York Ouse
#basin_ids = c(280172,107287) # basin without lakes for testing
basins = SelectBasins(basins_data = basins, basin_ids = basin_ids)

# Check whether consumption data are available for the WWTPs in basins
cons = CheckConsumptionData(basins$pts,chem,cons)

# Load river flow
flow_avg = LoadLongTermFlow("average")

# Attach flow to basin data
basins_avg = AddFlowToBasinData(basin_data = basins, flow_rast = flow_avg)
#hist(log10(basins_avg$pts$Q), main = "log10 Flow distribution", xlab = "log10 Flow (m3/s)")

# Run ePiE for all chems, all basins, 1 flow condition
#tic()
results1 = ComputeEnvConcentrations(basin_data = basins_avg, chem = chem, cons = cons, print=TRUE, cpp=FALSE)
results2 = ComputeEnvConcentrations(basin_data = basins_avg, chem = chem, cons = cons, print=TRUE, cpp=TRUE)
mean(results1$pts$WWTPremoval,na.rm=TRUE)
mean(results2$pts$WWTPremoval,na.rm=TRUE)
#toc()
str(results1,1)

# Plot interactive results for a single basin
InteractiveResultMap(results2, basin_id = basin_ids[1]) # Rhine
InteractiveResultMap(results2, basin_id = basin_ids[2]) # Ouse


