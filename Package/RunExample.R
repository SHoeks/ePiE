# Load package
library(ePiE)

# Open API-specific data
chem = LoadExampleChemProperties()

# Complete missing values in chem data
chem = CompleteChemProperties(chem = chem)

# Test SimpleTreat
removal = SimpleTreat4_0(chem_class=chem$class[1],MW=chem$MW[1],Pv=chem$Pv[1],S=chem$S[1],pKa=chem$pKa[1],
               Kp_ps=chem$Kp_ps[1],Kp_as=chem$Kp_as[1],k_bio_WWTP=chem$k_bio_wwtp[1],
               T_air=285,Wind=4,Inh=1000,E_rate=1,PRIM=-1,SEC=-1)
print(removal)

# Load example consumption data
cons = LoadExampleConsumption()
str(cons)

# Load basin data, river nodes and lakes, all European basins are included
basins = LoadEuropeanBasins()

# Use ViewBasinMap to generate an interactive map of the river basins and retrieve the basin ids (BASIN_ID)
#ViewBasinMap()

# Create a vector with two river basin IDs
basin_ids = c(124863,107287) # Rhine 1, Ouse (Yorkshire)

# Run the SelectBasins function to subset the river nodes and lakes
basins = SelectBasins(basins_data = basins, basin_ids = basin_ids)

# Check whether consumption data are available for the WWTPs in the selected basins
cons = CheckConsumptionData(basins$pts,chem,cons)
print(cons)

# Load river flow
flow_avg = LoadLongTermFlow("average")

# Attach discharge to basin data
basins_avg = AddFlowToBasinData(basin_data = basins, flow_rast = flow_avg)

# Visualize discharge conditions in river nodes
hist(log10(basins_avg$pts$Q),
     main = "log10 river discharge distribution",
     xlab = "log10 discharge (m3/s)")

# Run ePiE for all chems, all basins, 1 flow condition
results = ComputeEnvConcentrations(basin_data = basins_avg, chem = chem, cons = cons, verbose=TRUE, cpp=TRUE)
str(results,2)

# show the river concentrations
hist(log10(results$pts$C_w),
     main = "log10 river concentrations (ug/L)",
     xlab = "log10 conc (ug/L)")

# Plot interactive results for a single basin
InteractiveResultMap(results, basin_id = basin_ids[1]) # Rhine
InteractiveResultMap(results, basin_id = basin_ids[2], cex = 4) # Ouse


