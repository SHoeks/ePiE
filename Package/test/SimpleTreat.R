# SimpleTreat 4.0 in R

# Load ePiE package (install if needed)
# install.packages("/path/to/ePiE_1.098.tar.gz",repos = NULL) # uncomment this line to install ePiE pkg from tar.gz file
library(ePiE)

# Load ePiE example properties (Ibuprofen)
chem = ePiE::LoadExampleChemProperties()

# Complete missing values in chem data
chem = ePiE::CompleteChemProperties(chem = chem)

# This function describes the 6box- and 9box-models of SimpleTreat 4.0 (version 21-07-14)
# --- Required user input - chemical characteristics
#chem_class -- chemical class ("neutral", "acid", or "base")
#MW         -- molecular weight (g/mol)
#Pv         -- vapour pressure (Pa)
#S          -- water solubility (mg/L)
#pKa        -- acid dissociation coefficient (-) - only relevant for acids and bases, otherwise NA
#Kp_ps      -- sorption coefficient to primary sludge (L/kg)
#Kp_as      -- sorption coefficient to activated sludge (L/kg)
#k_bio_WWTP -- biodegration rate constant during CAS (s-1) - assuming degradation only in the aqueous
#phase of activated sludge (chemical absorbed to solids is not available for biodegradation)
# --- Required user input - scenario parameters
#PRIM       -- presence/absence of primary treatment (-1:present; 0:absent)
#SEC        -- presence/absence of secondary treatment (-1:present; 0:absent)
#T_air      -- air temperature (Kelvin)
#Wind       -- wind speed (m/s)
#Inh        -- load entering the WWTP (PE; person equivalents)
#E_rate     -- emission rate of chemical (kg/d)
removal = ePiE::SimpleTreat4_0(chem_class=chem$class[1],
                               MW=chem$MW[1],
                               Pv=chem$Pv[1],
                               S=chem$S[1],
                               pKa=chem$pKa[1],
                               Kp_ps=chem$Kp_ps_n[1],
                               Kp_as=chem$Kp_as_n[1],
                               k_bio_WWTP=chem$k_bio_wwtp[1],
                               T_air=285,Wind=4,
                               Inh=1000,E_rate=1,
                               PRIM=-1,SEC=-1)




