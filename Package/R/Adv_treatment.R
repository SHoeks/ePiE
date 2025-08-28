

Adv_treatment <- function(phi_photo_UV,E_lambda_UV,k_O2,T_O2,k_OH,T_OH,UV,Cl,O3) {
# Script for the calculation of removal fraction during advanced treatment, by Rik Oldenkamp, 07-10-2016
  #includes UV-treatment, ozonation, chlorination

# --- Required user input
  #phi_photo_UV -- quantum yield of chemical at wavelength relevant for UV-treatment (mol/mol)
  #E_lambda_UV  -- molar absorption coefficient of chemical at wavelength relevant for UV-treatment (L/mol/cm)
  #k_O2         -- 2nd order rate constant for reaction with singlet oxygen (L/mol/s)
  #T_O2         -- temperature during degradation test with singlet oxygen (K)
  #k_OH         -- 2nd order rate constant for reaction with OH-radicals (L/mol/s)
  #T_OH         -- temperature during degradation test with OH-radicals (K)
  #UV           -- presence (-1) or absence (0) of UV-treatment in WWTP of interest
  #Cl           -- presence (-1) or absence (0) of chlorination treatment in WWTP of interest
  #O3           -- presence (-1) or absence (0) of ozonation treatment in WWTP of interest

# --- System characteristics - UV-treatment
HRT_UV  <- 0.003          #Hydraulic retention time UV-treatment (h)
                            #as in full scale pilot plant in Netherlands (PILLS project report, section 3.2.2, table 3.8)
                            #calculated from volume (15L) and flow rate (5m3/h[max of range 3.5-5m3/h])
T_UV    <- 290            #Water temperature during UV-treatment (K)
                            #as in full scale pilot plant in Netherlands (PILLS project report, section 3.2.2, table 3.8)
lambda_UV <- 254          #Relevant wavelength of UV-treatment (nm)
                            #as in full scale pilot plant in Netherlands (PILLS project report, section 3.2.2, table 3.8)
                            #minimum from range reported (200nm - 300nm)
I_UV      <- 6*400*(1.196e5/lambda_UV)/5350 #intensity of radiation of wavelength lambda during UV-treatment (mmol/cm2/s)
                            #6 lamps of 400W each: 6*400 W = 2400 J/s
                            #1 J = 1.196e5/lambda_UV mmol(photons) (Schwarzenbach et al., 1993; page 438)
                            #internal surface area of tank = 5350 cm2 (length = 1.3 m; internal diameter = 125 mm)
C_O2_UV   <- 1E-13          #Concentration singlet oxygen during UV-treatment (M)
C_OH_UV   <- 1E-12          #Concentration hydroxyl radicals during UV-treatment (M)

# --- System characteristics - chlorination tank
HRT_Cl  <- 0.25              #Hydraulic retention time chlorination (h)
T_Cl    <- 288         #Temperature during chlorination treatment (K)
                            #same default value as SimpleTreat 4.0 was assumed
C_O2_Cl <- 1E-13             #Concentration singlet oxygen during chlorination treatment (M)
C_OH_Cl <- 1E-12             #Concentration hydroxyl radicatls during chlorination treatment (M)

# --- System characteristics - ozonation unit
HRT_O3  <- 0.25           #Hydraulic retention time ozonation (h)
                            #as in full scale pilot plant in Germany (PILLS project report, section 3.2.1)
T_O3    <- 300         #Water temperature during ozonation treatment (K) - pilot plant Germany
                            #as in full scale pilot plant in Germany (PILLS project report, section 3.2.1)
C_O2_O3 <- 1E-13             #Concentration singlet oxygen during ozonation treatment (M)
                            #Ozone dose in full scale pilot plant in Germany (Pills project report, section 3.2.1):
                            #5-10 mg/L ozone --> how many moles of singlet oxygen does this result in?
C_OH_O3 <- 1E-12             #Concentration hydroxyl radicals during ozonation treatment (M)
                            #Ozone dose in full scale pilot plant in Germany (Pills project report, section 3.2.1):
                            #5-10 mg/L ozone --> how many moles of hydroxyl radicals does this result in?

# --- Calculation of degradation rate constants
k_UV  <- phi_photo_UV*E_lambda_UV*I_UV+
            k_O2*C_O2_UV*2^((T_O2-T_UV)/10)+
            k_OH*C_OH_UV*2^((T_OH-T_UV)/10)
k_Cl <- k_O2*C_O2_Cl*2^((T_O2-T_Cl)/10)+
            k_OH*C_OH_Cl*2^((T_OH-T_Cl)/10)
k_O3 <- k_O2*C_O2_O3*2^((T_O2-T_O3)/10)+
            k_OH*C_OH_O3*2^((T_OH-T_O3)/10)

  #setting rate constants at 0 when NA
  k_UV <- ifelse(is.na(k_UV),0,k_UV)
  k_Cl <- ifelse(is.na(k_Cl),0,k_Cl)
  k_O3 <- ifelse(is.na(k_O3),0,k_O3)

# --- Calculation of removal fractions during advanced treatment
f_UV <- -UV*(1-exp(-k_UV*HRT_UV*3600))           #fraction of chemical removed during UV-treatment
f_Cl <- -Cl*(1-exp(-k_Cl*HRT_Cl*3600))           #fraction of chemical removed during chlorination
f_O3 <- -O3*(1-exp(-k_O3*HRT_O3*3600))           #fraction of chemical removed during ozonation

list(f_rem_adv=1-(1-f_UV)*(1-f_Cl)*(1-f_O3))     #output: fraction removed during advanced treatment

}
