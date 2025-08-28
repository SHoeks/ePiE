SimpleTreat4_0 <- function(chem_class,MW,Pv,S,pKa,Kp_ps,Kp_as,k_bio_WWTP,T_air,Wind,Inh,E_rate,PRIM,SEC){

  # SimpleTreat 4.0 in R
  # This R-script describes the 6box- and 9box-models of SimpleTreat 4.0 (version 21-07-14)
  # Adapted from spreadsheet model by Rik Oldenkamp, 13-09-2016

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

  # --- Other scenario parameters that are set at default
  T_water     <- 288              #water temperature (Kelvin)

  # --- Main operational parameters (current default values, but can be user input)
  S_flow      <- 0.2              #sewage flow (m3/person/d)
  M_flow_rs   <- 0.09             #mass flow raw sewage (kg/person/d)
  BOD_rs      <- 60               #BOD in raw sewage (g/person/d)
  f_BOD_ss    <- 0.5417           #fraction BOD in sewage solids (-)
  f_ss_rem    <- 2/3              #fraction of sewage solids removed (-)
  SLR         <- 0.1              #sludge loading rate (kgBOD/kgdwt/d)
  aeration    <- "s"              #type of aeration applied (bubble: "b", or surface: "s")
  pH_WWTP     <- 7                #pH of WWTP water (-)

  # --- Additional chemical parameter calculations
  MW          <- MW/1000                                              #molecular weight (kg/mol)
  S           <- S/MW/1000                                            #solubility (mol/m3)
  fn          <- ifelse(chem_class=="acid",1/(1+10^(pH_WWTP-pKa)),    #fraction present in neutral form (-)
                      ifelse(chem_class=="base",1/(1+10^(pKa-pH_WWTP)),1))
  H           <- Pv/S*fn                                              #Henry constant (Pa*m3/mol)

  # --- Additional system characteristics - raw sewage
  dens_rs     <- 1.5                              #density solids raw sewage (kgdwt/L)
  C_ss_rs     <- M_flow_rs/S_flow                 #concentration susp solids raw sewage (kgdwt/m3)
  f_OC_rs     <- 0.3                              #mass fraction OC in raw sewage (-)
  C_tot_rs    <- 1000*E_rate/(S_flow*Inh)         #total chemical concentration in raw sewage (mg/L)
  C_diss_rs   <- C_tot_rs/(1+Kp_ps*C_ss_rs/1000)  #chemical concentration in raw sewage - dissolved (mg/L)
  C_solids_rs <- Kp_ps*C_diss_rs                  #chemical sorbed to solids in raw sewage (mg/kg)

  # --- Additional system characteristics - primary sedimentation
  depth_PS    <- 4                    #depth primary sedimentation tank (m)
  HRT_PS      <- 2                    #hydraulic retention time primary sedimentation tank (h)
  V_PS        <- S_flow*HRT_PS/24     #volume primary sedimentation tank (m3/person)
  A_PS        <- V_PS/depth_PS        #area primary sedimentation tank (m2/person)
  C_ss_PS     <- C_ss_rs*(1-f_ss_rem) #concentration susp solids in primary sedimentation tank (kgdwt/m3)
  f_OC_PS     <- 0.3                  #fraction OC in primary sedimentation tank (-)
  dens_PS     <- 1.5                  #density solids primary sedimentation tank (kgdwt/L)

  # --- Additional system characteristics - aerator
  depth_ae      <- 3                                              #depth aerator (m)
  oxygen_C      <- 0.002                                          #oxygen concentration (kgO2/m3)
  C_as          <- 4                                              #concentration activated sludge in aerator (kgdwt/m3)
  rate_ae       <- 0.0000131                                      #aeration rate (m3/s/person)
  f_OC_as       <- 0.37                                           #fraction OC in activated sludge (-)
  dens_as       <- 1.3                                            #density solids activated sludge (kgdwt/L)
  oxygen_req_9b <- (BOD_rs/1000)*(1-(f_BOD_ss*f_ss_rem))/S_flow   #oxygen requirement aerator - 9box (kgBOD/m3)
  V_ae_9b       <- S_flow*oxygen_req_9b/(SLR*C_as)                #volume aerator - 9box (m3/person)
  HRT_ae_9b     <- V_ae_9b/S_flow*24                              #hydraulic retention time aerator - 9box (h)
  A_ae_9b       <- V_ae_9b/depth_ae                               #area aerator - 9box (m2/person)
  oxygen_req_6b <- (BOD_rs/1000)/S_flow                           #oxygen requirement aerator - 6box (kgBOD/m3)
  V_ae_6b       <- S_flow*oxygen_req_6b/(SLR*C_as)                #volume aerator - 6box (m3/person)
  HRT_ae_6b     <- V_ae_6b/S_flow*24                              #hydraulic retention time aerator - 6box (h)
  A_ae_6b       <- V_ae_6b/depth_ae                               #area aerator - 6box (m2/person)

  # --- Additional system characteristics - solids liquid separation
  depth_SLS   <- 3                  #depth SLS (m)
  HRT_SLS     <- 6                  #HRT SLS (h)
  V_SLS       <- S_flow*HRT_SLS/24  #volume SLS (m3/person)
  A_SLS       <- V_SLS/depth_SLS    #area SLS (m2/person)
  C_ss_SLS    <- 0.0075             #concentration susp solids SLS (kgdwt/m3)
  f_OC_SLS    <- 0.37               #fraction OC in SLS (-)
  dens_SLS    <- 1.3                #density solids SLS (kgdwt/L)

  # --- Additional system characteristics - sludge loading characteristics
  f_BOD_rem           <- 0.818-0.0422*log(SLR)                              #fraction of BOD removed by activated sludge process (-)
  Y_BOD               <- 0.947+0.0739*log(SLR)                              #yield of biomass (sludge growth) per BOD (kgdwt/kgBOD)
  Surplus_sludge_9b   <- S_flow*(oxygen_req_9b*f_BOD_rem*Y_BOD-C_ss_SLS)    #wasted (surplus) sludge - 9box (kgdwt/person/d)
  Surplus_sludge_6b   <- S_flow*(oxygen_req_6b*f_BOD_rem*Y_BOD-C_ss_SLS)    #wasted (surplus) sludge - 6box (kgdwt/person/d)
  E_solids_eff        <- C_ss_SLS*S_flow                                    #emitted solids in effluent (kgdwt/person/d)
  SRT_9b              <- V_ae_9b*C_as/(Surplus_sludge_9b+C_ss_SLS*S_flow)   #sludge retention time - 9box(d)
  SRT_6b              <- V_ae_6b*C_as/(Surplus_sludge_6b+C_ss_SLS*S_flow)   #sludge retention time - 6box(d)

  # --- Additional system characteristics - general
  H_air       <- 10                   #height air column over WWTP (m)
  A_WWTP_9b   <- A_PS+A_ae_9b+A_SLS   #total area WWTP - 9box (m2/person)
  A_WWTP_6b   <- A_ae_6b+A_SLS        #total area WWTP - 6box (m2/person)

    # --- Advective transport calculations (m3/s)
  adv_9b       <- matrix(0,nrow=10,ncol=10)
  adv_9b[1,2]  <- sqrt(A_WWTP_9b*Inh)*H_air*Wind
  adv_9b[2,1]  <- sqrt(A_WWTP_9b*Inh)*H_air*Wind
  adv_9b[1,3]  <- S_flow*Inh/24/3600
  adv_9b[3,6]  <- S_flow*Inh/24/3600
  adv_9b[1,4]  <- M_flow_rs*Inh/(dens_rs*1000)/24/3600
  adv_9b[4,5]  <- f_ss_rem*adv_9b[1,4]
  adv_9b[4,7]  <- adv_9b[1,4]*(1-f_ss_rem)
  adv_9b[5,1]  <- adv_9b[4,5]
  adv_9b[6,8]  <- adv_9b[1,3]
  adv_9b[7,9]  <- (S_flow/24/3600)*Inh*C_as/(dens_as*1000)
  adv_9b[8,1]  <- adv_9b[6,8]
  adv_9b[9,1]  <- (S_flow/24/3600)*Inh*C_ss_SLS/(dens_SLS*1000)
  adv_9b[9,10] <- adv_9b[7,9]-adv_9b[9,1]
  adv_9b[10,1] <- Surplus_sludge_9b*Inh/24/3600/(dens_SLS*1000)
  adv_9b[10,7] <- adv_9b[9,10]-adv_9b[10,1]
  sludge_decay_9b <- (((adv_9b[4,7]*dens_PS+adv_9b[10,7]*dens_as)*1000-adv_9b[7,9]*dens_as*1000)/(dens_as*1000))*dens_as*1000*3600*24 #net sludge decay - 9box (kgdwt/person/d)


  adv_6b       <- matrix(0,nrow=10,ncol=10)
  adv_6b[1,2]  <- sqrt(A_WWTP_6b*Inh)*H_air*Wind
  adv_6b[2,1]  <- sqrt(A_WWTP_6b*Inh)*H_air*Wind
  adv_6b[1,6]  <- S_flow*Inh/24/3600
  adv_6b[1,7]  <- M_flow_rs*Inh/(dens_rs*1000)/24/3600
  adv_6b[6,8]  <- adv_6b[1,6]
  adv_6b[7,9]  <- (S_flow/24/3600)*Inh*C_as/(dens_as*1000)
  adv_6b[8,1]  <- adv_6b[6,8]
  adv_6b[9,1]  <- (S_flow/24/3600)*Inh*C_ss_SLS/(dens_SLS*1000)
  adv_6b[9,10] <- adv_6b[7,9]-adv_6b[9,1]
  adv_6b[10,1] <- Surplus_sludge_6b*Inh/24/3600/(dens_SLS*1000)
  adv_6b[10,7] <- adv_6b[9,10]-adv_6b[10,1]
  sludge_decay_6b <- (((adv_6b[1,7]*dens_rs+adv_6b[10,7]*dens_as)*1000-adv_6b[7,9]*dens_as*1000)/(dens_as*1000))*dens_as*1000*3600*24 #net sludge decay - 6box (kgdwt/person/d)


  # --- Box volume calculations (m3)
  vol_9b      <- rep(0,9)
  vol_9b[1]   <- A_WWTP_9b*Inh*H_air
  vol_9b[2]   <- V_PS*Inh
  vol_9b[3]   <- (vol_9b[2]*C_ss_PS)/(dens_PS*1000)
  vol_9b[4]   <- M_flow_rs*f_ss_rem/1000/dens_rs
  vol_9b[5]   <- V_ae_9b*Inh
  vol_9b[6]   <- vol_9b[5]*C_as/(dens_as*1000)
  vol_9b[7]   <- V_SLS*Inh
  vol_9b[8]   <- (vol_9b[7]*C_ss_SLS)/(dens_SLS*1000)
  vol_9b[9]   <- Surplus_sludge_9b/1000/dens_as


  vol_6b      <- rep(0,9)
  vol_6b[1]   <- A_WWTP_6b*Inh*H_air
  vol_6b[5]   <- V_ae_6b*Inh
  vol_6b[6]   <- vol_6b[5]*C_as/(dens_as*1000)
  vol_6b[7]   <- V_SLS*Inh
  vol_6b[8]   <- (vol_6b[7]*C_ss_SLS)/(dens_SLS*1000)
  vol_6b[9]   <- Surplus_sludge_6b/1000/dens_as

  # --- Exchange rate parameters
  K_air       <- 0.00278                          #velocity of exchange towards air at air-water surface, non-aerated basins (m/s)
  K_water     <- 0.0000278                        #velocity of exchange towards water at air-water surface, non-aerated basins (m/s)
  R           <- 8.314                            #gas constant (J/K/mol)
  Kaw         <- H/(R*T_air)                      #partition coefficient air-water (-)
  k_strip_9b  <- ifelse(aeration=="s",            #rate constant stripping in aerator - 9box (s-1)
                        0.6*oxygen_req_9b/(HRT_ae_9b*3600*(0.009-oxygen_C))*(40*Kaw/(40*Kaw+1)),
                        ifelse(aeration=="b",k_strip_9b <- 0.00089*rate_ae/V_ae_9b*H^1.04,NA))
  k_strip_6b  <- ifelse(aeration=="s",            #rate constant stripping in aerator - 6box (s-1)
                        0.6*oxygen_req_6b/(HRT_ae_6b*3600*(0.009-oxygen_C))*(40*Kaw/(40*Kaw+1)),
                        ifelse(aeration=="b",k_strip_6b <- 0.00089*rate_ae/V_ae_6b*H^1.04,NA))
  k_vol_9b    <- A_ae_9b*Inh*((1/(vol_9b[1]*A_ae_9b/A_WWTP_9b)+Kaw/vol_9b[5])/(1/K_air+Kaw/K_water))
                                                  #rate constant volatilisation aerator - 9box (s-1)
  k_vol_6b    <- A_ae_6b*Inh*((1/(vol_6b[1]*A_ae_6b/A_WWTP_6b)+Kaw/vol_6b[5])/(1/K_air+Kaw/K_water))
                                                  #rate constant volatilisation aerator - 6box (s-1)
  k_ae_9b     <- k_strip_9b+k_vol_9b              #total rate constant aerator - 9box (s-1)
  k_ae_6b     <- k_strip_6b+k_vol_6b              #total rate constant aerator - 6box (s-1)
  t50_PS      <- 3600                             #solids-water half-life for uptake&clearance - primary sedimentation (s)
  t50_ae      <- 360                              #solids-water half-life for uptake&clearance - aeration (s)
  t50_SLS     <- 3600                             #solids-water half-life for uptake&clearance - solids liquid separator (s)

  # --- Diffusive transport calculations (m3/s)
  diff_9b       <- matrix(0,nrow=10,ncol=10)
  diff_9b[2,3]  <- A_PS*Inh/(1/K_air+Kaw/K_water)
  diff_9b[3,2]  <- A_PS*Inh/(1/(K_air*Kaw)+1/K_water)
  diff_9b[2,6]  <- k_ae_9b/(1/(vol_9b[1]*A_ae_9b/A_WWTP_9b)+Kaw/vol_9b[5])
  diff_9b[6,2]  <- k_ae_9b/(1/(vol_9b[1]*A_ae_9b/A_WWTP_9b*Kaw)+1/vol_9b[5])
  diff_9b[2,8]  <- A_SLS*Inh/(1/K_air+Kaw/K_water)
  diff_9b[8,2]  <- A_SLS*Inh/(1/(K_air*Kaw)+1/K_water)
  diff_9b[3,4]  <- (log(2)/t50_PS)/(1/vol_9b[2]+1/(vol_9b[3]*dens_PS*Kp_ps))
  diff_9b[4,3]  <- (log(2)/t50_PS)/(dens_PS*Kp_ps/vol_9b[2]+1/vol_9b[3])
  diff_9b[6,7]  <- (log(2)/t50_ae)/(1/vol_9b[5]+1/(vol_9b[6]*dens_as*Kp_as))
  diff_9b[7,6]  <- (log(2)/t50_ae)/(dens_as*Kp_as/vol_9b[5]+1/vol_9b[6])
  diff_9b[8,9]  <- (log(2)/t50_SLS)/(1/vol_9b[7]+1/(vol_9b[8]*dens_SLS*Kp_as))
  diff_9b[9,8]  <- (log(2)/t50_SLS)/(dens_SLS*Kp_as/vol_9b[7]+1/vol_9b[8])


  diff_6b       <- matrix(0,nrow=10,ncol=10)
  diff_6b[2,6]  <- k_ae_6b/(1/(vol_6b[1]*A_ae_6b/A_WWTP_6b)+Kaw/vol_6b[5])
  diff_6b[6,2]  <- k_ae_6b/(1/(vol_6b[1]*A_ae_6b/A_WWTP_6b*Kaw)+1/vol_6b[5])
  diff_6b[2,8]  <- A_SLS*Inh/(1/K_air+Kaw/K_water)
  diff_6b[8,2]  <- A_SLS*Inh/(1/(K_air*Kaw)+1/K_water)
  diff_6b[6,7]  <- (log(2)/t50_ae)/(1/vol_6b[5]+1/(vol_6b[6]*dens_as*Kp_as))
  diff_6b[7,6]  <- (log(2)/t50_ae)/(dens_as*Kp_as/vol_6b[5]+1/vol_6b[6])
  diff_6b[8,9]  <- (log(2)/t50_SLS)/(1/vol_6b[7]+1/(vol_6b[8]*dens_SLS*Kp_as))
  diff_6b[9,8]  <- (log(2)/t50_SLS)/(dens_SLS*Kp_as/vol_6b[7]+1/vol_6b[8])


  # --- External concentration chemical imported into system (g/m3)
  conc_ext_9b    <- rep(0,9)
  conc_ext_9b[2] <- C_diss_rs
  conc_ext_9b[3] <- C_solids_rs*dens_rs

  conc_ext_6b    <- rep(0,9)
  conc_ext_6b[5] <- C_diss_rs
  conc_ext_6b[6] <- C_solids_rs*dens_rs

  # --- First order biodegradation rate per compartment (s-1)
  biodeg_9b      <- rep(0,9)
  biodeg_9b[5]   <- k_bio_WWTP       #only degradation in aqueuous phase of activated sludge assumed

  biodeg_6b      <- rep(0,9)
  biodeg_6b[5]   <- k_bio_WWTP       #only degradation in aqueuous phase of activated sludge assumed

  # --- Mass balance calculations
  coef_9box   <- matrix(0,nrow=10,ncol=10)
  for (i in 2:10) {for (j in 2:10) {coef_9box[i,j]=-adv_9b[j,i]-diff_9b[j,i] } }
  for (i in 2:10){coef_9box[i,i]=sum(adv_9b[i,],diff_9b[i,],biodeg_9b[i-1]*vol_9b[i-1])}

  coef_1_9box <- solve(coef_9box[c(2:10),c(2:10)])

  constant_9b <- rep(0,9)
  for (i in 1:9) {constant_9b[i]=adv_9b[1,i+1]*conc_ext_9b[i]}

  conc_9b <- data.frame(conc9b=coef_1_9box%*%constant_9b)             #concentration per compartment - 9box (g/m3)


  M_in_9b     <- adv_9b[1,3]*conc_ext_9b[2]+adv_9b[1,4]*conc_ext_9b[3]    #total mass inflow - 9box (g/s)
  M_out_9b    <- sum(adv_9b[2:10,1]*conc_9b[,1])                          #total mass outflow - 9box (g/s)

  # print(adv_9b)
  # print(adv_9b[2:10,1])
  # print(conc_9b)
  # print(conc_9b[,1])
  # print(M_out_9b)

  coef_6box   <- matrix(0,nrow=10,ncol=10)
  for (i in 2:10) {for (j in 2:10) {coef_6box[i,j]=-adv_6b[j,i]-diff_6b[j,i] } }

  for (i in 2:10){coef_6box[i,i]=sum(adv_6b[i,],diff_6b[i,],biodeg_6b[i-1]*vol_6b[i-1])}
  coef_1_6box <- solve(coef_6box[c(2,6:10),c(2,6:10)])

  constant_6b <- rep(0,9)
  for (i in 1:9) {constant_6b[i]=adv_6b[1,i+1]*conc_ext_6b[i]}
  conc_6b     <- data.frame(conc_6b=coef_1_6box%*%constant_6b[c(1,5:9)])  #concentration per compartment - 9box (g/m3)
  #print(conc_6b);
  M_in_6b     <- adv_6b[1,6]*conc_ext_6b[5]+adv_6b[1,7]*conc_ext_6b[6]    #total mass inflow - 9box (g/s)
  M_out_6b    <- sum(adv_6b[c(2,6:10),1]*conc_6b[,1])                     #total mass outflow - 9box (g/s)


  # --- SimpleTreat Output
  #Elimination in primary settler (only 9box)
  f_rem_PS_vol      <- (conc_9b[2,1]*diff_9b[3,2]-conc_9b[1,1]*diff_9b[2,3])/M_in_9b  #fraction removed in primary settler via volatilisation (-)
  f_rem_PS_sludge   <- (conc_9b[4,1]*adv_9b[5,1])/M_in_9b                             #fraction removed in primary settler via primary sludge (-)
  f_rem_PS_tot      <- f_rem_PS_vol+f_rem_PS_sludge                                   #total fraction removed in primary settler (-)

  #Elimination in aerator (9box or 6box)
  f_rem_ae_strip_9b <- (diff_9b[6,2]*conc_9b[5,1]-diff_9b[2,6]*conc_9b[1,1])/M_in_9b                      #fraction removed in aerator via stripping - 9box (-)
  f_rem_ae_bio_9b   <- (biodeg_9b[5]*vol_9b[5]*conc_9b[5,1]+biodeg_9b[6]*vol_9b[6]*conc_9b[6,1])/M_in_9b  #fraction removed in aerator via biodegradation - 9box (-)
  f_rem_ae_tot_9b   <- f_rem_ae_strip_9b+f_rem_ae_bio_9b                                                  #total fraction removed in aerator - 9box (-)

  f_rem_ae_strip_6b <- (diff_6b[6,2]*conc_6b[2,1]-diff_6b[2,6]*conc_6b[1,1])/M_in_6b                      #fraction removed in aerator via stripping - 6box (-)
  f_rem_ae_bio_6b   <- (biodeg_6b[5]*vol_6b[5]*conc_6b[2,1]+biodeg_6b[6]*vol_6b[6]*conc_6b[3,1])/M_in_6b  #fraction removed in aerator via biodegradation - 6box (-)
  f_rem_ae_tot_6b   <- f_rem_ae_strip_6b+f_rem_ae_bio_6b                                                  #total fraction removed in aerator - 6box (-)

  #Elimination in solids liquid separator (9box or 6box)
  f_rem_SLS_vol_9b    <- (diff_9b[8,2]*conc_9b[7,1]-diff_9b[2,8]*conc_9b[1,1])/M_in_9b  #fraction removed in solids liquid separator via volatilisation - 9box (-)
  f_rem_SLS_sludge_9b <- (adv_9b[10,1]*conc_9b[9,1])/M_in_9b                            #fraction removed in solids liquid separator via surplus sludge - 9box (-)
  f_rem_SLS_tot_9b    <- f_rem_SLS_vol_9b+f_rem_SLS_sludge_9b                           #total fraction removed in solids liquid separator - 9box (-)

  f_rem_SLS_vol_6b    <- (diff_6b[8,2]*conc_6b[4,1]-diff_6b[2,8]*conc_6b[1,1])/M_in_6b  #fraction removed in solids liquid separator via volatilisation - 6box (-)
  f_rem_SLS_sludge_6b <- (adv_6b[10,1]*conc_6b[6,1])/M_in_6b                            #fraction removed in solids liquid separator via surplus sludge - 6box (-)
  f_rem_SLS_tot_6b    <- f_rem_SLS_vol_6b+f_rem_SLS_sludge_6b                           #total fraction removed in solids liquid separator - 6box (-)

  #Total elimination (9box or 6box)
  f_rem_tot_9b  <- f_rem_PS_tot+f_rem_ae_tot_9b+f_rem_SLS_tot_9b  #total elimination fraction from wastewater - 9box (-)
  f_eff_diss_9b <- (adv_9b[8,1]*conc_9b[7,1])/M_in_9b             #total fraction emitted via effluent, dissolved - 9box (-)
  f_eff_ass_9b  <- (adv_9b[9,1]*conc_9b[8,1])/M_in_9b             #total fraction emitted via effluent, associated - 9box (-)
  f_eff_tot_9b  <- f_eff_diss_9b+f_eff_ass_9b                     #total fraction emitted via effluent - 9box (-)

  f_rem_tot_6b  <- f_rem_ae_tot_6b+f_rem_SLS_tot_6b     #total elimination fraction from wastewater - 6box (-)
  f_eff_diss_6b <- (adv_6b[8,1]*conc_6b[4,1])/M_in_6b   #total fraction emitted via effluent, dissolved - 6box (-)
  f_eff_ass_6b  <- (adv_6b[9,1]*conc_6b[5,1])/M_in_6b   #total fraction emitted via effluent, associated - 6box (-)
  f_eff_tot_6b  <- f_eff_diss_6b+f_eff_ass_6b           #total fraction emitted via effluent - 6box (-)

  #Summary of distribution (9box or 6box)
  f_air_9b     <- adv_9b[2,1]*conc_9b[1,1]/M_in_9b                                                  #fraction of chemical emitted to air - 9box (-)
  f_water_9b   <- (adv_9b[8,1]*conc_9b[7,1]+adv_9b[9,1]*conc_9b[8,1])/M_in_9b                       #fraction of chemical emitted to water - 9box (-)
  f_ps_9b      <- adv_9b[5,1]*conc_9b[4,1]/M_in_9b                                                  #fraction of chemical removed via primary sludge - 9box (-)
  f_as_9b      <- adv_9b[10,1]*conc_9b[9,1]/M_in_9b                                                 #fraction of chemical removed via surplus sludge - 9box (-)
  f_deg_9b     <- (biodeg_9b[5]*vol_9b[5]*conc_9b[5,1]+biodeg_9b[6]*vol_9b[6]*conc_9b[6,1])/M_in_9b #fraction of chemical degraded - 9box (-)

  f_air_6b     <- adv_6b[2,1]*conc_6b[1,1]/M_in_6b                                                  #fraction of chemical emitted to air - 9box (-)
  f_water_6b   <- (adv_6b[8,1]*conc_6b[4,1]+adv_6b[9,1]*conc_6b[5,1])/M_in_6b                       #fraction of chemical emitted to water - 9box (-)
  f_as_6b      <- adv_6b[10,1]*conc_6b[6,1]/M_in_6b                                                 #fraction of chemical removed via surplus sludge - 9box (-)
  f_deg_6b     <- (biodeg_6b[5]*vol_6b[5]*conc_6b[2,1]+biodeg_6b[6]*vol_6b[6]*conc_6b[3,1])/M_in_6b #fraction of chemical degraded - 9box (-)

  #Concentrations (9box or 6box)
  conc_air_9b     <- conc_9b[1,1]                                                                   #concentration in air - 9box (g/m3)
  conc_comb_s_9b  <- (conc_9b[4,1]*adv_9b[5,1]+conc_9b[9,1]*adv_9b[10,1])/                          #concentration in combined sludge - 9box (mg/kg)
    (adv_9b[5,1]*dens_PS+adv_9b[10,1]*dens_SLS)
  conc_ps_9b      <- conc_9b[4,1]/dens_PS                                                           #concentration in primary sludge - 9box (mg/kg)
  conc_as_9b      <- conc_9b[9,1]/dens_SLS                                                          #concentration in surplus sludge - 9box (mg/kg)
  conc_ml_diss_9b <- conc_9b[5,1]                                                                   #concentration in mixed liquor, dissolved - 9box (mg/L)
  conc_ml_ass_9b  <- conc_9b[6,1]/(1000*dens_as)*C_as                                               #concentration in mixed liquor, associated - 9box (mg/L)
  conc_ml_tot_9b  <- conc_ml_diss_9b+conc_ml_ass_9b                                                 #total concentration in mixed liquor - 9box (mg/L)
  conc_eff_diss_9b <- conc_9b[7,1]                                                                  #concentration in effluent, dissolved - 9box (mg/L)
  conc_eff_ass_9b <- conc_9b[8,1]/dens_SLS*C_ss_SLS/1000                                            #concentration in effluent, associated - 9box (mg/L)
  conc_eff_tot_9b <- conc_eff_diss_9b+conc_eff_ass_9b                                               #total concentration in effluent - 9box (mg/L)
  conc_eff_sol_9b <- conc_9b[8,1]/dens_SLS                                                          #concentration in solids effluent - 9box (mg/kg)

  conc_air_6b     <- conc_6b[1,1]                               #concentration in air - 6box (g/m3)
  conc_as_6b      <- conc_6b[6,1]/dens_SLS                      #concentration in surplus sludge - 6box (mg/kg)
  conc_ml_diss_6b <- conc_6b[2,1]                               #concentration in mixed liquor, dissolved - 6box (mg/L)
  conc_ml_ass_6b  <- conc_6b[3,1]/(1000*dens_as)*C_as           #concentration in mixed liquor, associated - 6box (mg/L)
  conc_ml_tot_6b  <- conc_ml_diss_6b+conc_ml_ass_6b             #total concentration in mixed liquor - 6box (mg/L)
  conc_eff_diss_6b <- conc_6b[4,1]                              #concentration in effluent, dissolved - 6box (mg/L)
  conc_eff_ass_6b <- conc_6b[5,1]/dens_SLS*C_ss_SLS/1000        #concentration in effluent, associated - 6box (mg/L)
  conc_eff_tot_6b <- conc_eff_diss_6b+conc_eff_ass_6b           #total concentration in effluent - 6box (mg/L)
  conc_eff_sol_6b <- conc_6b[5,1]/dens_SLS                      #concentration in solids effluent - 6box (mg/kg)

  #print(conc_eff_sol_6b)

  if (PRIM==-1 & SEC==-1) {
    list(f_rem=f_rem_tot_9b, C_sludge=conc_as_9b)
  } else if (PRIM==-1 & SEC==0) {
    list(f_rem=f_rem_PS_tot,C_sludge=0)
  } else if (PRIM==0 & SEC==-1) {
    list(f_rem=f_rem_tot_6b, f_sludge=conc_as_6b)
  } else if (PRIM==0 & SEC==0) {
    list(f_rem=0, C_sludge=0)
  }
}
