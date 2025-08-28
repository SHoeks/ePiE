Chem_complete <- function(chem){

  chem$MW<-as.numeric(chem$MW)

  #urinary excretion and fecal egestion
  chem$f_u <- ifelse(is.na(chem$f_u),ifelse(is.na(chem$f_f),1,0),chem$f_u)
  chem$f_f <- ifelse(is.na(chem$f_f),0,chem$f_f)

  #biodegradation in WWTP
  chem$k_bio_wwtp_n <- ifelse(is.na(chem$k_bio_wwtp_n),0,chem$k_bio_wwtp_n) #worst-case no WWTP biodegradation if k_bio_wwtp = NA
  chem$k_bio_wwtp_alt <- ifelse(is.na(chem$k_bio_wwtp_alt),0,chem$k_bio_wwtp_alt)

  chem$fn_WWTP <- ifelse(chem$class=="neutral",1, #fraction neutral in WWTP (pH=7)
                         ifelse(chem$class=="acid",1/(1+10^(7-chem$pKa)),
                                ifelse(chem$class=="base",1/(1+10^(chem$pKa-7)),NA)))

  chem$k_bio_wwtp <- chem$fn_WWTP * chem$k_bio_wwtp_n + (1 - chem$fn_WWTP) * chem$k_bio_wwtp_alt

  #sorption
  chem$Kp_ps_n <- ifelse(is.na(chem$Kp_ps_n)&!is.na(chem$Kp_as_n),(0.30/0.37)*chem$Kp_as_n,chem$Kp_ps_n) #if Kp_ps=NA, extrapolate from Kp_as based on fOC in respective sludges
  chem$Kp_ps_alt <- ifelse(is.na(chem$Kp_ps_alt)&!is.na(chem$Kp_as_alt),(0.30/0.37)*chem$Kp_as_alt,chem$Kp_ps_alt) #if Kp_ps=NA, extrapolate from Kp_as based on fOC in respective sludges

  chem$Kp_as_n <- ifelse(is.na(chem$Kp_as_n)&!is.na(chem$Kp_ps_n),(0.37/0.30)*chem$Kp_ps_n,chem$Kp_as_n) #if Kp_as=NA, extrapolate from Kp_ps based on fOC in respective sludges
  chem$Kp_as_alt <- ifelse(is.na(chem$Kp_as_alt)&!is.na(chem$Kp_ps_alt),(0.37/0.30)*chem$Kp_ps_alt,chem$Kp_as_alt) #if Kp_as=NA, extrapolate from Kp_ps based on fOC in respective sludges

  #estimation of KOC of neutral form based on Sabljic et al (1995) or based on Franco and Trapp (2008)
  chem$KOC_n <- ifelse(!is.na(chem$KOC_n),chem$KOC_n,
                       ifelse(chem$class=="neutral",1.26*chem$KOW_n^0.81,
                                                      ifelse(chem$class=="acid",10^(0.54*log10(chem$KOW_n)+1.11),
                                                             ifelse(chem$class=="base",10^(0.37*log10(chem$KOW_n)+1.70),NA))))

  #estimation of KOC of alternate form based on Franco and Trapp (2008)
  chem$KOC_alt <- ifelse(!is.na(chem$KOC_alt),chem$KOC_alt,
                         ifelse(chem$class=="neutral",chem$KOC_n,
                                ifelse(chem$class=="acid",10^(0.11*log10(chem$KOW_n)+1.54),
                                       ifelse(chem$class=="base",10^(chem$pKa^0.65*(chem$KOW_n/(chem$KOW_n+1))^0.14),NA))))

  chem$KOW_alt <- 10^(log10(chem$KOW_n)-3.5)                                #assumption that KOW of alternate form is about 3.5 log-points lower than the neutral KOW (Trapp & Horobin, 2005)

  chem$Kp_ps_n <- ifelse(is.na(chem$Kp_ps_n),0.30*chem$KOC_n,chem$Kp_ps_n) #estimation of Kp_ps from KOC of neutral and alternate form
  chem$Kp_ps_alt <- ifelse(is.na(chem$Kp_ps_alt),0.30*chem$KOC_alt,chem$Kp_ps_alt)
  chem$Kp_as_n <- ifelse(is.na(chem$Kp_as_n),0.37*chem$KOC_n,chem$Kp_as_n)
  chem$Kp_as_alt <- ifelse(is.na(chem$Kp_as_alt),0.37*chem$KOC_n,chem$Kp_as_alt)

  chem$Kp_ps <- chem$Kp_ps_n*chem$fn_WWTP + chem$Kp_ps_alt*(1-chem$fn_WWTP)
  chem$Kp_as <- chem$Kp_as_n*chem$fn_WWTP + chem$Kp_as_alt*(1-chem$fn_WWTP)



  #biodegradation
  chem$k_bio_sw1_n <- ifelse(is.na(chem$k_bio_sw1_n),0,chem$k_bio_sw1_n) #worst-case no environmental biodegradation if k_bio_sw1 = NA
  chem$k_bio_sw1_alt <- ifelse(is.na(chem$k_bio_sw1_alt),0,chem$k_bio_sw1_alt) #worst-case no environmental biodegradation if k_bio_sw1 = NA

  chem$BACT_test <- 1e6 #bacterial density is not available --> assume 1e6
  chem$k_bio_sw2_n <- chem$k_bio_sw1_n/chem$BACT_test #2nd order biodeg is not available --> calculate from pseudo-first order
  chem$k_bio_sw2_alt <- chem$k_bio_sw1_alt/chem$BACT_test

  chem$T_bio_sw_n <- ifelse(is.na(chem$T_bio_sw_n),293.15,chem$T_bio_sw_n)
  chem$T_bio_sw_alt <- ifelse(is.na(chem$T_bio_sw_alt),293.15,chem$T_bio_sw_alt)
  chem$T_bio_sd_n <- ifelse(is.na(chem$T_bio_sd_n),293.15,chem$T_bio_sd_n)
  chem$T_bio_sd_alt <- ifelse(is.na(chem$T_bio_sd_alt),293.15,chem$T_bio_sd_alt)

  #hydrolysis
  chem$k_hydro_sw_n <- ifelse(is.na(chem$k_hydro_sw_n),0,chem$k_hydro_sw_n) #worst-case no environmental hydrolysis if k_hydro_sw = NA
  chem$k_hydro_sw_alt <- ifelse(is.na(chem$k_hydro_sw_alt),0,chem$k_hydro_sw_alt)

  chem$T_hydro_sw_n <- ifelse(is.na(chem$T_hydro_sw_n),293.15,chem$T_hydro_sw_n)
  chem$T_hydro_sw_alt <- ifelse(is.na(chem$T_hydro_sw_alt),293.15,chem$T_hydro_sw_alt)
  chem$T_hydro_sd_n <- ifelse(is.na(chem$T_hydro_sd_n),293.15,chem$T_hydro_sd_n)
  chem$T_hydro_sd_alt <- ifelse(is.na(chem$T_hydro_sd_alt),293.15,chem$T_hydro_sd_alt)

  #photolysis
  chem$k_photo12_sw_n <- ifelse(is.na(chem$k_photo12_sw_n),0,chem$k_photo12_sw_n)
  chem$k_photo12_sw_alt <- ifelse(is.na(chem$k_photo12_sw_alt),0,chem$k_photo12_sw_alt)
  chem$T_photo12_sw_n <- ifelse(is.na(chem$T_photo12_sw_n),293.15,chem$T_photo12_sw_n)
  chem$T_photo12_sw_alt <- ifelse(is.na(chem$T_photo12_sw_alt),293.15,chem$T_photo12_sw_alt)

  #tabular relationship between lambda, alpha and solar radiation intensity, applicable to clear midsummer day at 47.5degN (Schwarzenbach et al., 1993)
  lambda_range  <- c(0,298.75,301.25,303.75,306.75,308.75,311.25,313.75,316.25,318.75,321.25,325,335,345,355,365,375,385,395,405,435,465,495)
  alpha_range   <- c(0.0430,0.0415,0.0395,0.0375,0.0355,0.0335,0.0320,0.0305,0.0290,0.0275,0.0260,0.0220,0.0185,0.0150,0.0125,0.0100,0.0083,0.0069,0.0055,0.0042,0.0028,0.0019,0.0010)

  for (i in 1:nrow(chem)) {
    if(is.na(chem$lambda_solar_n[i])){chem$alpha_n[i]=1e-6}else{ # assume low chem$alpha
      chem$alpha_n[i]      <- alpha_range[findInterval(chem$lambda_solar_n[i],lambda_range)] #beam attenuation coefficient for light of wavelength lambda (cm-1)
    }
  }

  for (i in 1:nrow(chem)) {
    if(is.na(chem$lambda_solar_alt[i])){chem$alpha_alt[i]=1e-6}else{ # assume low chem$alpha
      chem$alpha_alt[i]      <- alpha_range[findInterval(chem$lambda_solar_alt[i],lambda_range)]      #beam attenuation coefficient for light of wavelength lambda (cm-1)
    }
  }

  return(chem)
}
