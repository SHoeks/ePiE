RunSimpleTreatBasinAvg = function(basins_data, chem){


  T_AIR = mean(basins_data$pts$T_AIR,na.rm = TRUE)
  Wind = mean(basins_data$pts$Wind,na.rm = TRUE)


  rm_frac = data.frame(API=chem$API,primary_rm_frac=rep(NA,nrow(chem)),secondary_rm_frac=rep(NA,nrow(chem)),total_rm_frac=rep(NA,nrow(chem)))

  for(chem_idx in 1:nrow(chem)){
    rm_frac$primary_rm_frac[chem_idx] = SimpleTreat4_0(chem_class=chem$class[chem_idx],
                                                       MW=chem$MW[chem_idx],
                                                       Pv=chem$Pv[chem_idx],
                                                       S=chem$S[chem_idx],
                                                       pKa=chem$pKa[chem_idx],
                                                       Kp_ps=chem$Kp_ps_n[chem_idx],
                                                       Kp_as=chem$Kp_as_n[chem_idx],
                                                       k_bio_WWTP=chem$k_bio_wwtp_n[chem_idx],
                                                       T_air=T_AIR,
                                                       Wind=Wind,
                                                       Inh=1,
                                                       E_rate=1,
                                                       PRIM=-1,
                                                       SEC=0)$f_rem

    rm_frac$total_rm_frac[chem_idx] = SimpleTreat4_0(chem_class=chem$class[chem_idx],
                                                     MW=chem$MW[chem_idx],
                                                     Pv=chem$Pv[chem_idx],
                                                     S=chem$S[chem_idx],
                                                     pKa=chem$pKa[chem_idx],
                                                     Kp_ps=chem$Kp_ps_n[chem_idx],
                                                     Kp_as=chem$Kp_as_n[chem_idx],
                                                     k_bio_WWTP=chem$k_bio_wwtp_n[chem_idx],
                                                     T_air=T_AIR,
                                                     Wind=Wind,
                                                     Inh=1,
                                                     E_rate=1,
                                                     PRIM=-1,
                                                     SEC=-1)$f_rem
  }


  primary_remaining = 1-rm_frac$primary_rm_frac
  total_remaining = 1-rm_frac$total_rm_frac
  rm_frac$secondary_rm_frac = (primary_remaining-total_remaining)/primary_remaining
  rm_frac$total_rm_frac = NULL
  return(rm_frac)

}

