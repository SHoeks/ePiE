LoadExampleChemProperties = function(){
  chem = data.frame(API = "Ibuprofen", CAS = "15687-27-1", class = "acid",
                 MW = 206.2808, KOW_n = 9332.543, Pv = 0.0248, S = 21, pKa = 4.85,
                 f_u = 0.2, f_f = NA, metab = NA, API_metab = NA,
                 #Kp_ps_n = 10.77399, Kp_ps_alt = 10.77399, Kp_as_n = 200.0158, Kp_as_alt = 200.0158,
                 #Kp_sd_n = 14.97686624, Kp_sd_alt = 14.97686624,
                 #KOC_n = NA, KOC_alt = NA,
                 k_bio_wwtp_n = 0.000197158, k_bio_wwtp_alt = 0.000197158,
                 #k_bio_sw1_n = 8.27e-07, T_bio_sw_n = 293.15, k_bio_sw1_alt = 8.27e-07,T_bio_sw_alt = 293.15,
                 #k_hydro_sw_n = 2.31e-07,T_hydro_sw_n = 293.15, k_hydro_sw_alt = 2.31e-07, T_hydro_sw_alt = 293.15,
                 #lambda_solar_n = 265L, k_photo12_sw_n = 3.85e-06, T_photo12_sw_n = 293.15,
                 #lambda_solar_alt = 265L, k_photo12_sw_alt = 3.85e-06,T_photo12_sw_alt = 293.15,
                 #Kp_susp_n = NA, Kp_susp_alt = NA, Kp_DOC_n = NA, Kp_DOC_alt = NA,
                 #k_bio_sd1_n = NA, T_bio_sd_n = NA,k_bio_sd1_alt = NA, T_bio_sd_alt = NA,k_hydro_sd_n = NA, T_hydro_sd_n = NA, k_hydro_sd_alt = NA,T_hydro_sd_alt = NA,
                 custom_wwtp_primary_removal = NA,custom_wwtp_secondary_removal = NA,
                 custom_wwtp_N_removal = 0,custom_wwtp_P_removal = 0, custom_wwtp_UV_removal = 0, custom_wwtp_Cl_removal = 0,
                 custom_wwtp_O3_removal = 0, custom_wwtp_sandfilter_removal = 0,custom_wwtp_microfilter_removal = 0)
  return(chem)
}

LoadExampleConsumption = function(per_capita_kg = 0.0045){
  cons = structure(list(cnt = c("AD", "AL", "AM", "AT", "AZ", "BA", "BE",
                                "BG", "BY", "CH", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI",
                                "FR", "FX", "GE", "HR", "HU", "IE", "IS", "IT", "LI", "LT", "LU",
                                "LV", "MC", "MD", "ME", "MK", "MT", "NL", "NO", "PL", "PT", "RO",
                                "RS", "RU", "SE", "SI", "SK", "SM", "TR", "UA", "UK", "XK", "GR"),
                        population = c(76177, 2862427, 2965269, 8858775, 9981457,
                                      3693880, 11455519, 7000039, 9475174, 8544527, 875899, 10649800,
                                      83019213, 5806081, 1324820, 10724599, 46937060, 5517919, 67290471,
                                      63536918, 3723464, 4076246, 9772756, 4825519.09090909, 356991,
                                      60030451.7, 38378, 2794184, 613894, 1919968, 38250, 3210598.66666667,
                                      622182, 2077132, 493559, 17282163, 5328212, 37972812, 10276617,
                                      19414458, 6963764, 143361657, 10230185, 2080908, 5450421, 34590,
                                      80073732.2727273, 41983564, 66647112, 1795666, 10640000)),
                   class = "data.frame", row.names = c(NA,-51L))
  cons$year = 2019
  cons$Ibuprofen = cons$population * per_capita_kg
  return(cons)
}
