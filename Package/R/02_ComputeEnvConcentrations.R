ComputeEnvConcentrations = function(basin_data,chem,cons,verbose=FALSE,cpp=FALSE){

  # extract river points and lakes
  pts = basin_data$pts
  hl = basin_data$hl

  for (chem_ii in 1:nrow(chem)) {

    pts.backup = pts
    chem.backup = chem

    # Set local (spatial-specific) parameters for pts and HL (overwrites current variables; pts and HL)
    pts_hl = Set_local_parameters_custom_removal_fast3(pts,hl,cons,chem,chem_ii)

    # Fate calculations rivers and lakes
    if(cpp){

      # Basin without lakes, add placeholder
      idx = which(!pts_hl$pts$basin_id%in%pts_hl$hl$basin_id)
      basin_ids_no_lakes = unique(pts_hl$pts$basin_id[idx])
      if(nrow(pts_hl$hl)>0 & length(basin_ids_no_lakes)>0){
        tmp = pts_hl$hl[1,]
        tmp = tmp[rep(1, each = length(basin_ids_no_lakes)), ]
        tmp$Hylak_id = -99999
        tmp$basin_id = basin_ids_no_lakes
        tmp$Lake_name = "placeholder"
        tmp$E_in = 0
        pts_hl$hl = rbind(pts_hl$hl,tmp)
      }else if(nrow(pts_hl$hl)==0 & length(basin_ids_no_lakes)>0){
        tmp = data.frame(Vol_total = 0,
                        k = 0,
                        k_ws = 0,
                        Depth_avg = 0,
                        H_sed = 0,
                        poros = 0,
                        rho_sd = 0,
                        Hylak_id = -99999,
                        E_in = 0,
                        k_sw = 0,
                        basin_id = NA)
        tmp = tmp[rep(1, each = length(basin_ids_no_lakes)), ]
        tmp$basin_id = basin_ids_no_lakes
        tmp$Lake_name = "placeholder"
        tmp$E_in = 0
        pts_hl$hl = tmp
      }

      # Translation basin_id df
      unique_basins = unique(pts_hl$pts$basin_id)
      basin_id_df = data.frame(basin_id=unique_basins,new_id=1:length(unique_basins))
      pts_hl$hl$basin_id = basin_id_df$new_id[match(pts_hl$hl$basin_id,basin_id_df$basin_id)]
      pts_hl$pts$basin_id = basin_id_df$new_id[match(pts_hl$pts$basin_id,basin_id_df$basin_id)]

      # Convert data types
      pts_hl$hl$basin_id = as.integer(pts_hl$hl$basin_id)
      pts_hl$hl$Hylak_id = as.integer(pts_hl$hl$Hylak_id)
      pts_hl$pts$basin_id = as.integer(pts_hl$pts$basin_id)
      pts_hl$pts$Hylak_id = as.integer(pts_hl$pts$Hylak_id)

      # Check verbose
      if(class(verbose)!="logical"){verbose = TRUE}

      results = Compute_env_concentrations_v4_cpp(
        pts_ID = pts_hl[[1]]$ID, # std::vector<std::string>
        pts_ID_nxt = pts_hl[[1]]$ID_nxt, # std::vector<std::string>
        pts_basin_id = pts_hl[[1]]$basin_id, # std::vector<int>
        pts_upcount = pts_hl[[1]]$upcount, # std::vector<int>
        pts_lake_out = pts_hl[[1]]$lake_out, # std::vector<int>
        pts_Hylak_id = pts_hl[[1]]$Hylak_id, # std::vector<int>
        pts_E_w = pts_hl[[1]]$E_w, # std::vector<double>
        pts_E_up = pts_hl[[1]]$E_up, # std::vector<double>
        pts_Q = pts_hl[[1]]$Q, # std::vector<double>
        pts_E_w_NXT = pts_hl[[1]]$E_w_NXT, # std::vector<double>
        pts_k_NXT = pts_hl[[1]]$k_NXT, # std::vector<double>
        pts_k_ws = pts_hl[[1]]$k_ws, # std::vector<double>
        pts_k_sw = pts_hl[[1]]$k_sw, # std::vector<double>
        pts_H_sed = pts_hl[[1]]$H_sed, # std::vector<double>
        pts_H = pts_hl[[1]]$H, # std::vector<double>
        pts_poros = pts_hl[[1]]$poros, # std::vector<double>
        pts_rho_sd = pts_hl[[1]]$rho_sd, # std::vector<double>
        pts_dist_nxt = pts_hl[[1]]$dist_nxt, # std::vector<double>
        pts_V_NXT = pts_hl[[1]]$V_NXT, # std::vector<double>
        pts_f_rem_WWTP = pts_hl[[1]]$f_rem_WWTP, # std::vector<double>
        pts_x = pts_hl[[1]]$x, # std::vector<double>
        pts_y = pts_hl[[1]]$y, # std::vector<double>
        pts_Pt_type = pts_hl[[1]]$Pt_type, # std::vector<std::string>
        hl_Vol_total = pts_hl[[2]]$Vol_total, # std::vector<double>
        hl_k = pts_hl[[2]]$k, # std::vector<double>
        hl_k_ws = pts_hl[[2]]$k_ws, # std::vector<double>
        hl_Depth_avg = pts_hl[[2]]$Depth_avg, # std::vector<double>
        hl_H_sed = pts_hl[[2]]$H_sed, # std::vector<double>
        hl_poros = pts_hl[[2]]$poros, # std::vector<double>
        hl_rho_sd = pts_hl[[2]]$rho_sd, # std::vector<double>
        hl_Hylak_id = pts_hl[[2]]$Hylak_id, # std::vector<int>
        hl_E_in = pts_hl[[2]]$E_in, # std::vector<double>
        hl_k_sw = pts_hl[[2]]$k_sw, # std::vector<double>
        hl_basin_id = pts_hl[[2]]$basin_id, # std::vector<int>
        print = verbose
      )

      # Remove placeholder lakes
      results$HL = results$HL[results$HL$Hylak_id>0,]

      # Reset basin ids
      results$pts$basin_ID = basin_id_df$basin_id[match(results$pts$basin_ID,basin_id_df$new_id)]
      idx = which(results$HL$Hylak_id==pts_hl[[2]]$Hylak_id)
      results$HL$basin_id = pts_hl[[2]]$basin_id[idx]
      results$HL$basin_id = basin_id_df$basin_id[match(results$HL$basin_id,basin_id_df$new_id)]

    }else{

      # Run non-C++ version
      results = Compute_env_concentrations_v4(pts_hl[[1]],pts_hl[[2]],print=verbose)

    }

    results[[1]]$API = chem$API[chem_ii]
    if (nrow(hl)!=0) results[[2]]$API = chem$API[chem_ii]

    # Create output
    if(nrow(hl)!=0) {
      if(chem_ii==1){
        results_com = results[[1]]
        results_lakes = results[[2]]
      }else{
        results_com = rbind(results_com,results[[1]])
        results_lakes = rbind(results_lakes,results[[2]])
      }
    }else{
      results_lakes = NULL
      if(chem_ii==1) {
        results_com = results[[1]]
      }else{
        results_com = rbind(results_com,results[[1]])
      }
    }

    pts = pts.backup
    chem = chem.backup
  }

  # Format output
  out = list(pts=results_com,hl=results_lakes)
  out$pts$basin_id = out$pts$basin_ID
  out$pts$basin_ID = NULL

  # Return output
  return(out)
}
