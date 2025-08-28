ComputeEnvConcentrations = function(basin_data,chem,cons,print=FALSE,cpp=FALSE){

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

      if(FALSE){
        save.image("cpp_test_env.RData")
        load("inst/test/cpp_test_env.RData")
        Rcpp::sourceCpp("src/compenvcons_v4.cpp", rebuild = TRUE)
        results2 = ePiE:::Compute_env_concentrations_v4(pts_hl[[1]],pts_hl[[2]],print)
      }

      # basin without lakes, add placeholder
      idx = which(!pts_hl$pts$basin_id%in%pts_hl$hl$basin_id)
      basin_ids_no_lakes = unique(pts_hl$pts$basin_id[idx])
      if(nrow(pts_hl$hl)>0){
        tmp = pts_hl$hl[1,]
        tmp = tmp[rep(1, each = length(basin_ids_no_lakes)), ]
        tmp$Hylak_id = -99999
        tmp$basin_id = basin_ids_no_lakes
        tmp$Lake_name = "placeholder"
        tmp$E_in = 0
        pts_hl$hl = rbind(pts_hl$hl,tmp)
      }else{
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

      # convert data types
      pts_hl$hl$basin_id = as.integer(pts_hl$hl$basin_id)
      pts_hl$hl$Hylak_id = as.integer(pts_hl$hl$Hylak_id)

      results = Compute_env_concentrations_v4_cpp(
        pts_ID = pts_hl[[1]]$ID,
        pts_ID_nxt = pts_hl[[1]]$ID_nxt,
        pts_basin_id = pts_hl[[1]]$basin_id,
        pts_upcount = pts_hl[[1]]$upcount,
        pts_lake_out = pts_hl[[1]]$lake_out,
        pts_Hylak_id = pts_hl[[1]]$Hylak_id,
        pts_E_w = pts_hl[[1]]$E_w,
        pts_E_up = pts_hl[[1]]$E_up,
        pts_Q = pts_hl[[1]]$Q,
        pts_E_w_NXT = pts_hl[[1]]$E_w_NXT,
        pts_k_NXT = pts_hl[[1]]$k_NXT,
        pts_k_ws = pts_hl[[1]]$k_ws,
        pts_k_sw = pts_hl[[1]]$k_sw,
        pts_H_sed = pts_hl[[1]]$H_sed,
        pts_H = pts_hl[[1]]$H,
        pts_poros = pts_hl[[1]]$poros,
        pts_rho_sd = pts_hl[[1]]$rho_sd,
        pts_dist_nxt = pts_hl[[1]]$dist_nxt,
        pts_V_NXT = pts_hl[[1]]$V_NXT,
        pts_f_rem_WWTP = pts_hl[[1]]$f_rem_WWTP,
        pts_x = pts_hl[[1]]$x,
        pts_y = pts_hl[[1]]$y,
        pts_Pt_type = pts_hl[[1]]$Pt_type,
        hl_Vol_total = pts_hl[[2]]$Vol_total,
        hl_k = pts_hl[[2]]$k,
        hl_k_ws = pts_hl[[2]]$k_ws,
        hl_Depth_avg = pts_hl[[2]]$Depth_avg,
        hl_H_sed = pts_hl[[2]]$H_sed,
        hl_poros = pts_hl[[2]]$poros,
        hl_rho_sd = pts_hl[[2]]$rho_sd,
        hl_Hylak_id = pts_hl[[2]]$Hylak_id,
        hl_E_in = pts_hl[[2]]$E_in,
        hl_k_sw = pts_hl[[2]]$k_sw,
        hl_basin_id = pts_hl[[2]]$basin_id,
        print = print
      )

      # remove placeholder lakes
      results$HL = results$HL[results$HL$Hylak_id>0,]

    }else{
      results = Compute_env_concentrations_v4(pts_hl[[1]],pts_hl[[2]],print)
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

    pts=pts.backup
    chem= chem.backup
  }

  return(list(pts=results_com,hl=results_lakes))
}
