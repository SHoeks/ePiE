Compute_env_concentrations_v3 = function(pts,HL,print=TRUE){

  # testing purpose
  if(FALSE){
    pts = Set_local_parameters_custom_removal_fast2(basins_avg$pts,basins_avg$hl,cons,chem,1,UseCpp=FALSE)[[1]]
    HL = Set_local_parameters_custom_removal_fast2(basins_avg$pts,basins_avg$hl,cons,chem,1,UseCpp=FALSE)[[2]]
    print = TRUE
  }

  #store all columns as vectors (faster)
  for(i in 1:ncol(pts)) assign(paste('pts.',colnames(pts)[i],sep=''),pts[,i])
  for(i in 1:ncol(HL)) assign(paste('HL.',colnames(HL)[i],sep=''),HL[,i])
  if(!exists("pts.Hylak_id")) pts.Hylak_id = rep(1,length(pts.ID))
  if(!exists("pts.lake_out")) pts.lake_out = rep(0,length(pts.ID))

  #store initial order pts
  pts.order = 1:nrow(pts)
  break.vec1 = c();

  # precalculate indexing
  HL_indices_match = match(pts.Hylak_id,HL.Hylak_id)
  pts_indices_down = match(paste0(pts.basin_id,'_',pts.ID_nxt),paste0(pts.basin_id,'_',pts.ID))

  #continue looping until all points and lakes are assessed
  while (any(pts.fin==0)){

    # Print progress
    if(print){
      print(paste('# points in pts:',sum(pts.fin == 0),sep=' '))
      print(paste('# points in HL:',ifelse(nrow(HL)!=0,sum(HL.fin==0),0),sep=' '))
    }

    # break if stuck in while loop
    break.vec1 = c(break.vec1,sum(pts.fin == 0));
    if(length(break.vec1)-length(unique(break.vec1))>10) break

    # for loop
    for (j in 1:nrow(pts)) {

      # prepare indexing
      if(pts.fin[j]==0){
        HL_index_match = HL_indices_match[j]
        pts_index_down = pts_indices_down[j]
      }

      # processed point (or not)
      if(pts.fin[j]==0 & pts.upcount[j]==0) {

        if (!is.na(match(pts.basin_id[j], HL.basin_id)) & (pts.lake_out[j] == 1)) {

          # lake conc
          E_total = HL.E_in[HL_index_match] + pts.E_w[j] + pts.E_up[j] #total emission into lake (kg/yr)

          V = HL.Vol_total[HL_index_match] * 1e6  #volume (m3)
          k = HL.k[HL_index_match]          #k (1/sec)
          pts.C_w[j] = E_total / (pts.Q[j] + k * V) * 1e6 / (365*24*3600) #mg/m3 = ug/L

          chem_exchange = HL.k_ws[HL_index_match] / HL.k_sw[HL_index_match]
          H_ratio = HL.Depth_avg[HL_index_match] / HL.H_sed[HL_index_match]
          dens_transform = HL.poros[HL_index_match] + (1 - HL.poros[HL_index_match]) * HL.rho_sd[HL_index_match]

          pts.C_sd[j] = pts.C_w[j] * chem_exchange * H_ratio * dens_transform #ug/kg

          #assign concentrations to outflow point and lake
          HL.C_w[HL_index_match] = pts.C_w[j]
          HL.C_sd[HL_index_match] = pts.C_sd[j]

          #mark lake as done
          HL.fin[HL_index_match] = 1

          #contribution to next point downstream
          pts.E_w_NXT[j] = pts.C_w[j] * pts.Q[j] * 365 * 24 * 3600 / 1e6 * exp(-pts.k_NXT[j] * pts.dist_nxt[j] / pts.V_NXT[j]) #kg/yr
          pts.E_up[pts_index_down] = pts.E_up[pts_index_down] + pts.E_w_NXT[j]
          pts.upcount[pts_index_down] = pts.upcount[pts_index_down] - 1


        } else if ((pts.Hylak_id[j] == 0) | (pts.lake_out[j] == 1)) {

          # node and lake out conc
          E_total = pts.E_w[j] + pts.E_up[j] #total mass flowing into node (kg/yr)
          pts.C_w[j] = E_total / pts.Q[j] * 1e6 / (365*24*3600) #mg/m3 = ug/L

          chem_exchange = pts.k_ws[j] / pts.k_sw[j]
          H_ratio = pts.H[j] / pts.H_sed[j]
          dens_transform = pts.poros[j] + (1 - pts.poros[j]) * pts.rho_sd[j]

          pts.C_sd[j] = pts.C_w[j] * chem_exchange * H_ratio * dens_transform #ug/kg

          #contribution to next point downstream
          pts.E_w_NXT[j] = E_total * exp(-pts.k_NXT[j] * pts.dist_nxt[j] / pts.V_NXT[j]) #kg/yr
          pts.E_up[pts_index_down] = pts.E_up[pts_index_down] + pts.E_w_NXT[j]
          pts.upcount[pts_index_down] = pts.upcount[pts_index_down] - 1

        } else {

          # lake node conc
          E_total = pts.E_w[j] + pts.E_up[j] #total mass flowing into lake node (kg/yr)
          pts.C_w[j] = NA
          pts.C_sd[j] = NA

          #contribution to next point downstream
          pts.E_w_NXT[j] = E_total
          pts.E_up[pts_index_down] = pts.E_up[pts_index_down] + pts.E_w_NXT[j]
          pts.upcount[pts_index_down] = pts.upcount[pts_index_down] - 1
        }

        # Mark this point as done
        pts.fin[j] = 1


      }

    } # end for loop

  } # end while loop
  if (nrow(HL)!=0) {
    return(list(pts=data.frame(ID=pts.ID,Pt_type=pts.Pt_type,ID_nxt=pts.ID_nxt,basin_ID=pts.basin_id,x=pts.x,y=pts.y,Q=pts.Q,C_w=pts.C_w,C_sd=pts.C_sd,WWTPremoval=pts.f_rem_WWTP),
         HL=data.frame(Hylak_id=HL.Hylak_id,C_w=HL.C_w,C_sd=HL.C_sd)))
  } else {
    return(list(pts=data.frame(ID=pts.ID,Pt_type=pts.Pt_type,ID_nxt=pts.ID_nxt,basin_ID=pts.basin_id,x=pts.x,y=pts.y,Q=pts.Q,C_w=pts.C_w,C_sd=pts.C_sd,WWTPremoval=pts.f_rem_WWTP)))
  }
}
