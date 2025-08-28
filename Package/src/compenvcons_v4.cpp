// #include "../inst/include/Compute_env_concentrations_v4.h"
#include <fenv.h>
#include <stdio.h>
#include <stdlib.h>
#include <list>
#include <dirent.h>
#include <string>
#include <iostream>
#include <vector>
#include <algorithm>
#include <ctime>
#include <cmath>
#include <limits>

#include <Rcpp.h>
using namespace Rcpp;

// helper functions ----------------------------------------------------

int count_not_finished(const std::vector<int>& fin) {
  int counter = 0;
  int n = fin.size();
  for( int i = 0;  i < n; i += 1 ) {
    if(fin[i]==0) counter++;
  }
  return counter;
}

int which(const std::vector<int>& vec, const int& match) {
  int counter = 0;
  int n = vec.size();
  for( int i = 0;  i < n; i += 1 ) {
    if(vec[i]==match) break;
    else counter++;
  }
  return counter;
}

int which_string(const std::vector<std::string>& vec1, const std::string& match1) {
  int counter = 0;
  int n = vec1.size();
  for( int i = 0;  i < n; i += 1 ) {
    if(vec1[i]==match1) break;
    else counter++;
  }
  return counter;
}

int exists_within(const std::vector<std::string>& vec1, const std::string& match1) {
  bool chck = false;
  int n = vec1.size();
  for( int i = 0;  i < n; i += 1 ) {
    if(vec1[i]==match1) {
      chck = true;
      break;
    }
  }
  return chck;
}

int exists_within(const std::vector<int>& vec1, const int& match1) {
  bool chck = false;
  int n = vec1.size();
  for( int i = 0;  i < n; i += 1 ) {
    if(vec1[i]==match1) {
      chck = true;
      break;
    }
  }
  return chck;
}

int which_string_double(const std::vector<std::string>& vec1,
                        const std::string& match1,
                        const std::vector<std::string>& vec2, const std::string& match2) {
  int counter = 0;
  int n = vec1.size();
  for( int i = 0;  i < n; i += 1 ) {
    if(vec1[i]==match1 && vec2[i]==match2) break;
    else counter++;
  }
  return counter;
}

int which_string_double(const std::vector<std::string>& vec1,
                        const std::string& match1, const std::vector<int>& vec2, const int& match2) {
  int counter = 0;
  int n = vec1.size();
  for( int i = 0;  i < n; i += 1 ) {
    if(vec1[i]==match1 && vec2[i]==match2) break;
    else counter++;
  }
  return counter;
}

/* test code in r
 ComputeEnvConcentrations(
 pts_ID = pts$ID,
 pts_ID_nxt = pts$ID_nxt,
 pts_basin_id = pts$basin_id,
 pts_upcount = pts$upcount,
 pts_lake_out = pts$lake_out,
 pts_Hylak_id = pts$Hylak_id,
 pts_E_w = pts$E_w,
 pts_E_up = pts$E_up,
 pts_Q = pts$Q,
 pts_E_w_NXT = pts$E_w_NXT,
 pts_k_NXT = pts$k_NXT,
 pts_k_ws = pts$k_ws,
 pts_k_sw = pts$k_sw,
 pts_H_sed = pts$H_sed,
 pts_H = pts$H,
 pts_poros = pts$poros,
 pts_rho_sd = pts$rho_sd,
 pts_dist_nxt = pts$dist_nxt,
 pts_V_NXT = pts$V_NXT,
 pts_f_rem_WWTP = pts$f_rem_WWTP,
 hl_Vol_total = hl$Vol_total,
 hl_k = hl$k,
 hl_k_ws = hl$k_ws,
 hl_Depth_avg = hl$Depth_avg,
 hl_H_sed = hl$H_sed,
 hl_poros = hl$poros,
 hl_rho_sd = hl$rho_sd,
 hl_Hylak_id = hl$Hylak_id,
 hl_E_in = hl$E_in,
 hl_k_sw = hl$k_sw,
 hl_basin_id = hl$basin_id,
 print
 )
 */

// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::export]]
List Compute_env_concentrations_v4_cpp(
    const std::vector<std::string> &pts_ID,
    const std::vector<std::string> &pts_ID_nxt,
    const std::vector<int> &pts_basin_id,
    const std::vector<int> &pts_upcount,
    const std::vector<int> &pts_lake_out,
    const std::vector<int> &pts_Hylak_id,
    const std::vector<double> &pts_E_w,
    const std::vector<double> &pts_E_up,
    const std::vector<double> &pts_Q,
    const std::vector<double> &pts_E_w_NXT,
    const std::vector<double> &pts_k_NXT,
    const std::vector<double> &pts_k_ws,
    const std::vector<double> &pts_k_sw,
    const std::vector<double> &pts_H_sed,
    const std::vector<double> &pts_H,
    const std::vector<double> &pts_poros,
    const std::vector<double> &pts_rho_sd,
    const std::vector<double> &pts_dist_nxt,
    const std::vector<double> &pts_V_NXT,
    const std::vector<double> &pts_f_rem_WWTP,
    const std::vector<double> &pts_x,
    const std::vector<double> &pts_y,
    const std::vector<std::string> &pts_Pt_type,
    const std::vector<double> &hl_Vol_total,
    const std::vector<double> &hl_k,
    const std::vector<double> &hl_k_ws,
    const std::vector<double> &hl_Depth_avg,
    const std::vector<double> &hl_H_sed,
    const std::vector<double> &hl_poros,
    const std::vector<double> &hl_rho_sd,
    const std::vector<int> &hl_Hylak_id,
    const std::vector<double> &hl_E_in,
    const std::vector<double> &hl_k_sw,
    const std::vector<int>  hl_basin_id,
    bool print
) {

  // variables
  int nrow_pts;
  int nrow_hl;
  std::vector<int> pts_fin;
  std::vector<int> hl_fin;
  std::vector<double> pts_C_w;
  std::vector<double> pts_C_sd;
  std::vector<double> hl_C_w;
  std::vector<double> hl_C_sd;
  std::vector<double> pts_E_w_NXT_tmp;
  std::vector<double> pts_E_up_tmp;
  std::vector<double> pts_upcount_tmp;
  std::vector<int> break_vec1;

  int pts_not_finished;
  int hl_not_finished;
  double E_total;
  double H_ratio;
  double chem_exchange;
  double dens_transform;
  double V;
  double k;
  int match_index;
  int match_index_d;

  // clear vectors
  //   pts_fin.clear();
  //   hl_fin.clear();
  //   pts_C_w.clear();
  //   pts_C_sd.clear();
  //   hl_C_w.clear();
  //   hl_C_sd.clear();
  //   pts_E_w_NXT_tmp.clear();
  //   pts_E_up_tmp.clear();
  //   pts_upcount_tmp.clear();

  // get number river nodes and lakes
  nrow_pts = pts_ID.size();
  nrow_hl = hl_Hylak_id.size();

  // create accounting/tmp vectors pts
  //   pts_fin.reserve(nrow_pts);
  //   pts_C_w.reserve(nrow_pts);
  //   pts_C_sd.reserve(nrow_pts);
  //   pts_E_w_NXT_tmp.reserve(nrow_pts);
  //   pts_E_up_tmp.reserve(nrow_pts);
  //   pts_upcount_tmp.reserve(nrow_pts);
  for( int i = 0;  i < nrow_pts; i += 1 ){
    pts_fin.push_back(0);
    pts_C_w.push_back(0.0);
    pts_C_sd.push_back(0.0);
    pts_E_w_NXT_tmp.push_back(pts_E_w_NXT[i]);
    pts_E_up_tmp.push_back(pts_E_up[i]);
    pts_upcount_tmp.push_back(pts_upcount[i]);
  }

  // create accounting/tmp vectors hl
  hl_fin.reserve(nrow_pts);
  hl_C_w.reserve(nrow_hl);
  hl_C_sd.reserve(nrow_hl);
  for( int i = 0;  i < nrow_hl; i += 1 ){
    hl_fin.push_back(0);
    hl_C_w.push_back(0.0);
    hl_C_sd.push_back(0.0);
  }

  // n points and lakes finished?
  pts_not_finished = count_not_finished(pts_fin);
  hl_not_finished = count_not_finished(hl_fin);

  // process until all points are finished
  while(pts_not_finished!=0) {

    // check how many pts and HL are finished
    pts_not_finished = count_not_finished(pts_fin);
    hl_not_finished = count_not_finished(hl_fin);

    // print progress
    std::cout << "# points in pts: " << pts_not_finished << std::endl;
    std::cout << "# points in HL: " << hl_not_finished << std::endl;

    // break when no points are able to be labeled finished
    break_vec1.push_back(pts_not_finished);
    if(break_vec1.size()>10){
      if(break_vec1.end()[-1]==break_vec1.end()[-10]) break;
    }

    for( int j = 0;  j < nrow_pts; j += 1 ) {

      E_total = 0.0;
      H_ratio = 0.0;
      chem_exchange = 0.0;
      dens_transform = 0.0;
      V = 0.0;
      k = 0.0;
      match_index = 0;
      match_index_d = 0;

      if(pts_fin[j]==0 && pts_upcount_tmp[j]==0) {

        if (pts_lake_out[j] == 1) {

          // Concentrations in lake (ug/L)
          match_index = which(hl_Hylak_id,pts_Hylak_id[j]);
          E_total = hl_E_in[match_index] + pts_E_w[j] + pts_E_up_tmp[j]; // total emission into lake (kg/yr)

          V = hl_Vol_total[match_index] * 1e6;  // volume (m3)
          k = hl_k[match_index];  //k (1/sec)
          pts_C_w[j] = E_total / (pts_Q[j] + k * V) * 1e6 / (365*24*3600); //mg/m3 = ug/L

          chem_exchange = hl_k_ws[match_index] / hl_k_sw[match_index];
          H_ratio = hl_Depth_avg[match_index] / hl_H_sed[match_index];
          dens_transform = hl_poros[match_index] + (1 - hl_poros[match_index]) * hl_rho_sd[match_index];

          pts_C_sd[j] = pts_C_w[j] * chem_exchange * H_ratio * dens_transform; // ug/kg

          // Assign concentrations to outflow point and lake
          hl_C_w[match_index] = pts_C_w[j];
          hl_C_sd[match_index] = pts_C_sd[j];

          // Mark lake as done
          hl_fin[match_index] = 1;

          // Contribution to next point downstream
          pts_E_w_NXT_tmp[j] = pts_C_w[j] * pts_Q[j] * 365 * 24 * 3600 / 1e6 *
            std::exp(-pts_k_NXT[j] * pts_dist_nxt[j] / pts_V_NXT[j]); //kg/yr
          match_index_d = which_string_double(pts_ID,pts_ID_nxt[j],pts_basin_id,pts_basin_id[j]);
          pts_E_up_tmp[match_index_d] = pts_E_up_tmp[match_index_d] + pts_E_w_NXT_tmp[j];
          pts_upcount_tmp[match_index_d] = pts_upcount_tmp[match_index_d] - 1;

        }else if(pts_Hylak_id[j] == 0 || pts_lake_out[j] == 1){ //  || pts_lake_out[j] == 1

          // Concentrations at nodes (ug/L)
          E_total = pts_E_w[j] + pts_E_up_tmp[j]; // total mass flowing into node (kg/yr)
          pts_C_w[j] = E_total / pts_Q[j] * 1e6 / (365*24*3600);  // mg/m3 = ug/L

          chem_exchange = pts_k_ws[j] / pts_k_sw[j];
          H_ratio = pts_H[j] / pts_H_sed[j];
          dens_transform = pts_poros[j] + (1 - pts_poros[j]) * pts_rho_sd[j];

          pts_C_sd[j] = pts_C_w[j] * chem_exchange * H_ratio * dens_transform; // ug/kg

          // Contribution to next point downstream
          pts_E_w_NXT_tmp[j] = E_total * std::exp(-pts_k_NXT[j] * pts_dist_nxt[j] / pts_V_NXT[j]); // kg/yr
          match_index_d = which_string_double(pts_ID,pts_ID_nxt[j],pts_basin_id,pts_basin_id[j]);
          pts_E_up_tmp[match_index_d] = pts_E_up_tmp[match_index_d] + pts_E_w_NXT_tmp[j];
          pts_upcount_tmp[match_index_d] = pts_upcount_tmp[match_index_d] - 1;

        }else{

          E_total = pts_E_w[j] + pts_E_up_tmp[j]; //total mass flowing into lake node (kg/yr)
          pts_C_w[j] =  std::numeric_limits<double>::quiet_NaN();//-99.999;
          pts_C_sd[j] =  std::numeric_limits<double>::quiet_NaN();//-99.999;

          // Contribution to next point downstream
          pts_E_w_NXT_tmp[j] = E_total;
          match_index_d = which_string_double(pts_ID,pts_ID_nxt[j],pts_basin_id,pts_basin_id[j]);
          pts_E_up_tmp[match_index_d] = pts_E_up_tmp[match_index_d] + pts_E_w_NXT_tmp[j];
          pts_upcount_tmp[match_index_d] = pts_upcount_tmp[match_index_d] - 1;


        }

        // Mark this point as done
        pts_fin[j] = 1;


      }

    }
  }
  // std::cout << "finished" << std::endl;

  //   // clear vectors
  //   pts_fin.clear();
  //   hl_fin.clear();
  //   //pts_C_w.clear();
  //   pts_C_sd.clear();
  //   hl_C_w.clear();
  //   hl_C_sd.clear();
  //   pts_E_w_NXT_tmp.clear();
  //   pts_E_up_tmp.clear();
  //   pts_upcount_tmp.clear();

  // nrow_HL
  int nrow_HL = hl_Hylak_id.size();

  // return data
  List OutList;
  DataFrame pts_out = DataFrame::create( _["ID"] = pts_ID,
                                         _["ID_nxt"] = pts_ID_nxt,
                                         _["basin_ID"] = pts_basin_id,
                                         _["Pt_type"] = pts_Pt_type,
                                         _["Hylak_id"] = pts_Hylak_id,
                                         _["x"] = pts_x,
                                         _["y"] = pts_y,
                                         _["Q"] = pts_Q,
                                         _["C_w"] = pts_C_w,
                                         _["C_sd"] = pts_C_sd,
                                         _["WWTPremoval"] = pts_f_rem_WWTP );
  if(nrow_HL!=0) {

    DataFrame HL_out = DataFrame::create( _["Hylak_id"] = hl_Hylak_id,
                                          _["C_w"] = hl_C_w,
                                          _["C_sd"] = hl_C_sd );

    OutList = List::create( _["pts"] = pts_out, _["HL"] = HL_out);

  } else {

    OutList = List::create( _["pts"] = pts_out);

  }
  return OutList;
//
//   // return OutList;
//   return pts_C_w;
}
