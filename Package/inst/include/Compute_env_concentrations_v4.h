#ifndef COMPUTEENVCONCENTRATIONS_H
#define COMPUTEENVCONCENTRATIONS_H

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
// #include <Rcpp.h>
// using namespace Rcpp;



//' Does ComputeEnvConcentrations
//' @param vec1 vector something
//' @param vec2 vector something
//' @param vec3 vector something
//' @param vec4 vector something
//' @param vec5 vector something
// [[Rcpp::export]]
// [[Rcpp::plugins("cpp11")]]
std::vector<double> Compute_env_concentrations_v4_cpp(
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
    const std::vector<int>  hl_basin_id
);

#endif // COMPUTEENVCONCENTRATIONS_H
