## Existing functions renamed

-   [x] Add_new_flow_fast() + Select_hydrology_fast2() rename one function called AddFlowToBasin()
-   [x] Rename Check_cons_v2() to CheckConsumption()
-   [x] Rename Compute_env_concentration_cpp_custom_removal() to ComputeEnvConcentrations()

## New functions

-   [x] LoadExampleChemProperties(), this loads the standard chem properties example file for Ibuprofen
-   [x] LoadEuropeanBasins(), this loads a couple of European basins
-   [x] LoadExampleConsumption(), this loads example consumption data
-   [x] CompleteChemProperties() --\> Check_chem_WWTP_removal_data() + Chem_complete()
-   [x] CheckConsumptionData(), runs Check_cons_v2, is able to fill potential gaps in the future
-   [x] SelectBasins(), this selects a basin from the loaded file, it also calls Set_upstream_points_v2()

## Output and plotting functions

-   [ ] ...

## All functions?

``` r
export(Add_new_flow_fast)
export(Adv_treatment)
export(Calculate_stats)
export(Check_chem_WWTP_removal_data)
export(Check_cons_v2)
export(Chem_complete)
export(Compute_env_concentration_cpp_custom_removal)
export(Compute_env_concentrations_v2)
export(Create_map_inc_Lakes)
export(Create_map)
export(Create_multi_maps_scaled_v2)
export(Create_multi_maps_scaled_v3)
export(Create_save_multi_maps)
export(Download_basins)
export(Download_flow)
export(Include_sampling_locations)
export(List_basins)
export(Load_flow_lt)
export(Load_flow)
export(load_lake_polygons)
export(load_river_lines)
export(Make_spatial)
export(Select_basin_pts3)
export(Select_basin_HL)
export(Select_basin_pts3_v2)
export(Select_hydrology_fast2)
export(Set_local_parameters_custom_removal_fast2)
export(Set_upstream_points_v2)
export(SimpleTreat4_0)
export(nearestPointOnSegment)
export(nearestPointOnLine)
export(snapPointsToLines)
export(Set_monit)
export(Set_eqdist)
export(Locate_sampling_locations)
export(unrotate)
export(Write_spatial_file_parallel)
export(Write_spatial_file)
```
