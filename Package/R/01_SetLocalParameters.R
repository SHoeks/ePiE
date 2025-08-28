SetLocalParameters = function(basin_data,chem,chem_ii,cons){
    #out = Set_local_parameters_custom_removal_fast2(pts=basin_data$pts,HL=basin_data$hl,cons=cons,chem=chem,chem_ii=chem_ii,UseCpp=FALSE)
    out = Set_local_parameters_custom_removal_fast3(pts=basin_data$pts,HL=basin_data$hl,cons=cons,chem=chem,chem_ii=chem_ii)
    return(out)
}

