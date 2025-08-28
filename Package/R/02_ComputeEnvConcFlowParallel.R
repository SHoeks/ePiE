ComputeEnvConcFlowParallel = function(basin_data_list,chem,cons){

  if(any(grepl("^future$",.packages(all.available = TRUE)))){
    suppressMessages(require("future"))
  }else{
    stop("future package not intalled, unable to run in parallel")
  }

  if(class(basin_data_list[[1]])!="list" & !all(names(basin_data_list[[1]])==c("pts","hl"))){
    stop("basin_data_list not formatted as a nested list with pts and hl per flow condition")
  }

  out_run = list()
  out_val = list()
  plan(multisession)
  for(i in 1:length(basin_data_list)){
    out_run[[i]] = future( ComputeEnvConcentrations(basin_data_list[[i]],chem,cons,FALSE), seed=TRUE) %plan% multisession
  }
  for(i in 1:length(basin_data_list)){
    out_val[[i]] = value(out_run[[i]])
  }

  names(out_val) = names(basin_data_list)
  return(out_val)
}
