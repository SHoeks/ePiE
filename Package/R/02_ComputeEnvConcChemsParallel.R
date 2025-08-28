ComputeEnvConcChemsParallel = function(basin_data,chem,cons,n_workers=4){

  # check future package
  if(any(grepl("^future$",.packages(all.available = TRUE)))){
    suppressMessages(require("future"))
  }else{
    stop("future package not intalled, unable to run in parallel")
  }

  if(availableCores()<n_workers){
    n_workers = availableCores()
    cat("number of cores set to",n_workers,"because machine doens't have more core available\n")
  }

  # print status
  cat("using",n_workers,"workers to compute concentrations for",nrow(chem),"chemicals\n")

  # output lists
  out_run = list()
  out_val = list()

  # initiate workers
  if(future::supportsMulticore()) {
    future::plan(future::multicore,workers = n_workers)
  } else {
    future::plan(future::multisession,workers = n_workers)
  }

  # split chemicals into batches
  batches = split(1:nrow(chem), ceiling(seq_along(1:nrow(chem))/n_workers))
  for(i in 1:length(batches)){
    if(length(batches[[i]])<6){
      cat("running epie for batch:",i,"chem:",paste0(chem$API[batches[[i]]],collapse=", "),"( n chems:",length(batches[[i]]),")","...\n")
    }else{
      cat("running epie for batch:",i,"chem:",chem$API[batches[[i]]][1],"to",chem$API[batches[[i]]][length(batches[[i]])],"( n chems:",length(batches[[i]]),")","...\n")
    }
    for(w in batches[[i]]){
      out_run[[w]] = future( ComputeEnvConcentrations(basin_data,chem[w,],cons,FALSE), seed=TRUE) %plan% multisession
    }
    for(w in batches[[i]]) {
      out_val[[w]] = value(out_run[[w]])
    }
  }
  future:::ClusterRegistry("stop")
  names(out_val) = chem$API
  return(out_val)
}
