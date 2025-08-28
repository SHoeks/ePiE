# LoadEuropeanBasins = function(){
#   libPath = ePiEPath()
#   out = list()
#   out$pts = fst::read.fst(glue::glue("{libPath}/basin_db/pts_c75.fst"))
#   out$hl = fst::read.fst(glue::glue("{libPath}/basin_db/hl_c75.fst"))
#   return(out)
# }

LoadEuropeanBasins = function(){
  out = list()
  out$pts = ePiE::pts
  out$hl = ePiE::hl
  return(out)
}

ePiEPath = function() {
  lib_name = 'ePiE'
  lib_paths = .libPaths()
  libPath = paste0(lib_paths[grep(lib_name,lapply(lib_paths,list.files))][1],'/',lib_name)
  return(libPath)
}
