.onLoad <- function(libname, pkgname) {
  # info <- packageDescription("ePiE")
  # packageStartupMessage("ePiE version: ",info$Version)
}

.onAttach <- function(libname, pkgname) {
  info <- packageDescription("ePiE")
  packageStartupMessage("ePiE version: ",info$Version)
}

ePiEVersion <- function() {
  info <- packageDescription("ePiE")
  message("ePiE version: ",info$Version)
  return(info$Version)
}
