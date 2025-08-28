

# taken from https://github.com/AustralianAntarcticDivision/raadtools/blob/master/R/utils.R#L9
unrotate <- function(x, filename = "", ...){
  e <- extent(x)
  xrange <- e@xmax - e@xmin
  inverse <- FALSE
  if (xrange < 350 | xrange > 370 | e@xmin < -10 | e@xmax > 370) {
    if (xrange < 350 | xrange > 370 | e@xmin < -190 | e@xmax > 190) {
      warning('this does not look like an appropriate object for this function')
    } else {
      inverse <- TRUE
    }
  }
  e <- extent(x)
  xrange <- e@xmax - e@xmin
  hx <- e@xmin + xrange / 2

  r1 <- crop(x, extent(e@xmin, hx, e@ymin, e@ymax))
  r2 <- crop(x, extent(hx, e@xmax, e@ymin, e@ymax))
  if (inverse) {
    r1@extent@xmin <- r2@extent@xmax
    r1@extent@xmax <- r1@extent@xmin + 0.5 * xrange
  } else {
    r2@extent@xmin <- r2@extent@xmin - xrange
    r2@extent@xmax <- r2@extent@xmax - xrange
  }

  out <- merge(r1, r2, overlap=FALSE)
  names(out) <- names(x)
  out@z <- x@z

  if (length(grep("Oceania", names(x))) > 0) {
    out <- raster::crop(out, c(100, 240, extent(out)[3:4]))
    # print("Oceania")
    }
  if (length(grep("Russia", names(x))) > 0) {
    out <- raster::crop(out, c(0, 200, extent(out)[3:4]))
    # print("Russia")
  }

  if (filename != "") {
    out <- writeRaster(out, filename, ...)
  }

  return(out)
}
