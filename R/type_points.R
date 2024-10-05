type_points = function() {
  out <- list(
    draw = draw_points,
    data = data_points,
    name = "p"
  )
  class(out) <- "tinyplot_type"
  return(out)
}


data_points = function(datapoints, ...) {
    return(list())
}


draw_points <- function(ix, iy, icol, ibg, ipch, ilwd, cex, ...) {
  points(
    x = ix,
    y = iy,
    col = icol,
    bg = ibg,
    type = "p",
    pch = ipch,
    lwd = ilwd,
    cex = cex
  )
}

