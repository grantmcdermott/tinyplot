type_points = function() {
  out = list(
    draw = draw_points(),
    data = NULL,
    name = "p"
  )
  class(out) = "tinyplot_type"
  return(out)
}


draw_points = function() {
    fun = function(ix, iy, icol, ibg, ipch, ilwd, cex, ...) {
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
    return(fun)
}

