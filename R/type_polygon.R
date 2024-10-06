type_polygon = function() {
  out = list(
    draw = draw_polygon(),
    data = NULL,
    name = "polygon"
  )
  class(out) = "tinyplot_type"
  return(out)
}


draw_polygon = function() {
    fun = function(ix, iy, icol, ibg, ilty = par("lty"), ilwd = par("lwd"), ...) {
        polygon(
            x = ix,
            y = iy,
            border = icol,
            col = ibg,
            lty = ilty,
            lwd = ilwd
        )
    }
    return(fun)
}
