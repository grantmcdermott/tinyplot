type_lines = function(type = "l") {
  out = list(
    draw = draw_lines(type = type),
    data = NULL,
    name = type
  )
  class(out) = "tinyplot_type"
  return(out)
}


draw_lines = function(type = "l") {
    fun = function(ix, iy, icol, ipch, ilty, ilwd, ...) {
        lines(
            x = ix,
            y = iy,
            col = icol,
            type = type,
            pch = ipch,
            lty = ilty,
            lwd = ilwd
        )
    }
    return(fun)
}
