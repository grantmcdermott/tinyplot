type_polypath = function() {
  out = list(
    draw = draw_polypath(),
    data = NULL,
    name = "polypath"
  )
  class(out) = "tinyplot_type"
  return(out)
}


draw_polypath = function() {
    fun =  function(ix, iy, icol, ibg, ilty, ilwd, dots, ...) {
        irule = ifelse(!is.null(dots[["rule"]]), dots[["rule"]], "winding")
        polypath(
            x = ix,
            y = iy,
            border = icol,
            col = ibg,
            lty = ilty,
            lwd = ilwd,
            rule = irule
        )
    }
    return(fun)
}
