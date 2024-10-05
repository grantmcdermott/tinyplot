type_segments = function() {
  out <- list(
    draw = draw_segments(),
    data = NULL,
    name = "segments"
  )
  class(out) <- "tinyplot_type"
  return(out)
}


draw_segments <- function() {
    fun <- function(ixmin, iymin, ixmax, iymax, ilty, ilwd, icol, ...) {
        segments(
            x0 = ixmin, y0 = iymin, x1 = ixmax, y1 = iymax,
            lty = ilty,
            lwd = ilwd,
            col = icol
        )
    }
    return(fun)
}
