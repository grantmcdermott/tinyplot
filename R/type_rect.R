type_rect = function() {
  out <- list(
    draw = draw_rect(),
    data = NULL,
    name = "rect"
  )
  class(out) <- "tinyplot_type"
  return(out)
}


draw_rect <- function() {
    fun <- function(ixmin, iymin, ixmax, iymax, ilty, ilwd, icol, ibg, ...) {
        rect(
            xleft = ixmin, ybottom = iymin, xright = ixmax, ytop = iymax,
            lty = ilty,
            lwd = ilwd,
            border = icol,
            col = ibg
        )
    }
    return(fun)
}
