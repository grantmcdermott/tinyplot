#' Error bars type
#'
#' @inheritParams graphics::arrows
#' @export
type_errorbar = function(length = 0.05) {
  out = list(
    draw = draw_errorbar(length = length),
    data = data_pointrange(),
    name = "p"
  )
  class(out) = "tinyplot_type"
  return(out)
}


draw_errorbar = function(length = 0.05) {
    fun = function(ix, iy, ixmin, iymin, ixmax, iymax, icol, ibg, ipch, ilwd, cex, ...) {
        arrows(
            x0 = ixmin,
            y0 = iymin,
            x1 = ixmax,
            y1 = iymax,
            col = icol,
            lwd = ilwd,
            length = length,
            angle = 90,
            code = 3
        )
        draw_points()(ix = ix, iy = iy, icol = icol, ibg = ibg, ipch = ipch, ilwd = ilwd, cex = cex)
    }
    return(fun)
}


