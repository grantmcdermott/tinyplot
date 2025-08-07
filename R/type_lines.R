#' Lines plot type
#'
#' @description Type function for plotting lines.
#' 
#' @inheritParams graphics::plot.default
#' 
#' @examples
#' # "l" type convenience character string
#' tinyplot(circumference ~ age | Tree, data = Orange, type = "l")
#' 
#' # Use `type_lines()` to pass extra arguments for customization
#' tinyplot(circumference ~ age | Tree, data = Orange, type = type_lines(type = "s"))
#' 
#' @export
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
    fun = function(ix, iy, icol, ipch, ibg, ilty, ilwd, icex = 1, ...) {
        lines(
            x = ix,
            y = iy,
            col = icol,
            type = type,
            pch = ipch,
            bg = ibg,
            lty = ilty,
            lwd = ilwd,
            cex = icex
        )
    }
    return(fun)
}
