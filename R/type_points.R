#' Points plot type
#'
#' @description Type function for plotting points, i.e. a scatter plot.
#' 
#' @examples
#' # "p" type convenience character string
#' tinyplot(dist ~ speed, data = cars, type = "p")
#' 
#' # Same result with type_points()
#' tinyplot(dist ~ speed, data = cars, type = type_points())
#' 
#' # Note: Specifying the type here is redundant. Like base plot, tinyplot
#' # automatically produces a scatter plot if x and y are numeric
#' tinyplot(dist ~ speed, data = cars)
#' 
#' @export
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

