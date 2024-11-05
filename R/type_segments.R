#' Line segments plot type
#'
#' @description Type function for plotting line segments.
#' 
#' @details Contrary to base \code{\link[graphics]{segments}}, line segments in
#' [tinyplot] must be specified using the `xmin`, `ymin`,`xmax`, and `ymax`
#' arguments. 
#' 
#' @examples
#' # "segments" type convenience character string
#' tinyplot(
#'   xmin = c(0,.1), ymin = c(.2,1), xmax = c(1,.9), ymax = c(.75,0),
#'   type = "segments"
#' )
#' 
#' # Same result with type_segments()
#' tinyplot(
#'   xmin = c(0,.1), ymin = c(.2,1), xmax = c(1,.9), ymax = c(.75,0),
#'   type = type_segments()
#' )
#' 
#' @export
type_segments = function() {
  out = list(
    draw = draw_segments(),
    data = NULL,
    name = "segments"
  )
  class(out) = "tinyplot_type"
  return(out)
}


draw_segments = function() {
    fun = function(ixmin, iymin, ixmax, iymax, ilty, ilwd, icol, ...) {
        segments(
            x0 = ixmin, y0 = iymin, x1 = ixmax, y1 = iymax,
            lty = ilty,
            lwd = ilwd,
            col = icol
        )
    }
    return(fun)
}
