#' Rectangle plot type
#'
#' @description Type function for plotting rectangles.
#' 
#' @details Contrary to base \code{\link[graphics]{rect}}, rectangles in
#' [tinyplot] must be specified using the `xmin`, `ymin`,`xmax`, and `ymax`
#' arguments. 
#' 
#' @examples
#' i = 4*(0:10)
#' 
#' # "rect" type convenience character string
#' tinyplot(
#'   xmin = 100+i, ymin = 300+i, xmax = 150+i, ymax = 380+i,
#'   by = i, fill = 0.2,
#'   type = "rect"
#' )
#' 
#' # Same result with type_rect()
#' tinyplot(
#'   xmin = 100+i, ymin = 300+i, xmax = 150+i, ymax = 380+i,
#'   by = i, fill = 0.2,
#'   type = type_rect()
#' )
#' 
#' @export
type_rect = function() {
  out = list(
    draw = draw_rect(),
    data = data_rect(),
    name = "rect"
  )
  class(out) = "tinyplot_type"
  return(out)
}


data_rect = function() {
  fun = function(settings, ...) {
    # legend customizations
    settings$legend_args[["pch"]] = settings$legend_args[["pch"]] %||% 22
    settings$legend_args[["pt.cex"]] = settings$legend_args[["pt.cex"]] %||% 3.5
    settings$legend_args[["pt.lwd"]] = settings$legend_args[["pt.lwd"]] %||% par("lwd")
    settings$legend_args[["lty"]] = settings$legend_args[["lty"]] %||% 0
    settings$legend_args[["y.intersp"]] = settings$legend_args[["y.intersp"]] %||% 1.25
    settings$legend_args[["seg.len"]] = settings$legend_args[["seg.len"]] %||% 1.25
  }
  return(fun)
}


draw_rect = function() {
    fun = function(ixmin, iymin, ixmax, iymax, ilty, ilwd, icol, ibg, ...) {
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
