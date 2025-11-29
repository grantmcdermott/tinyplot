#' Polygon plot type
#'
#' @description Type function for plotting polygons.
#' Arguments are passed to \code{\link[graphics]{polygon}}.
#' 
#' @inheritParams graphics::polygon
#' 
#' @examples
#' # "polygon" type convenience character string
#' tinyplot(1:9, c(2,1,2,1,NA,2,1,2,1), type = "polygon")
#' 
#' # Use `type_polygon()` to pass extra arguments for customization
#' tinyplot(1:9, c(2,1,2,1,NA,2,1,2,1), type = type_polygon(density = c(10, 20)))
#' 
#' @export
type_polygon = function(density = NULL, angle = 45) {
  out = list(
    draw = draw_polygon(density = density, angle = angle),
    data = data_polygon(),
    name = "polygon"
  )
  class(out) = "tinyplot_type"
  return(out)
}


data_polygon = function() {
  fun = function(settings, ...) {
    settings$legend_args[["pch"]] = settings$legend_args[["pch"]] %||% 22
    settings$legend_args[["pt.cex"]] = settings$legend_args[["pt.cex"]] %||% 3.5
    settings$legend_args[["y.intersp"]] = settings$legend_args[["y.intersp"]] %||% 1.25
    settings$legend_args[["seg.len"]] = settings$legend_args[["seg.len"]] %||% 1.25
  }
  return(fun)
}


draw_polygon = function(density = density, angle = 45) {
    fun = function(ix, iy, icol, ibg, ilty = par("lty"), ilwd = par("lwd"), ...) {
        polygon(
            x = ix,
            y = iy,
            border = icol,
            col = ibg,
            lty = ilty,
            lwd = ilwd,
            density = density,
            angle = angle
        )
    }
    return(fun)
}
