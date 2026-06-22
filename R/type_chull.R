#' Convex hull plot type
#'
#' @description Type function for drawing convex hulls around grouped points.
#'   Uses \code{\link[grDevices]{chull}} to compute the convex hull of each
#'   group and draws the result as a polygon.
#'
#' @inheritParams type_polygon
#'
#' @examples
#' # "chull" type convenience character string
#' tinyplot(Sepal.Length ~ Petal.Length | Species, data = iris, type = "chull")
#'
#' # layer filled convex hull(s) on top of points
#' tinyplot(Sepal.Length ~ Petal.Length | Species, data = iris, theme = "basic")
#' tinyplot_add(type = "chull", fill = 0.2)
#'
#' @export
type_chull = function(density = NULL, angle = 45) {
  out = list(
    draw = draw_polygon(density = density, angle = angle),
    data = data_chull(),
    name = "chull"
  )
  class(out) = "tinyplot_type"
  return(out)
}


data_chull = function() {
  fun = function(settings, ...) {
    env2env(settings, environment(), c("datapoints", "by", "facet"))

    datapoints = split(datapoints, list(datapoints$by, datapoints$facet))
    datapoints = Filter(function(k) nrow(k) > 0, datapoints)

    datapoints = lapply(datapoints, function(dat) {
      idx = grDevices::chull(dat$x, dat$y)
      idx = c(idx, idx[1L])
      data.frame(
        x = dat$x[idx],
        y = dat$y[idx],
        by = dat$by[1L],
        facet = dat$facet[1L]
      )
    })
    datapoints = do.call(rbind, datapoints)

    by = if (length(unique(datapoints$by)) == 1) by else datapoints$by
    facet = if (length(unique(datapoints$facet)) == 1) facet else datapoints$facet

    # legend customizations
    settings$legend_args[["pch"]] = settings$legend_args[["pch"]] %||% 22
    settings$legend_args[["pt.cex"]] = settings$legend_args[["pt.cex"]] %||% 3.5
    settings$legend_args[["y.intersp"]] = settings$legend_args[["y.intersp"]] %||% 1.25
    settings$legend_args[["seg.len"]] = settings$legend_args[["seg.len"]] %||% 1.25

    env2env(environment(), settings, c("datapoints", "by", "facet"))
  }
  return(fun)
}
