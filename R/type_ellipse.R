#' Confidence ellipse plot type
#'
#' @description Type function for drawing confidence ellipses around grouped
#'   points.
#'
#' @details
#' A confidence ellipse summarizes the spread and correlation of two variables:
#' its axes align with the principal directions of variation, their lengths
#' reflect the standard deviations along those directions, and the overall size
#' is scaled so that (assuming bivariate normality) the specified proportion of
#' observations falls within the boundary.
#'
#' Operationally, the ellipse is computed from the covariance matrix
#' (\code{\link[stats]{cov}}) of each group via eigendecomposition
#' (\code{\link[base]{eigen}}), scaled by a chi-squared quantile
#' (\code{\link[stats]{qchisq}}) for the specified confidence level.
#' Groups with fewer than 3 points are silently dropped.
#'
#' @importFrom stats cov qchisq
#'
#' @param level Numeric confidence level for the ellipse. Default is `0.95`.
#' @param segments Integer number of line segments used to approximate the
#'   ellipse. Default is `100`.
#' @inheritParams type_polygon
#'
#' @examples
#' # "ellipse" type convenience character string
#' tinyplot(Sepal.Length ~ Petal.Length | Species, data = iris, type = "ellipse")
#'
#' # layer filled ellipse(s) on top of points
#' tinyplot(Sepal.Length ~ Petal.Length | Species, data = iris, theme = "basic")
#' tinyplot_add(type = "ellipse", fill = 0.2)
#' 
#' # to avoid clipping, it's better to reverse the layering (ellipse first)
#' tinyplot(Sepal.Length ~ Petal.Length | Species, data = iris, theme = "basic",
#'          type = "ellipse", fill = 0.2)
#' tinyplot_add(type = "p")
#'
#' # custom confidence level
#' tinyplot(Sepal.Length ~ Petal.Length | Species, data = iris,
#'   type = type_ellipse(level = 0.5))
#'
#' @export
type_ellipse = function(level = 0.95, segments = 100, density = NULL, angle = 45) {
  out = list(
    draw = draw_polygon(density = density, angle = angle),
    data = data_ellipse(level = level, segments = segments),
    name = "ellipse"
  )
  class(out) = "tinyplot_type"
  return(out)
}


data_ellipse = function(level = 0.95, segments = 100) {
  fun = function(settings, ...) {
    env2env(settings, environment(), c("datapoints", "by", "facet"))

    datapoints = split(datapoints, list(datapoints$by, datapoints$facet))
    datapoints = Filter(function(k) nrow(k) >= 3, datapoints)

    radius = sqrt(qchisq(level, df = 2))
    theta = seq(0, 2 * pi, length.out = segments + 1)
    unit = cbind(cos(theta), sin(theta))

    datapoints = lapply(datapoints, function(dat) {
      center = c(mean(dat$x), mean(dat$y))
      cov_mat = cov(cbind(dat$x, dat$y))
      eig = eigen(cov_mat, symmetric = TRUE)
      scale_mat = eig$vectors %*% diag(sqrt(eig$values) * radius)
      pts = unit %*% t(scale_mat)
      data.frame(
        x = pts[, 1] + center[1],
        y = pts[, 2] + center[2],
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
