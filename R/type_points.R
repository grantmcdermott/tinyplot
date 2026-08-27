#' Points plot type
#'
#' @description Type function for plotting points, i.e. a scatter plot.
#' @param clim Numeric giving the lower and upper limits of the character
#'   expansion (`cex`) normalization for bubble charts.
#' @param xlevels,xord arguments controlling the order of the (categorical) `x`
#'   variable, and hence of the x-axis. Supply one or the other; if both
#'   arguments are provided, `xlevels` takes precedence and `xord` is silently
#'   ignored.
#'
#'   - `xlevels` specifies the levels _literally_, either a character vector of
#'   level names in the desired order (e.g., `c("C", "B", "A")`), or a numeric
#'   vector of the corresponding level indexes (e.g. `3:1`).
#'
#'   - `xord` instead accepts a keyword or custom function, which then _derives_
#'   the order from the data. Options are:
#'
#'     - `"total"` ranks the categories by the `y` values observed at each one,
#'     largest first.
#'     - `"minvar"` ranks them by the variance of those values, lowest first.
#'     - `"asis"` or `"rev"` permute the existing levels without consulting the
#'     data at all. The former takes the categories in the order that they
#'     appear in the data, while the latter reverses the current level order.
#'     - a custom function that determines both the ranking statistic and its
#'     direction. The statistic is always sorted ascending, so
#'     `function(y) sum(y)` reverses `"total"`, and `function(y) -median(y)`
#'     ranks by median rather than by sum.
#'
#'   Note that `x` is only reordered when it is categorical (i.e., factor or
#'   character). A numeric `x` is plotted at its own values and cannot be
#'   reordered, so supplying either argument there is ignored with a warning.
#'   Each argument defaults to `NULL`, i.e. keep the existing factor levels.
#' @inheritParams dodge_positions
#'
#' @examples
#' # "p" type convenience character string
#' tinyplot(Sepal.Length ~ Petal.Length, data = iris, type = "p")
#'
#' # Same result with type_points()
#' tinyplot(Sepal.Length ~ Petal.Length, data = iris, type = type_points())
#'
#' # Note: Specifying the type here is redundant. Like base plot, tinyplot
#' # automatically produces a scatter plot if x and y are numeric
#' tinyplot(Sepal.Length ~ Petal.Length, data = iris)
#'
#' # Grouped scatter plot example
#' tinyplot(Sepal.Length ~ Petal.Length | Species, data = iris)
#'
#' # Continuous grouping (with gradient legend)
#' tinyplot(Sepal.Length ~ Petal.Length | Sepal.Width, data = iris, pch = 19)
#'
#' # Bubble chart version
#' tinyplot(Sepal.Length ~ Petal.Length, data = iris, cex = iris$Sepal.Width)
#'
#' # Fancier version with dual legends and extra customization
#' tinyplot(Sepal.Length ~ Petal.Length | Species,
#'   data = iris,
#'   cex = iris$Sepal.Width, clim = c(1, 5),
#'   pch = 21, fill = 0.3)
#'
#' @export
type_points = function(clim = c(0.5, 2.5), dodge = 0, fixed.dodge = FALSE, xlevels = NULL, xord = NULL) {
  out = list(
    data = data_points(clim = clim, dodge = dodge, fixed.dodge = fixed.dodge, xlevels = xlevels, xord = xord),
    draw = draw_points(),
    name = "p"
  )
  class(out) = "tinyplot_type"
  return(out)
}

data_points = function(clim = c(0.5, 2.5), dodge = 0, fixed.dodge = FALSE, xlevels = NULL, xord = NULL) {
  fun = function(settings, ...) {
    env2env(settings, environment(), "datapoints")

    # Store clim for bubble() function
    settings$clim = clim

    # catch for factors (we should still be able to "force" plot these with points)
    datapoints$x = sanitize_xlevels(datapoints$x, xlevels)
    warn_ignored_ordering(datapoints$x, xlevels, xord)
    # `xord` must run here, before the factor is collapsed to integer
    # positions below -- once x is an integer there are no levels left to
    # reorder.
    if (!is.null(xord) && is.null(xlevels)) {
      datapoints$x = sanitize_ord(
        datapoints$x, datapoints[["y"]], NULL,
        xord, arg = "xord", keywords = ord_keywords_distribution
      )
    }
    if (is.factor(datapoints$x)) {
      xlvls = levels(datapoints$x)
      xlabs = seq_along(xlvls)
      names(xlabs) = xlvls
      datapoints$x = as.integer(datapoints$x)
    } else {
      xlabs = NULL
    }
    if (is.factor(datapoints$y)) {
      ylvls = levels(datapoints$y)
      ylabs = seq_along(ylvls)
      names(ylabs) = ylvls
      datapoints$y = as.integer(datapoints$y)
    } else {
      ylabs = NULL
    }

    # dodge
    if (dodge != 0) {
      datapoints = dodge_positions(datapoints, dodge, fixed.dodge)
    }

    # legend customizations
    settings$legend_args[["pt.lwd"]] = settings$legend_args[["pt.lwd"]] %||% settings$lwd

    env2env(environment(), settings, c(
      "datapoints",
      "xlabs",
      "ylabs"
    ))
  }
}

draw_points = function() {
  fun = function(ix, iy, icol, ibg, ipch, ilwd, icex, ...) {
    # browser()
    points(
      x = ix,
      y = iy,
      col = icol,
      bg = ibg,
      type = "p",
      pch = ipch,
      lwd = ilwd,
      cex = icex
    )
  }
  return(fun)
}
