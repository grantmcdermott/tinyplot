#' Points plot type
#'
#' @description Type function for plotting points, i.e. a scatter plot.
#' @param clim Numeric giving the lower and upper limits of the character
#'   expansion (`cex`) normalization for bubble charts.
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
type_points = function(clim = c(0.5, 2.5)) {
  out = list(
    data = data_points(clim = clim),
    draw = draw_points(),
    name = "p"
  )
  class(out) = "tinyplot_type"
  return(out)
}

data_points = function(clim = c(0.5, 2.5)) {
  fun = function(settings, cex = NULL, ...) {
    list2env(settings[c("datapoints", "cex", "legend_args")], environment())

    # catch for factors (we should still be able to "force" plot these with points)
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

    bubble = FALSE
    bubble_cex = 1
    if (!is.null(cex) && length(cex) == nrow(datapoints)) {
      bubble = TRUE
      ## Identify the pretty break points for our bubble labels
      bubble_labs = pretty(cex, n = 5)
      len_labs = length(bubble_labs)
      cex = rescale_num(sqrt(c(bubble_labs, cex)) / pi, to = clim)
      bubble_cex = cex[1:len_labs]
      cex = cex[(len_labs+1):length(cex)]
      # catch for cases where pretty breaks leads to smallest category of 0
      if (bubble_labs[1] == 0) {
        bubble_labs = bubble_labs[-1]
        bubble_cex = bubble_cex[-1]
      }
      names(bubble_cex) = format(bubble_labs)
      if (max(clim) > 2.5) {
        legend_args[["x.intersp"]] = max(clim) / 2.5
        legend_args[["y.intersp"]] = sapply(bubble_cex / 2.5, max, 1)
      }
    }

    update_settings(settings,
      datapoints = datapoints,
      xlabs = xlabs,
      ylabs = ylabs,
      cex = cex,
      bubble = bubble,
      bubble_cex = bubble_cex,
      legend_args = legend_args
    )
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
