#' Points plot type
#'
#' @description Type function for plotting points, i.e. a scatter plot.
#' @param clim Numeric giving the lower and upper limits of the character
#'   expansion normalization for bubble charts.
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
  fun = function(datapoints, cex = NULL, ...) {
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

    # browser()
    bubble = FALSE
    if (!is.null(cex) && length(cex) == nrow(datapoints)) {
      cex = rescale_num(cex, to = clim) 
      bubble = TRUE
    }
    
    out = list(
      datapoints = datapoints,
      xlabs = xlabs,
      ylabs = ylabs,
      cex = cex,
      bubble = bubble
    )
    return(out)
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

