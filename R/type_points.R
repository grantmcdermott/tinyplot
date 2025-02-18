#' Points plot type
#'
#' @description Type function for plotting points, i.e. a scatter plot.
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
type_points = function() {
  out = list(
    draw = draw_points(),
    data = data_points(),
    name = "p"
  )
  class(out) = "tinyplot_type"
  return(out)
}

data_points = function() {
  fun = function(datapoints, ...) {
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
    
    out = list(
      datapoints = datapoints,
      xlabs = xlabs,
      ylabs = ylabs
    )
    return(out)
  }
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

