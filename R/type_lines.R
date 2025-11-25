#' Lines plot type
#'
#' @description Type function for plotting lines.
#' 
#' @inheritParams graphics::plot.default
#' @inheritParams dodge_positions
#' 
#' @examples
#' # "l" type convenience character string
#' tinyplot(circumference ~ age | Tree, data = Orange, type = "l")
#' 
#' # Use `type_lines()` to pass extra arguments for customization
#' tinyplot(circumference ~ age | Tree, data = Orange, type = type_lines(type = "s"))
#' 
#' @export
type_lines = function(type = "l", dodge = 0, fixed.dodge = FALSE) {
  out = list(
    draw = draw_lines(type = type),
    data = data_lines(dodge = dodge, fixed.dodge = fixed.dodge),
    name = type
  )
  class(out) = "tinyplot_type"
  return(out)
}


data_lines = function(dodge = 0, fixed.dodge = FALSE) {
  if (is.null(dodge) || dodge == 0) return(NULL)
  fun = function(settings, ...) {
    env2env(settings, environment(), c("datapoints", "xlabs"))

    if (is.character(datapoints$x)) {
      datapoints$x = as.factor(datapoints$x)
    }
    if (is.factor(datapoints$x)) {
      xlvls = unique(datapoints$x)
      datapoints$x = factor(datapoints$x, levels = xlvls)
      xlabs = seq_along(xlvls)
      names(xlabs) = xlvls
      datapoints$x = as.integer(datapoints$x)
    }

    # dodge
    if (dodge != 0) {
      datapoints = dodge_positions(datapoints, dodge, fixed.dodge)
    }

    x = datapoints$x
    env2env(environment(), settings, c(
      "x",
      "xlabs",
      "datapoints"
    ))
  }
  fun
}


draw_lines = function(type = "l") {
    fun = function(ix, iy, icol, ipch, ibg, ilty, ilwd, icex = 1, ...) {
        lines(
            x = ix,
            y = iy,
            col = icol,
            type = type,
            pch = ipch,
            bg = ibg,
            lty = ilty,
            lwd = ilwd,
            cex = icex
        )
    }
    return(fun)
}
