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
#' # Direct legend labels are a good option for grouped lined plots (assuming
#' # there aren't too many groups and the data are sorted along the x-axis)
#' tinyplot(
#'   circumference ~ age | Tree, data = Orange, type = "l",
#'   legend = "direct"
#' )
#' 
#' # Fancier version(s) that use a theme and repel overlapping labels
#' Orange2 = transform(Orange, Tree = paste("Tree", Tree))
#' tinyplot(
#'   circumference ~ age | Tree, data = Orange2, type = "l",
#'   legend = list("direct", repel = TRUE), # auto repel
#'   theme = "socviz"
#' )
#' tinyplot(
#'   circumference ~ age | Tree, data = Orange2, type = "l",
#'   legend = list("direct", nudge_y = c("Tree 1" = 3, "Tree 3" = -5)), # manual
#'   theme = "socviz"
#' )
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
  fun = function(settings, ...) {
    env2env(settings, environment(), c("datapoints", "xlabs"))

    if (is.character(datapoints$x)) {
      datapoints$x = as.factor(datapoints$x)
    }
    if (is.factor(datapoints$x)) {
      # honour pre-ordered factors; otherwise fall back to first-appearance order
      xlvls = if (is.ordered(datapoints$x)) levels(datapoints$x) else unique(datapoints$x)
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
    fun = function(ix, iy, icol, ipch, ibg, ilty, ilwd, icex = 1, flip = FALSE, ...) {
        ltype = type
        if (isTRUE(flip)) {
            # flip_datapoints() has already swapped the coordinates, but the
            # base engine still draws these types in fixed orientations:
            # type = "h" always drops vertically, and the step types commit
            # to which coordinate moves first. So draw "h" as explicit
            # horizontal segments to the baseline, and mirror the step order.
            if (ltype == "h") {
                x0 = if (par("xlog")) 10^par("usr")[1] else 0
                segments(
                    x0 = x0,
                    y0 = iy,
                    x1 = ix,
                    y1 = iy,
                    col = icol,
                    lty = ilty,
                    lwd = ilwd
                )
                return(invisible(NULL))
            } else if (ltype == "s") {
                ltype = "S"
            } else if (ltype == "S") {
                ltype = "s"
            }
        }
        lines(
            x = ix,
            y = iy,
            col = icol,
            type = ltype,
            pch = ipch,
            bg = ibg,
            lty = ilty,
            lwd = ilwd,
            cex = icex
        )
    }
    return(fun)
}
