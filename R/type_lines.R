#' Lines plot type
#'
#' @description Type function for plotting lines.
#' 
#' @inheritParams graphics::plot.default
#' @inheritParams dodge_positions
#' @inheritParams type_points
#'
#' @section Categorical axes:
#'
#' Like the other plot types, `type_lines()` places categorical (factor or
#' character) data according to the factor levels. Character variables are
#' coerced with [factor()] and so end up in alphabetical order. To order the
#' categories by their appearance in the data instead, use
#' `xlevels = "asis"`, or set the levels explicitly, e.g.
#' `factor(x, levels = unique(x))`.
#'
#' Note that the lines themselves are always drawn in the order that the rows
#' arrive in, exactly as base [lines()] does. Categories whose level order
#' differs from their row order will therefore produce a zig-zag, just as an
#' unsorted numeric x-variable would.
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
type_lines = function(type = "l", dodge = 0, fixed.dodge = FALSE, xlevels = NULL) {
  out = list(
    draw = draw_lines(type = type),
    data = data_lines(dodge = dodge, fixed.dodge = fixed.dodge, xlevels = xlevels),
    name = type
  )
  class(out) = "tinyplot_type"
  return(out)
}


data_lines = function(dodge = 0, fixed.dodge = FALSE, xlevels = NULL) {
  fun = function(settings, ...) {
    env2env(settings, environment(), "datapoints")

    # Categorical axes follow the factor levels, exactly as in data_points().
    # (Character vectors have already been coerced by sanitize_datapoints().)
    # Ordering by the levels rather than by first appearance means an explicit
    # `factor(x, levels = ...)` is honoured, and that layering a line type onto
    # a point type (or vice versa) lands on the same categories. #679
    datapoints[["x"]] = sanitize_xlevels(datapoints[["x"]], xlevels)
    if (is.factor(datapoints[["x"]])) {
      xlvls = levels(datapoints[["x"]])
      xlabs = seq_along(xlvls)
      names(xlabs) = xlvls
      datapoints[["x"]] = as.integer(datapoints[["x"]])
    } else {
      xlabs = NULL
    }
    if (is.factor(datapoints[["y"]])) {
      ylvls = levels(datapoints[["y"]])
      ylabs = seq_along(ylvls)
      names(ylabs) = ylvls
      datapoints[["y"]] = as.integer(datapoints[["y"]])
    } else {
      ylabs = NULL
    }

    # dodge
    if (dodge != 0) {
      datapoints = dodge_positions(datapoints, dodge, fixed.dodge)
    }

    x = datapoints[["x"]]
    y = datapoints[["y"]]
    env2env(environment(), settings, c(
      "x",
      "y",
      "xlabs",
      "ylabs",
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
