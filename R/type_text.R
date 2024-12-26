#' Text annotations plot type
#'
#' @description Type function for adding text annotations to a plot. This function allows
#' you to draw text at specified (x,y) coordinates. If no label is provided, the y-values
#' will be used as the text labels.
#'
#' @param labels Character vector of the same length as the number of x,y coordinates.
#' @param font Font to be used, following [graphics::par()]
#' @inheritParams graphics::text
#' @examples
#'
#' tinyplot(mpg ~ hp | factor(cyl),
#'   data = mtcars,
#'   type = type_text(
#'     labels = row.names(mtcars),
#'     font = 2,
#'     adj = 0))
#'
#' @export
type_text = function(labels, adj = NULL, pos = NULL, offset = 0.5, vfont = NULL, font = NULL) {
  out = list(
    draw = draw_text(adj = adj, pos = pos, offset = offset, vfont = vfont, font = font),
    data = data_text(labels = labels),
    name = "text"
  )
  class(out) = "tinyplot_type"
  return(out)
}

data_text = function(labels) {
  fun = function(datapoints, ...) {
    assert_character(labels, name = "labels")
    if (length(labels) != 1 && length(labels) != nrow(datapoints)) {
      msg <- sprintf("`labels` must be of length 1 or %s.", nrow(datapoints))
      stop(msg, call. = FALSE)
    }
    datapoints$labels = labels
    out = list(datapoints = datapoints)
    return(out)
  }
  return(fun)
}

draw_text = function(adj = NULL, pos = NULL, offset = 0.5, vfont = NULL, font = NULL) {
  fun = function(ix, iy, ilabels, icol, cex, ...) {
    text(x = ix, y = iy, labels = ilabels, col = icol, adj = adj, pos = pos, offset = offset, vfont = vfont, font = font, cex = cex)
  }
}

