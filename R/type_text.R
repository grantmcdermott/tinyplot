#' Text annotations plot type
#'
#' @description Type function for adding text annotations to a plot. This function allows
#' you to draw text at specified (x,y) coordinates.
#'
#' @param labels Character vector of length 1 or of the same length as the
#'  number of x,y coordinates.
#' @param font Font to be used, following [graphics::par()].
#' @param xpd Logical value or `NA` denoting text clipping behaviour, following
#'  [graphics::par()].
#' @param srt Numeric giving the desired string rotation in degrees.
#' @param clim Numeric giving the lower and upper limits of the character
#'   expansion (`cex`) normalization for bubble charts.
#' @inheritParams graphics::text
#' @examples
#' tinyplot(mpg ~ hp | factor(cyl),
#'   data = mtcars,
#'   type = type_text(
#'     labels = row.names(mtcars),
#'     font = 2,
#'     adj = 0
#'   )
#' )
#' 
#' # to avoid clipping text at the plot region, we can use xpd = NA
#' tinyplot(mpg ~ hp | factor(cyl),
#'   data = mtcars,
#'   type = type_text(
#'     labels = row.names(mtcars),
#'     font = 2,
#'     adj = 0,
#'     xpd = NA
#'   )
#' )
#'
#' @export
type_text = function(labels, adj = NULL, pos = NULL, offset = 0.5, vfont = NULL, font = NULL, xpd = NULL, srt = 0, clim = c(0.5, 2.5)) {
  out = list(
    draw = draw_text(adj = adj, pos = pos, offset = offset, vfont = vfont, font = font, xpd = xpd, srt = srt),
    data = data_text(labels = labels, clim = clim),
    name = "text"
  )
  class(out) = "tinyplot_type"
  return(out)
}

data_text = function(labels, clim = c(0.5, 2.5)) {
  fun = function(datapoints, legend_args, cex = NULL, ...) {
    if (length(labels) != 1 && length(labels) != nrow(datapoints)) {
      msg <- sprintf("`labels` must be of length 1 or %s.", nrow(datapoints))
      stop(msg, call. = FALSE)
    }
    datapoints$labels = labels
    
    # browser()
    bubble = FALSE
    bubble_cex = 1
    if (!is.null(cex) && length(cex) == nrow(datapoints)) {
      bubble = TRUE 
      ## Identify the pretty break points for our bubble labels
      bubble_labs = pretty(cex, n = 5)
      len_labs = length(bubble_labs)
      # cex = rescale_num(c(bubble_labs, cex), to = clim)
      cex = rescale_num(sqrt(c(bubble_labs, cex))/pi, to = clim)
      bubble_cex = cex[1:len_labs]
      cex = cex[(len_labs+1):length(cex)]
      names(bubble_cex) = format(bubble_labs)
      if (max(clim) > 2.5) {
        legend_args[["x.intersp"]] = max(clim) / 2.5
        legend_args[["y.intersp"]] = sapply(bubble_cex / 2.5, max, 1)
      }
    }

    out = list(
      datapoints = datapoints,
      cex = cex,
      bubble = bubble,
      bubble_cex = bubble_cex
    )
    return(out)
  }
  return(fun)
}

draw_text = function(adj = NULL, pos = NULL, offset = 0.5, vfont = NULL, font = NULL, xpd = NULL, srt = 0) {
  if (is.null(xpd)) xpd = par("xpd")
  fun = function(ix, iy, ilabels, icol, icex, ...) {
    text(
      x = ix, y = iy, labels = ilabels, col = icol,
      adj = adj, pos = pos, offset = offset,
      vfont = vfont, font = font,
      xpd = xpd,
      srt = srt,
      cex = icex
    )
  }
}
