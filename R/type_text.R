#' Text annotations plot type
#'
#' @description Type function for adding text annotations to a plot at the
#' specified (`x`,`y`) coordinates.
#'
#' @param labels Character vector of length `1` or the same length as the
#'   number of `x`,`y` coordinates. If left as `NULL`, then the labels will
#'   automatically inherit the corresponding `y` values. See Examples.
#' @param family The name of a font family. Default of `NULL` means that the
#' family will be the same as the main plot text, following
#' \code{\link[graphics]{par}}. Note that if a `family` argument is provided,
#' then `vfont` (below) will automatically be ignored.
#' @param font Integer giving the font face to be used,
#' following \code{\link[graphics]{par}}. On most devices, the mapping is: `1` =
#' regular, `2` = bold, `3` = italic, `4` = bold italic, and `5` = symbol.
#' @param xpd Logical value or `NA` denoting text clipping behaviour, following
#'  \code{\link[graphics]{par}}.
#' @param srt Numeric giving the desired string rotation in degrees.
#' @param clim Numeric giving the lower and upper limits of the character
#'   expansion (`cex`) normalization for bubble charts.
#' @inheritParams graphics::text
#' @examples
#' # simplest case (no labels), will auto revert to y labels
#' tinyplot(1:12, type = "text")
#'
#' # pass explicit `labels` arg if you want specific text
#' tinyplot(1:12, type = "text", labels = month.abb)
#'
#' # for advanced customization, it's safer to pass args through `type_text()`
#' tinyplot(1:12, type = type_text(
#'   labels = month.abb, family = "HersheyScript", srt = -20))
#'
#' # same principles apply to grouped and/or facet data
#' tinyplot(mpg ~ hp | factor(cyl),
#'   data = mtcars,
#'   type = type_text(
#'     labels = row.names(mtcars),
#'     family = "HersheySans",
#'     font = 2,
#'     adj = 0
#'   )
#' )
#'
#' # tip: use `xpd = NA` to avoid clipping text at the plot region
#' tinyplot(mpg ~ hp | factor(cyl),
#'   data = mtcars,
#'   type = type_text(
#'     labels = row.names(mtcars),
#'     family = "HersheySans",
#'     font = 2,
#'     adj = 0,
#'     xpd = NA
#'   )
#' )
#'
#' @export
type_text = function(
  labels = NULL,
  adj = NULL,
  pos = NULL,
  offset = 0.5,
  family = NULL,
  font = NULL,
  vfont = NULL,
  xpd = NULL,
  srt = 0,
  clim = c(0.5, 2.5)
) {
  out = list(
    draw = draw_text(
      adj = adj,
      pos = pos,
      offset = offset,
      vfont = vfont,
      family = family,
      font = font,
      xpd = xpd,
      srt = srt
    ),
    data = data_text(labels = labels, clim = clim),
    name = "text"
  )
  class(out) = "tinyplot_type"
  return(out)
}

data_text = function(labels = NULL, clim = c(0.5, 2.5)) {
  fun = function(settings, cex = NULL, ...) {
    list2env(settings, envir = environment())
    if (is.null(labels)) {
      labels = datapoints$y
    }
    if (length(labels) != 1 && length(labels) != nrow(datapoints)) {
      msg = sprintf("`labels` must be of length 1 or %s.", nrow(datapoints))
      stop(msg, call. = FALSE)
    }
    datapoints$labels = labels
    if (is.factor(datapoints$x)) {
      datapoints$x = as.numeric(datapoints$x)
    }
    if (is.factor(datapoints$y)) {
      datapoints$y = as.numeric(datapoints$y)
    }

    bubble = FALSE
    bubble_cex = 1
    if (!is.null(cex) && length(cex) == nrow(datapoints)) {
      bubble = TRUE
      ## Identify the pretty break points for our bubble labels
      bubble_labs = pretty(cex, n = 5)
      len_labs = length(bubble_labs)
      # cex = rescale_num(c(bubble_labs, cex), to = clim)
      cex = rescale_num(sqrt(c(bubble_labs, cex)) / pi, to = clim)
      bubble_cex = cex[1:len_labs]
      cex = cex[(len_labs + 1):length(cex)]
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

    out = list(
      datapoints = datapoints,
      cex = cex,
      bubble = bubble,
      bubble_cex = bubble_cex
    )
    out = modify_list(settings, out)
    return(out)
  }
  return(fun)
}

draw_text = function(
  adj = NULL,
  pos = NULL,
  offset = 0.5,
  vfont = NULL,
  family = NULL,
  font = NULL,
  xpd = NULL,
  srt = 0
) {
  if (is.null(xpd)) {
    xpd = par("xpd")
  }
  if (!is.null(family)) {
    vfont = NULL
  }
  fun = function(ix, iy, ilabels, icol, icex, ...) {
    text(
      x = ix,
      y = iy,
      labels = ilabels,
      col = icol,
      adj = adj,
      pos = pos,
      offset = offset,
      family = family,
      font = font,
      vfont = vfont,
      xpd = xpd,
      srt = srt,
      cex = icex
    )
  }
}
