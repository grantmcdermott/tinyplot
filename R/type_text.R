#' Text annotations plot type
#'
#' @description Type function for adding text annotations to a plot at the
#' specified (`x`,`y`) coordinates.
#'
#' @param labels Character vector of length `1` or the same length as the
#'   number of `x`,`y` coordinates. If left as `NULL`, then the labels will
#'   automatically inherit the corresponding `y` values. See Examples.
#' @param labeller A formatting function (or convenience string) passed to
#'   [`tinylabel`] for formatting the `labels`. Useful for ensuring that the
#'   text labels match the formatting of an axis, e.g. `labeller = "%"` to
#'   display the labels as percentages. Default is `NULL`, i.e. no formatting.
#'   See Examples.
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
#' @param repel Logical or numeric controlling automatic repulsion of
#'   overlapping text labels. The default `FALSE` draws the labels at their
#'   exact (`x`,`y`) coordinates. `TRUE` nudges overlapping labels apart using a
#'   force-directed algorithm (labels are pushed off each other and then sprung
#'   back toward their original positions). A numeric value does the same but
#'   additionally enforces that much minimum padding (in user coordinates)
#'   between labels. Works best with the default centered text placement (i.e.
#'   without `pos`). **Caveat:** The repulsion logic currently operates
#'   within each group rather than across groups. So the text of different
#'   groups may still overlap. See Examples.
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
#' # you can also use a labeller function (passed to `tinylabel`) to
#' # customize
#' tinyplot(1:12, type = "text", labels = month.abb, labeller = toupper)
#'
#' # for advanced customization, it's safer to pass args through `type_text()`
#' tinyplot(
#'   1:12,
#'   type = type_text(
#'   labels = month.abb,
#'   family = "HersheyScript",
#'   srt = -20)
#' )
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
#' # use `repel = TRUE` to automatically nudge overlapping labels apart
#' tinyplot(
#'   mpg ~ wt, data = mtcars,
#'   type = type_text(labels = row.names(mtcars), repel = TRUE)
#' )
#'
#' # limitation: `repel` logic currently works per group, so grouped text data
#' # may still overlap
#' tinyplot(
#'   mpg ~ wt | factor(cyl), data = mtcars,
#'   type = type_text(labels = row.names(mtcars), repel = TRUE)
#' )
#'
#' @export
type_text = function(
  labels = NULL,
  labeller = NULL,
  adj = NULL,
  pos = NULL,
  offset = 0.5,
  family = NULL,
  font = NULL,
  vfont = NULL,
  xpd = NULL,
  srt = 0,
  repel = FALSE,
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
      srt = srt,
      repel = repel
    ),
    data = data_text(labels = labels, labeller = labeller, clim = clim),
    name = "text"
  )
  class(out) = "tinyplot_type"
  return(out)
}

data_text = function(labels = NULL, labeller = NULL, clim = c(0.5, 2.5)) {
  fun = function(settings, ...) {
    env2env(settings, environment(), "datapoints")

    # Store clim for bubble() function
    settings$clim = clim

    if (is.null(labels)) {
      labels = datapoints$y
    }
    if (length(labels) != 1 && length(labels) != nrow(datapoints)) {
      msg = sprintf("`labels` must be of length 1 or %s.", nrow(datapoints))
      stop(msg, call. = FALSE)
    }
    if (!is.null(labeller)) {
      labels = tinylabel(labels, labeller)
    }
    datapoints$labels = labels
    if (is.factor(datapoints$x)) {
      datapoints$x = as.numeric(datapoints$x)
    }
    if (is.factor(datapoints$y)) {
      datapoints$y = as.numeric(datapoints$y)
    }

    env2env(environment(), settings, "datapoints")
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
  srt = 0,
  repel = FALSE
) {
  if (is.null(xpd)) {
    xpd = par("xpd")
  }
  if (!is.null(family)) {
    vfont = NULL
  }
  fun = function(ix, iy, ilabels, icol, icex, ...) {
    # Optionally repel overlapping labels. Measurement requires user
    # coordinates, which are only correct here (post `plot.window()`), not in
    # the type's data function. See `repel_text()` in R/repel.R.
    if (!isFALSE(repel) && length(ix) > 1) {
      min_gap = if (isTRUE(repel)) 0 else as.numeric(repel)
      w = strwidth(ilabels, units = "user", cex = icex, font = font, family = family)
      h = strheight(ilabels, units = "user", cex = icex, font = font, family = family)
      rp = repel_text(
        x = ix, y = iy, widths = w, heights = h,
        min_gap = min_gap, axis = "both"
      )
      ix = rp[["x"]]
      iy = rp[["y"]]
    }
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
