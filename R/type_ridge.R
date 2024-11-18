#' Ridge plot type
#'
#' @description Type function for producing ridge plots (also known as joy plots),
#' which display density distributions for multiple groups with vertical offsets.
#' This function uses `tinyplot` scaffolding, which enables added functionality
#' such as grouping and faceting.
#'
#' The line color is controlled by the `col` argument in the `tinyplot()` call.
#' The fill color is controlled by the `bg` argument in the `tinyplot()` call.
#'
#'
#' @param scale Numeric. Controls the scaling factor of each plot.
#' Values greater than 1 means that plots overlap.
#' @param gradient logical or character. Should a color gradient be used
#' to shade the area under the density? If a character specification is
#' used, then it can either be of length 1 and specify the palette to be
#' used with `gradient = TRUE` corresponding to `gradient = "viridis"`.
#' If a character vector of length greater than 1 is used, then it
#' should specifify the colors in the palette, e.g.,
#' `gradient = hcl.colors(100)`.
#' @param breaks Numeric. If a color gradient is used for shading, the
#' breaks between the colors can be modified. The default is to use
#' equidistant breaks spanning the range of the `x` variable.
#'
#' @examples
#' tinyplot(Species ~ Sepal.Width, data = iris, type = "ridge")
#'
#' tinyplot(Month ~ Ozone,
#'   data = airquality,
#'   type = type_ridge(scale = 1),
#'   bg = "light blue", col = "black")
#'
#' ## equivalent specifications of color gradients
#' tinyplot(Species ~ Sepal.Width, data = iris, type = type_ridge(gradient = TRUE))
#' tinyplot(Species ~ Sepal.Width, data = iris, type = type_ridge(gradient = "viridis"))
#' tinyplot(Species ~ Sepal.Width, data = iris, type = type_ridge(gradient = hcl.colors(100)))
#'
#' ## color gradient with fewer segments
#' tinyplot(Species ~ Sepal.Width, data = iris,
#'   type = type_ridge(gradient = TRUE, breaks = pretty(iris$Sepal.Width, 6)))
#'
#'
#' @export
type_ridge = function(scale = 1.5, gradient = FALSE, breaks = NULL) {
  data_ridge = function() {
    fun = function(datapoints, ...) {
      get_density = function(k) {
        out = density(k$x)
        out = data.frame(x = out$x, ymax = out$y, ymin = 0, y = k$y[1])
        out$ymax = out$ymax / max(out$ymax) * scale
        out$facet = k$facet[1]
        out$by = k$by[1]
        return(out)
      }
      d = split(datapoints, list(datapoints$y, datapoints$facet))
      d = lapply(d, function(k) tryCatch(get_density(k), error = function(e) NULL))
      d = do.call(rbind, Filter(function(x) !is.null(x), d))
      d = split(d, d$facet)
      offset_z = function(k) {
        ksplit = split(k, k$y)
        for (idx in seq_along(ksplit)) {
          ksplit[[idx]]$ymax = ksplit[[idx]]$ymax + idx - 1
          ksplit[[idx]]$ymin = ksplit[[idx]]$ymin + idx - 1
        }
        k = do.call(rbind, ksplit)
        return(k)
      }
      d = do.call(rbind, lapply(d, offset_z))

      ## use color gradient?
      xlim = range(d$x, na.rm = TRUE)
      if (!isFALSE(gradient)) {
        palette = gradient
        gradient = TRUE
        if (isTRUE(palette)) palette = "viridis"
        if (is.character(palette) && length(palette) == 1L) {
          palette = hcl.colors(if (is.null(breaks)) 100 else length(breaks) - 1L, palette = palette)
        }
        if (is.null(breaks)) breaks = seq(from = xlim[1L], to = xlim[2L], length.out = length(palette) + 1L)
        palette = rep_len(palette, length(breaks) - 1L)
      } else {
        palette = NULL
        if (!is.null(breaks)) gradient = TRUE
      }
      if (!is.null(breaks)) {
        breaks[1L] = pmin(breaks[1L], xlim[1L])
        breaks[length(breaks)] = pmax(breaks[length(breaks)], xlim[2L])
      }

      out = list(
        datapoints = d,
        yaxt = "n",
        ylim = c(min(d$ymin), max(d$ymax)),
        type_info = list(
          gradient = gradient,
          palette = palette,
          breaks = breaks)
      )
      return(out)
    }
    return(fun)
  }

  draw_ridge = function() {
    fun = function(ix, iy, iz, ibg, icol, iymin, iymax, type_info, ...) {
      d = data.frame(x = ix, y = iy, ymin = iymin, ymax = iymax)
      dsplit = split(d, d$y)
      if (is.null(ibg)) ibg = "grey"
      for (i in rev(seq_along(dsplit))) {
        if (type_info[["gradient"]]) {
          with(dsplit[[i]], segmented_polygon(x, ymax, ymin = ymin[1L],
            breaks = type_info[["breaks"]],
            col = if (is.null(type_info[["palette"]])) ibg else type_info[["palette"]],
            border = if (is.null(type_info[["palette"]])) icol else "transparent"))
        }
        with(dsplit[[i]], polygon(x, ymax, col = if (type_info[["gradient"]]) "transparent" else ibg, border = icol))
      }
      lab = unique(d$y)
      val = cumsum(rep(1, length(lab))) - 1
      axis(2, at = val, lab)
    }
    return(fun)
  }

  out = list(
    draw = draw_ridge(),
    data = data_ridge(),
    name = "ridge"
  )
  class(out) = "tinyplot_type"
  return(out)
}

## auxiliary function for drawing shaded segmented polygon
#' @importFrom stats approx
segmented_polygon = function(x, y, ymin = 0, breaks = range(x), col = "lightgray", border = "transparent") {

  ## sanity check
  if (breaks[1L] > x[1L] || breaks[length(breaks)] < x[length(x)]) stop("'breaks' do no span range of 'x'")

  ## recycle color (if necessary)
  col = rep_len(col, length(breaks) - 1L)

  ## y values at breaks
  ybreaks = approx(x, y, xout = breaks)$y
  ybreaks[is.na(ybreaks)] = ymin

  ## add start/end of polygon
  x = x[c(1, seq_along(x), length(x))]
  y = c(ymin, y, ymin)

  ## split data into segments defined by breaks
  xy = split(data.frame(x = x, y = y), cut(x, breaks = breaks, include.lowest = TRUE))

  ## non-empty segments
  ok = vapply(xy, nrow, 0L) > 0L

  ## augment each segment with breaks and add NA in between
  xy = lapply(seq_along(xy)[ok], function(i) rbind(
    data.frame(x = rep.int(breaks[i], 2L), y = c(ymin, ybreaks[i])),
    xy[[i]],
    data.frame(x = c(rep.int(breaks[i + 1L], 2L), NA), y = c(ybreaks[i + 1L], ymin, NA))
  ))

  ## combine everything, omit last NA separator
  xy = do.call("rbind", xy)
  xy = xy[-nrow(xy), , drop = FALSE]

  ## draw all polygons
  polygon(xy$x, xy$y, col = col[ok], border = border)
}
