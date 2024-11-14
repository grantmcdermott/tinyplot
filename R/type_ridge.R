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
#'
#' @examples
#' tinyplot(Species ~ Sepal.Width, data = iris, type = "ridge")
#'
#' tinyplot(Month ~ Ozone,
#'   data = airquality,
#'   type = type_ridge(scale = 1),
#'   bg = "light blue", col = "black")
#'
#' @export
type_ridge = function(scale = 1.5) {
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

      out = list(
        datapoints = d,
        yaxt = "n",
        ylim = c(min(d$ymin), max(d$ymax))
      )
      return(out)
    }
    return(fun)
  }

  draw_ridge = function() {
    fun = function(ix, iy, iz, ibg, icol, iymin, iymax, ...) {
      d = data.frame(x = ix, y = iy, ymin = iymin, ymax = iymax)
      dsplit = split(d, d$y)
      if (is.null(ibg)) ibg = "grey"
      for (i in rev(seq_along(dsplit))) {
        with(dsplit[[i]], polygon(x, ymax, col = ibg, border = icol))
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
