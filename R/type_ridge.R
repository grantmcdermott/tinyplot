#' Ridge plot type
#'
#' @description Type function for producing ridge plots (also known as joy plots),
#' which display density distributions for multiple groups with vertical offsets.
#' This function uses `tinyplot` scaffolding, which enables added functionality
#' such as grouping and faceting.
#'
#' @param offset Numeric. Controls the vertical spacing between the ridges.
#'   Default is `0.8`. Smaller values will result in more overlap.
#' @param col Character. Fill color of each ridge.
#' @param border Character. Color of the ridge borders.
#'
#' @examples
#' tinyplot(Species ~ Sepal.Width, data = iris, type = "ridge")
#'
#' tinyplot(Month ~ Ozone,
#'   data = airquality,
#'   type = type_ridge(offset = .5, col = "light blue", border = "black"))
#'
#' @export
type_ridge = function(offset = .8, col = "grey", border = "white") {
  data_ridge = function() {
    fun = function(datapoints, ...) {
      d = split(datapoints, datapoints$y)
      get_density = function(k) {
        tmp = density(k$x)
        tmp = data.frame(x = tmp$x, y = tmp$y, z = k$y[1])
        tmp$y = unlist(tapply(tmp$y, tmp$z, function(k) k / max(k)))
        tmp
      }
      d = lapply(d, function(k) tryCatch(get_density(k), error = function(e) NULL))
      for (idx in seq_along(d)) {
        d[[idx]]$y = d[[idx]]$y + offset * (idx - 1)
      }
      out = do.call(rbind, d)
      out$by = out$facet = ""

      out = list(
        datapoints = out,
        yaxt = "n",
        ylim = range(out$y)
      )
      return(out)
    }
    return(fun)
  }

  draw_ridge = function() {
    fun = function(ix, iy, iz, ...) {
      d = data.frame(x = ix, y = iy, z = iz)
      ds = split(d, d$z)
      for (i in rev(seq_along(ds))) {
        with(ds[[i]], polygon(x, y = y, col = col, border = border))
      }
      lab = unique(d$z)
      val = cumsum(rep(offset, length(lab))) - offset
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
