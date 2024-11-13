#' Quantile-Quantile plot (QQ)
#'
#' @description Plots the theoretical quantiles of `y` on the x-axis against observed values of `y` on the y-axis.
#'
#' @param distribution Distribution function to use.
#' @inheritParams tinyplot
#' @examples
#' tinyplot(~mpg, data = mtcars, type = type_qq(lty = 3))
#' @importFrom stats qnorm ppoints
#' @export
type_qq = function(distribution = qnorm, lty = NULL, col = "black", lwd = 1) {
  data_qq = function(distribution) {
    fun = function(datapoints, xlabs, ...) {
      y = sort(datapoints$y)
      x = datapoints$x
      x = distribution(ppoints(x))
      datapoints$x = x
      datapoints$y = y
      out = list(
        datapoints = datapoints,
        xlabs = xlabs
      )
      return(out)
    }
  }

  draw_qq = function(lty, col, lwd) {
    fun = function(ix, iy, icol, ibg, ipch, ilwd, cex, xlab, ...) {
      points(
        x = ix,
        y = iy,
        col = icol,
        bg = ibg,
        type = "p",
        pch = ipch,
        lwd = ilwd,
        cex = cex
      )

      if (!is.null(lty)) {
        iy <- quantile(iy, c(0.25, 0.75))
        ix <- quantile(ix, c(0.25, 0.75))
        slope <- diff(iy) / diff(ix)
        intercept <- iy[1] - slope * ix[1]
        abline(a = intercept, b = slope, lty = lty, col = col, lwd = lwd)
      }
    }
    return(fun)
  }

  out = list(
    draw = draw_qq(lty = lty, col = col, lwd = lwd),
    data = data_qq(distribution = distribution),
    name = "p"
  )
  class(out) = "tinyplot_type"
  return(out)
}
