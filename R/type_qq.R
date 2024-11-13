#' Quantile-Quantile plot (QQ)
#'
#' @description Plots the theoretical quantiles of `x` on the horizontal axis
#' against observed values of `x` on the vertical axis.
#'
#' @param distribution Distribution function to use.
#' @examples
#' tinyplot(~mpg, data = mtcars, type = type_qq())
#'
#' # suppress the line
#' tinyplot(~mpg, data = mtcars, lty = 0, type = type_qq())
#' @importFrom stats qnorm ppoints quantile
#' @export
type_qq = function(distribution = qnorm) {
  data_qq = function(distribution) {
    fun = function(datapoints, ...) {
      y = sort(datapoints$y)
      x = datapoints$x
      x = distribution(ppoints(x))
      datapoints$x = x
      datapoints$y = y
      out = list(datapoints = datapoints)
      return(out)
    }
  }

  draw_qq = function() {
    fun = function(ix, iy, icol, ibg, ipch, ilwd, ilty, cex, xlab, ...) {
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

      if (!is.null(ilty)) {
        iy <- quantile(iy, c(0.25, 0.75))
        ix <- quantile(ix, c(0.25, 0.75))
        slope <- diff(iy) / diff(ix)
        intercept <- iy[1] - slope * ix[1]
        abline(a = intercept, b = slope, lty = ilty, col = icol, lwd = ilwd)
      }
    }
    return(fun)
  }

  out = list(
    draw = draw_qq(),
    data = data_qq(distribution = distribution),
    name = "qq"
  )
  class(out) = "tinyplot_type"
  return(out)
}
