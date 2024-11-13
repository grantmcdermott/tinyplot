#' Trace a vertical line on the plot
#'
#' @param v x-value(s) for vertical line(s). Numeric of length 1 or equal to the number of facets.
#' @inheritParams tinyplot
#' @examples
#' tinyplot(mpg ~ hp, data = mtcars)
#' tinyplot_add(type = type_vline(150))
#'
#' # facet-specify location and colors
#' cols = c("black", "green", "orange")
#' tinyplot(mpg ~ hp | factor(cyl), facet = ~ factor(cyl), data = mtcars, col = cols)
#' tinyplot_add(type = type_vline(
#'   v = c(100, 150, 200), col = cols, lty = 3, lwd = 3
#' ))
#' @export
type_vline = function(v = 0, col = "black", lty = 1, lwd = 1) {
  assert_numeric(v)
  data_vline = function(datapoints, ...) {
    if (nrow(datapoints) == 0) {
      msg = "`type_vline() only works on existing plots with x and y data points."
      stop(msg, call. = FALSE)
    }
    return(list())
  }
  draw_vline = function() {
    fun = function(ifacet, data_facet, icol, ilty, ilwd, ...) {
      nfacets = length(data_facet)

      if (length(v) == 1) {
        v = rep(v, nfacets)
      } else if (length(v) != nfacets) {
        msg = "Length of 'v' must be 1 or equal to the number of facets"
        stop(msg, call. = FALSE)
      }

      if (is.null(col)) {
        col = icol
      }
      if (length(col) == 1) {
        col = rep(col, nfacets)
      } else if (length(col) != nfacets) {
        msg = "Length of 'col' must be 1 or equal to the number of facets"
        stop(msg, call. = FALSE)
      }

      if (is.null(lty)) {
        lty = if (!is.null(ilty)) ilty else 1
      }
      if (length(lty) == 1) {
        lty = rep(lty, nfacets)
      } else if (length(lty) != nfacets) {
        msg = "Length of 'lty' must be 1 or equal to the number of facets"
        stop(msg, call. = FALSE)
      }

      if (is.null(lwd)) {
        lwd = if (!is.null(ilwd)) ilwd else 1
      }
      if (length(lwd) == 1) {
        lwd = rep(lwd, nfacets)
      } else if (length(lwd) != nfacets) {
        msg = "Length of 'lwd' must be 1 or equal to the number of facets"
        stop(msg, call. = FALSE)
      }

      abline(v = v[ifacet], col = col[ifacet], lty = lty[ifacet], lwd = lwd[ifacet])
    }
    return(fun)
  }
  out = list(
    draw = draw_vline(),
    data = data_vline,
    name = "vline"
  )
  class(out) = "tinyplot_type"
  return(out)
}
