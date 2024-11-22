#' Trace a vertical line on the plot
#'
#' @param v x-value(s) for vertical line(s). Numeric of length 1 or equal to the number of facets.
#' @examples
#' tinyplot(mpg ~ hp, data = mtcars)
#' tinyplot_add(type = type_vline(150))
#'
#' # facet-specify location and colors
#' cols = c("black", "green", "orange")
#' tinyplot(mpg ~ hp | factor(cyl), facet = ~ factor(cyl), data = mtcars, col = cols)
#' tinyplot_add(type = type_vline(v = c(100, 150, 200)), lty = 3, lwd = 3)
#'
#' @export
type_vline = function(v = 0) {
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

      abline(v = v[ifacet], col = icol, lty = ilty, lwd = ilwd)
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
