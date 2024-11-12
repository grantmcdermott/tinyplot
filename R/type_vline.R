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
  draw_vline = function() {
    fun = function(data_facet, ifacet, ...) {
      nfacets = length(data_facet)
      if (length(v) == 1) v = rep(v, nfacets)
      if (length(col) == 1) col = rep(col, nfacets)
      if (length(lty) == 1) lty = rep(lty, nfacets)
      if (length(lwd) == 1) lwd = rep(lwd, nfacets)
      abline(v = v[ifacet], col = col[ifacet], lty = lty[ifacet], lwd = lwd[ifacet])
    }
    return(fun)
  }
  out = list(
    draw = draw_vline(),
    data = NULL,
    name = "vline"
  )
  class(out) = "tinyplot_type"
  return(out)
}

