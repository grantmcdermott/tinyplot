#' Trace a horizontal line on the plot
#'
#' @param h y-value(s) for horizontal line(s). Numeric of length 1 or equal to the number of facets.
#' @inheritParams graphics::abline
#' @inheritParams tinyplot
#' @examples
#' tinyplot(mpg ~ hp | factor(cyl), facet = ~ factor(cyl), data = mtcars)
#' tinyplot_add(type = type_hline(h = 12, col = "pink", lty = 3, lwd = 3))
#' @export
type_hline = function(h = 0, col = "black", lty = 1, lwd = 1) {
  draw_hline = function() {
    fun = function(ifacet, data_facet, ...) {
      nfacets = length(data_facet)
      ifacet = 1
      if (length(h) == 1) h = rep(h, nfacets)
      if (length(col) == 1) col = rep(col, nfacets)
      if (length(lty) == 1) lty = rep(lty, nfacets)
      if (length(lwd) == 1) lwd = rep(lwd, nfacets)
      abline(h = h[ifacet], col = col[ifacet], lty = lty[ifacet], lwd = lwd[ifacet])
    }
    return(fun)
  }
  out = list(
    draw = draw_hline(),
    data = NULL,
    name = "hline"
  )
  class(out) = "tinyplot_type"
  return(out)
}

