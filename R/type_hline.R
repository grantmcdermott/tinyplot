#' Trace a horizontal line on the plot
#'
#' @param h y-value(s) for horizontal line(s). Numeric of length 1 or equal to the number of facets.
#' @inheritParams graphics::abline
#' @inheritParams tinyplot
#' @examples
#' tinyplot(mpg ~ hp | factor(cyl), facet = ~ factor(cyl), data = mtcars)
#' tinyplot_add(type = type_hline(h = 12, col = "pink", lty = 3, lwd = 3))
#' @export
type_hline = function(h = 0, col = NULL, lty = NULL, lwd = NULL) {
  draw_hline = function() {
    fun = function(ifacet, data_facet, icol, ilty, ilwd, ...) {
      nfacets = length(data_facet)

      if (length(h) == 1) {
        h = rep(h, nfacets)
      } else if (length(h) != nfacets) {
        msg = "Length of 'h' must be 1 or equal to the number of facets"
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
