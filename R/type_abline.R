#' Add straight lines to a plot
#'
#' @inheritParams graphics::abline
#' @inheritParams tinyplot
#' @examples
#' mod = lm(mpg ~ hp, data = mtcars)
#' y = mtcars$mpg
#' yhat = predict(mod)
#' tinyplot(y, yhat, xlim = c(0, 40), ylim = c(0, 40))
#' tinyplot_add(type = type_abline(a = 0, b = 1))
#' @export
type_abline = function(a = 0, b = 1, col = NULL, lty = NULL, lwd = NULL) {
  draw_abline = function() {
    fun = function(ifacet, data_facet, icol, ilty, ilwd, ...) {
      nfacets = length(data_facet)

      if (length(a) == 1) {
        a = rep(a, nfacets)
      } else if (length(a) != nfacets) {
        msg = "Length of 'a' must be 1 or equal to the number of facets"
        stop(msg, call. = FALSE)
      }

      if (length(b) == 1) {
        b = rep(b, nfacets)
      } else if (length(b) != nfacets) {
        msg = "Length of 'b' must be 1 or equal to the number of facets"
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

      abline(a = a[ifacet], b = b[ifacet], col = col[ifacet], lty = lty[ifacet], lwd = lwd[ifacet])
    }
    return(fun)
  }
  out = list(
    draw = draw_abline(),
    data = NULL,
    name = "abline"
  )
  class(out) = "tinyplot_type"
  return(out)
}
