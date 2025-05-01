#' @rdname type_abline
#' @param h y-value(s) for horizontal line(s). Numeric of length 1 or equal to
#'   the number of facets.
#' @export
type_hline = function(h = 0) {
  data_hline = function(datapoints, ...) {
    if (nrow(datapoints) == 0) {
      msg = "`type_hline() only works on existing plots with x and y data points."
      stop(msg, call. = FALSE)
    }
    return(list())
  }
  draw_hline = function() {
    fun = function(ifacet, data_facet, icol, ilty, ilwd, ...) {
      nfacets = length(data_facet)

      if (length(h) == 1) {
        h = rep(h, nfacets)
      } else if (length(h) != nfacets) {
        msg = "Length of 'h' must be 1 or equal to the number of facets"
        stop(msg, call. = FALSE)
      }

      abline(h = h[ifacet], col = icol, lty = ilty, lwd = ilwd)
    }
    return(fun)
  }
  out = list(
    draw = draw_hline(),
    data = data_hline,
    name = "hline"
  )
  class(out) = "tinyplot_type"
  return(out)
}
