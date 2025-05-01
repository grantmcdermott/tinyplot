#' @param v x-value(s) for vertical line(s). Numeric of length 1 or equal to the
#'   number of facets.
#' @rdname type_abline
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
