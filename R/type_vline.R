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
    fun = function(ifacet, data_facet, icol, ilty, ilwd, ngrps, nfacets, by_continuous, ...) {

      if (length(v) != 1) {
        if (!length(v) %in% c(ngrps, nfacets)) {
          msg = "Length of 'v' must be 1, or equal to the number of facets or number of groups."
          stop(msg, call. = FALSE)
        }
        v = if (length(v) == nfacets) v[ifacet] else h[iby]
      } else if (by_continuous) {
        icol = 1
      }
      

      abline(v = v, col = icol, lty = ilty, lwd = ilwd)
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
