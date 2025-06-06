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
    fun = function(ifacet, iby, data_facet, icol, ilty, ilwd, ngrps, nfacets, by_continuous, ...) {
      
      if (length(h) != 1) {
        if (!length(h) %in% c(ngrps, nfacets)) {
          msg = "Length of 'h' must be 1, or equal to the number of facets or number of groups."
          stop(msg, call. = FALSE)
        }
        h = if (length(h) == nfacets) h[ifacet] else h[iby]
      } else if (by_continuous) {
        icol = 1
      }

      abline(h = h, col = icol, lty = ilty, lwd = ilwd)
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
