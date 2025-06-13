#' @rdname type_abline
#' @param h y-value(s) for horizontal line(s). Numeric of length 1, or equal to
#'   the number of groups or number of facets.
#' @export
type_hline = function(h = 0) {
  assert_numeric(h)
  data_hline = function(datapoints, lwd, lty, col, ...) {
    if (nrow(datapoints) == 0) {
      msg = "`type_hline() only works on existing plots with x and y data points."
      stop(msg, call. = FALSE)
    }
    # keep track of unique lty and lwd (needed for group catch / escape hatch
    # later in draw_hline)
    ul_lwd = length(unique(lwd))
    ul_lty = length(unique(lty))
    ul_col = length(unique(col))
    return(list(type_info = list(ul_lty = ul_lty, ul_lwd = ul_lwd, ul_col = ul_col)))
  }
  draw_hline = function() {
    fun = function(
      ifacet, iby, data_facet, icol, ilty, ilwd,
      ngrps, nfacets, by_continuous, facet_by,
      type_info,
      ...
    ) {
      
      # browser()
      if (length(h) != 1) {
        if (!length(h) %in% c(ngrps, nfacets)) {
          msg = "Length of 'h' must be 1, or equal to the number of facets or number of groups."
          stop(msg, call. = FALSE)
        }
        if (length(h) == nfacets) {
          h = h[ifacet]
          if (!facet_by && by_continuous) icol = 1
        } else if (!by_continuous) {
          h = h[iby]
        }
      } else if (type_info[["ul_col"]]!=1 && !(type_info[["ul_lty"]]==ngrps || type_info[["ul_lwd"]]==ngrps)) {
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
