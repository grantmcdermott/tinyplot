#' @param v x-value(s) for vertical line(s). Numeric of length 1, or equal to
#'   the number of groups or number of facets (or the product thereof).
#' @rdname type_abline
#' @export
type_vline = function(v = 0) {
  assert_numeric(v)
  data_vline = function(datapoints, lwd, lty, col, ...) {
    if (nrow(datapoints) == 0) {
      msg = "`type_vline() only works on existing plots with x and y data points."
      stop(msg, call. = FALSE)
    }
    # keep track of unique lty and lwd (needed for group catch / escape hatch
    # later in draw_hline)
    ul_lwd = length(unique(lwd))
    ul_lty = length(unique(lty))
    ul_col = length(unique(col))
    return(list(type_info = list(ul_lty = ul_lty, ul_lwd = ul_lwd, ul_col = ul_col)))
  }
  draw_vline = function() {
    fun = function(
      ifacet, iby, data_facet, icol, ilty, ilwd,
      ngrps, nfacets, by_continuous, facet_by,
      type_info,
      ...
    ) {

      # flag for aesthetics by groups
      grp_aes = type_info[["ul_col"]] == 1 || type_info[["ul_lty"]] == ngrps || type_info[["ul_lwd"]] == ngrps
      
      if (length(v) != 1) {
        if (!length(v) %in% c(ngrps, nfacets, ngrps*nfacets)) {
          msg = "Length of 'v' must be 1, or equal to the number of facets or number of groups (or product thereof)."
          stop(msg, call. = FALSE)
        }
        if (length(v) == nfacets) {
          v = v[ifacet]
          if (!grp_aes) {
            icol = 1
          } else if (!facet_by && by_continuous) {
            icol = 1
          }
        } else if (!by_continuous && length(v) == ngrps * nfacets) {
          v = v[ifacet * ngrps - c(ngrps - iby)]
        } else if (!by_continuous) {
          v = v[iby]
        }
      } else if (!grp_aes) {
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
