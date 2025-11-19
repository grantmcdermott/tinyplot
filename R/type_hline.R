#' @rdname type_abline
#' @param h y-value(s) for horizontal line(s). Numeric of length 1, or equal to
#'   the number of groups or number of facets (or the product thereof).
#' @export
type_hline = function(h = 0) {
  assert_numeric(h)
  data_hline = function(settings, ...) {
    list2env(settings[c("lwd", "lty", "col", "datapoints")], environment())

    if (nrow(datapoints) == 0) {
      msg = "`type_hline() only works on existing plots with x and y data points."
      stop(msg, call. = FALSE)
    }
    # keep track of unique lty and lwd (needed for group catch / escape hatch
    # later in draw_hline)
    ul_lwd = length(unique(lwd))
    ul_lty = length(unique(lty))
    ul_col = length(unique(col))
    update_settings(settings, type_info = list(ul_lty = ul_lty, ul_lwd = ul_lwd, ul_col = ul_col))
  }
  draw_hline = function() {
    fun = function(ifacet, iby, data_facet, icol, ilty, ilwd,
                   ngrps, nfacets, by_continuous, facet_by,
                   type_info,
                   ...) {
      # flag for aesthetics by groups
      grp_aes = type_info[["ul_col"]] == 1 || type_info[["ul_lty"]] == ngrps || type_info[["ul_lwd"]] == ngrps

      if (length(h) != 1) {
        if (!length(h) %in% c(ngrps, nfacets, ngrps * nfacets)) {
          msg = "Length of 'h' must be 1, or equal to the number of facets or number of groups (or product thereof)."
          stop(msg, call. = FALSE)
        }
        if (!facet_by && length(h) == nfacets) {
          h = h[ifacet]
          if (!grp_aes && type_info[["ul_col"]] != ngrps) {
            icol = 1
          } else if (by_continuous) {
            icol = 1
          }
        } else if (!by_continuous && length(h) == ngrps * nfacets) {
          h = h[ifacet * ngrps - c(ngrps - iby)]
        } else if (!by_continuous) {
          h = h[iby]
        }
      } else if (!grp_aes) {
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
