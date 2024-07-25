# Facet layout structure
#
# This function is called by `tinyplot`. Given some inputs, it returns 
# information about the layout of the facets.
#
facet_layout = function(facet, add = FALSE, facet.args = list()) {
  nfacet_rows = 1
  nfacet_cols = 1
  if (!is.null(facet)) {
    facets = sort(unique(facet))
    ifacet = seq_along(facets)
    nfacets = length(facets)
    
    if (isTRUE(add)) {
      omfrow = par("mfrow")
      nfacet_rows = omfrow[1]
      nfacet_cols = omfrow[2]
    } else {
      if (!is.null(facet.args[["nrow"]])) {
        nfacet_rows = facet.args[["nrow"]]
        nfacet_cols = ceiling(nfacets / nfacet_rows)
      } else if (!is.null(facet.args[["ncol"]])) {
        nfacet_cols = facet.args[["ncol"]]
        nfacet_rows = ceiling(nfacets / nfacet_cols)
      } else {
        if (nfacets > 3) {
          nfacet_cols = ceiling(sqrt(nfacets))
          nfacet_rows = ceiling(nfacets / nfacet_cols)
        } else {
          nfacet_rows = 1L
          nfacet_cols = nfacets
        }
      }
    }

    oxaxis = tail(ifacet, nfacet_cols)
    oyaxis = seq(1, nfacets, by = nfacet_cols)

    if (nfacet_rows >= 3 || nfacet_cols >= 3) {
      cex_fct_adj = 0.66
    } else if (nfacet_rows == 2 && nfacet_cols == 2) {
      cex_fct_adj = 0.83
    } else {
      cex_fct_adj = 1
    }
  } else {
    facets = ifacet = nfacets = oxaxis = oyaxis = 1
    cex_fct_adj = 1
  }
  
  list(
    facets = facets,
    ifacet = ifacet,
    nfacets = nfacets,
    nfacet_rows = nfacet_rows,
    nfacet_cols = nfacet_cols,
    oxaxis = oxaxis,
    oyaxis = oyaxis,
    cex_fct_adj = cex_fct_adj
  )
}
