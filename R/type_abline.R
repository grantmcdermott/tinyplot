#' Add straight lines to a plot
#' @description
#' These functions add straight line(s) through the current plot.
#' @details
#' Unlike most tinyplot types, `type_abline`, `type_hline`, and `type_vline`
#' cannot be called as a base plot layer. Instead they *must* called as a
#' subsequent layer via [`tinyplot_add`].
#' 
#' @param a,b the intercept (default: `a` = 0) and slope (default: `b` = 1)
#'   terms. Numerics of length 1 or equal to the number of facets.
#' @examples
#' #
#' ## abline
#' 
#' tinyplot(x = -10:10, y = rnorm(21) + -10:10, grid = TRUE)
#' tinyplot_add(type = "abline")
#' # same as...
#' # tinyplot_add(type = type_abline(a = 0, b = 1))
#' 
#' # customize by passing bespoke intercept and slope values
#' tinyplot_add(type = type_abline(a = -1, b = -0.5))
#' 
#' #
#' ## hline and vline
#'
#' # Base plot layer
#' tinyplot(mpg ~ hp | cyl, facet = "by", data = mtcars, ylim = c(0, 40))
#' 
#' # Add horizontal lines at the (default) 0 y-intercept
#' tinyplot_add(type = "hline", col = "grey")
#' 
#' # Note that group+facet aesthetics will be inherited. We can use this to
#' # add customized lines (here: the mean `mpg` for each `cyl` group)  
#' tinyplot_add(type = type_hline(with(mtcars, tapply(mpg, cyl, mean))), lty = 2)
#' 
#' # Similar idea for vline
#' tinyplot_add(type = type_vline(with(mtcars, tapply(hp, cyl, mean))), lty = 2)
#' 
#' @export
type_abline = function(a = 0, b = 1) {
  data_abline = function(datapoints, ...) {
    if (nrow(datapoints) == 0) {
      msg = "`type_abline() only works on existing plots with x and y data points."
      stop(msg, call. = FALSE)
    }
    return(list())
  }
  draw_abline = function() {
    fun = function(ifacet, data_facet, icol, ilty, ilwd, ngrps, nfacets, by_continuous, ...) {

      if (length(a) != 1) {
        if (!length(a) %in% c(ngrps, nfacets)) {
          msg = "Length of 'a' must be 1, or equal to the number of facets or number of groups."
          stop(msg, call. = FALSE)
        }
        a = if (length(a) == nfacets) a[ifacet] else a[iby]
      }
      
      if (length(b) != 1) {
        if (!length(b) %in% c(ngrps, nfacets)) {
          msg = "Length of 'b' must be 1, or equal to the number of facets or number of groups."
          stop(msg, call. = FALSE)
        }
        b = if (length(b) == nfacets) b[ifacet] else b[iby]
      }
      
      if (by_continuous && (length(a)==1 || length(b)==1)) icol = 1

      abline(a = a, b = b, col = icol, lty = ilty, lwd = ilwd)
    }
    return(fun)
  }
  out = list(
    draw = draw_abline(),
    data = data_abline,
    name = "abline"
  )
  class(out) = "tinyplot_type"
  return(out)
}
