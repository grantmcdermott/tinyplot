#' tinyplot Method for Plotting Arrays
#'
#' @description Convenience interface for visualizing
#'   \code{\link[base]{array}} objects with tinyplot. Extends the
#'   \code{\link{tinyplot.matrix}} conventions to arrays with up to four
#'   dimensions, by mapping the higher dimensions to facets.
#'
#' @details The first two dimensions of the array are treated exactly like the
#'   rows and columns of a matrix; see \code{\link{tinyplot.matrix}}. By default,
#'   that means the values are plotted against the (first dimension) row index,
#'   with a separate `by` group for each element of the second dimension.
#'   Higher dimensions are then mapped to facets:
#'
#'   - 1D arrays are treated as a single-column matrix, i.e. a simple index
#'   plot. If a `y` variable is also supplied, e.g. `tinyplot(tapply(...), y)`,
#'   then the array is instead treated as a plain `x` vector.
#'   - 2D arrays are matrices, and hence dispatch to
#'   \code{\link{tinyplot.matrix}}.
#'   - 3D arrays are faceted by the third dimension (i.e., a "facet wrap").
#'   - 4D arrays are faceted by the third and fourth dimensions, as the rows
#'   and columns of a "facet grid", respectively.
#'   - Arrays with more than four dimensions are not supported. Subset or
#'   reshape them first.
#'
#'   Dimension names are used to label the groups, facets, and axis ticks, while
#'   the names of the dimnames (if any) are used as the axis, legend, and facet
#'   titles.
#'
#'   Note that contingency tables created by \code{\link[base]{table}} (e.g.,
#'   `HairEyeColor` or `Titanic`) are of class `"table"` and hence do not
#'   dispatch to this method.
#'
#' @inheritParams tinyplot.matrix
#' @param x an object of class `"array"`.
#' @param facet must be `NULL` for arrays with three or more dimensions, since
#'   the facets are then determined by the array dimensions.
#' @param ... further arguments passed to `tinyplot`.
#'
#' @inherit tinyplot return
#'
#' @seealso \code{\link{tinyplot.matrix}}, \code{\link[graphics]{matplot}}
#'
#' @examples
#' # 3D array: facet wrap by the third dimension
#' sims = array(
#'   cumsum(rnorm(20 * 3 * 4)), dim = c(20, 3, 4),
#'   dimnames = list(NULL, paste("Series", 1:3), paste("Run", 1:4))
#' )
#' tinyplot(sims, type = "l")
#'
#' # 4D array: facet grid by the third and fourth dimensions
#' sims4 = array(
#'   rnorm(20 * 3 * 2 * 2), dim = c(20, 3, 2, 2),
#'   dimnames = list(
#'     time = NULL, series = paste0("s", 1:3),
#'     model = c("A", "B"), scenario = c("low", "high")
#'   )
#' )
#' tinyplot(sims4, type = "b", facet.args = list(prefix = TRUE))
#'
#' # tile/heatmap types lay out each 2D slice as a grid
#' tinyplot(sims4[1:5, , , ], type = "heatmap", theme = "heatmap")
#'
#' @export
tinyplot.array = function(x, type = NULL, legend = NULL, facet = NULL, xlab = NULL, ylab = NULL, ...) {
  dep = deparse1(substitute(x))
  nd = length(dim(x))
  if (nd > 4L) {
    stop(
      "Arrays with more than 4 dimensions are not supported. ",
      "Please subset or reshape your array first.",
      call. = FALSE
    )
  }
  if (nd <= 2L) {
    assert_choice(facet, "by", null.ok = TRUE)
  } else if (!is.null(facet)) {
    stop(
      "`facet` must be NULL for arrays with 3 or more dimensions, since the ",
      "facets are determined by the array dimensions.",
      call. = FALSE
    )
  }
  if (nd == 1L) {
    ## A 1D array (e.g. from tapply()) passed alongside a `y` variable is just
    ## an x vector, so hand the call on to the default method. We check the
    ## unmatched call, since a positional `y` would otherwise be matched to
    ## one of this method's formals (`type`, `legend`, etc.).
    cl = sys.call()
    nms = names(cl) %||% character(length(cl))
    if ("y" %in% nms || (length(cl) > 2L && !nzchar(nms[3L]))) {
      cl[[1L]] = tinyplot.default
      return(eval(cl, parent.frame()))
    }
    ## Otherwise, it is treated as a single-column matrix
    dn = dimnames(x)
    dim(x) = c(length(x), 1L)
    if (!is.null(dn)) dimnames(x) = c(dn, list(NULL))
  }
  array_plot(
    x, type = type, legend = legend, facet = facet, xlab = xlab, ylab = ylab,
    dep = dep, ...
  )
}
