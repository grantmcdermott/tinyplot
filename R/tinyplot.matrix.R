#' tinyplot Method for Plotting Matrices
#'
#' @description Convenience interface for visualizing
#'   \code{\link[base]{matrix}} objects with tinyplot.
#'
#' @details Internally the matrix is converted to long form and visualized as a
#'   scatter (or other `type`) of each column's values against their row index.
#'   Each column is mapped to a separate `by` category, so a matrix with
#'   multiple columns produces a grouped plot. Optionally, it can also be
#'   faceted via `facet = "by"`. This mirrors the base R
#'   \code{\link[graphics]{matplot}} convention of plotting the columns of a
#'   matrix against the row numbers. If the matrix has column names, these are
#'   used as the group (and legend) labels. Single-column matrices are drawn as
#'   a simple index plot with no grouping or legend.
#'
#' @param x an object of class `"matrix"`.
#' @param type plot type passed on to `tinyplot`. Defaults to `"p"` (points).
#' @param legend specification passed on to `tinyplot`. The default is to draw a
#'   legend when the matrix has named columns, and to suppress it otherwise.
#' @param facet specification of `facet` passed on to `tinyplot`. The only
#'   accepted non-`NULL` value is the `"by"` convenience string, which facets
#'   the plot by matrix column.
#' @param xlab,ylab axis labels passed on to `tinyplot`. `ylab` defaults to the
#'   deparsed matrix name. `xlab` defaults to `"Index"` when the matrix has no
#'   row names; when it does, the row names already label the ticks so the
#'   x-axis title is suppressed.
#' @param ... further arguments passed to `tinyplot`.
#'
#' @returns No return value, called for the side effect of producing a plot.
#'
#' @seealso \code{\link[graphics]{matplot}}
#'
#' @examples
#' # basic use
#' tinyplot(VADeaths)
#' tinyplot(VADeaths, type = "b")
#' tinyplot(VADeaths, type = "b", legend = "direct", theme = "socviz")
#' tinyplot(VADeaths, type = "b", legend = FALSE, facet = "by", theme = "socviz")
#' 
#' # equivalent plot to an example in `?matplot`
#' sines = outer(1:20, 1:4, function(x, y) sin(x / 20 * pi * y))
#' tinyplot(sines, type = "o", pch = "by", lty = "by", col = rainbow(ncol(sines)))
#'
#' @export
tinyplot.matrix = function(x, type = NULL, legend = NULL, facet = NULL, xlab = NULL, ylab = NULL, ...) {
  assert_choice(facet, "by", null.ok = TRUE)
  ## Default to points. We set this explicitly (rather than relying on
  ## tinyplot's auto-inference) because the x-axis row labels are passed as a
  ## factor, which would otherwise be inferred as a boxplot.
  if (is.null(type)) type = "p"
  dep_x = deparse1(substitute(x))
  dims = dim(x)
  if (dims[2] == 1L) {
    bby = NULL
    legend = FALSE
  } else {
    nms = colnames(x)
    if (!is.null(nms)) {
      bby = factor(rep(nms, each = dims[1]), levels = nms)
      if (is.null(legend)) legend = list(title = NULL)
    } else {
      bby = factor(rep(seq_len(dims[2]), each = dims[1]))
      legend = FALSE
    }
  }
  ## If the matrix has row names, use them for the x-axis tick labels via an
  ## ordered factor (preserving row order). Otherwise fall back to a plain
  ## numeric index.
  rnms = rownames(x)
  dim(x) = dims[1] * dims[2]
  y = x
  if (is.null(rnms)) {
    x = rep(seq_len(dims[1]), times = dims[2])
    ## no row names: x is a plain numeric index, so label it as such
    if (is.null(xlab)) xlab = "Index"
  } else {
    x = factor(rep(rnms, times = dims[2]), levels = rnms, ordered = TRUE)
    ## row names already label the ticks, so an "Index" title is redundant
    if (is.null(xlab)) xlab = NA
  }
  if (is.null(ylab)) ylab = dep_x
  tinyplot.default(
    x = x, y = y,
    type = type,
    by = bby,
    facet = facet,
    legend = legend,
    xlab = xlab,
    ylab = ylab,
    ...
  )
}
