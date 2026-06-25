#' tinyplot Method for Plotting Matrices
#'
#' @description Convenience interface for visualizing
#'   \code{\link[base]{matrix}} objects with tinyplot.
#'
#' @details Internally the matrix is converted to long form and visualized as a
#'   scatter (or other `type`) of each column's values against their row index.
#'   Each column is mapped to a separate group (the `by` aesthetic), so a matrix
#'   with multiple columns produces a grouped plot, optionally faceted by column
#'   via `facet = "by"`. This mirrors the base R \code{\link[graphics]{matplot}}
#'   convention of plotting the columns of a matrix against the row numbers.
#'
#'   If the matrix has column names, these are used as the group (and legend)
#'   labels. Single-column matrices are drawn as a simple index plot with no
#'   grouping or legend.
#'
#' @param x an object of class `"matrix"`.
#' @param legend specification passed on to `tinyplot`. The default is to draw a
#'   legend (titled by the matrix name) when the matrix has named columns, and
#'   to suppress it otherwise.
#' @param facet specification of `facet` passed on to `tinyplot`. The only
#'   accepted (non-`NULL`) value is the `"by"` convenience string, which facets
#'   the plot by matrix column.
#' @param xlab,ylab axis labels passed on to `tinyplot`. These default to
#'   `"Index"` (the row number) and the deparsed matrix name, respectively.
#' @param ... further arguments passed to `tinyplot`.
#'
#' @returns No return value, called for the side effect of producing a plot.
#'
#' @seealso \code{\link{tinyplot}}, \code{\link[graphics]{matplot}}
#'
#' @examples
#' # basic use
#' iris_mat = as.matrix(iris[1:50, 1:4])
#' tinyplot(iris_mat)
#' tinyplot(iris_mat, legend = "direct", theme = "socviz")
#' tinyplot(iris_mat, legend = FALSE, facet = "by", theme = "socviz")
#' 
#' # equivalent example to one in `?matplot`
#' sines = outer(1:20, 1:4, function(x, y) sin(x / 20 * pi * y))
#' tinplot(sines, type = "o", pch = "by", lty = "by", col = rainbow(ncol(sines)))
#'
#' @export
tinyplot.matrix = function(x, legend = NULL, facet = NULL, xlab = NULL, ylab = NULL, ...) {
  assert_choice(facet, "by", null.ok = TRUE)
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
  dim(x) = dims[1] * dims[2]
  y = x
  x = rep(seq_len(dims[1]), times = dims[2])
  if (is.null(xlab)) xlab = "Index"
  if (is.null(ylab)) ylab = dep_x
  tinyplot.default(
    x = x, y = y,
    by = bby,
    facet = facet,
    legend = legend,
    xlab = xlab,
    ylab = ylab,
    ...
  )
}
