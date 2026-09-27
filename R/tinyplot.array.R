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
#'   titles. To assign the dimensions to different roles, rearrange them first
#'   with \code{\link[base]{aperm}}.
#'
#'   **x/y pairs.** Arrays often hold a stack of matrices, one per variable,
#'   e.g. the hip and knee angles of the `gait` data (`Time` x `Subject` x
#'   `Variable`). If an array of three or more dimensions has exactly one
#'   length-2 dimension besides the first, then its two slices are plotted
#'   against each other, as x and y. Each of the remaining dimensions then
#'   shifts down a role:
#'
#'   - The first dimension becomes `by`, and orders the points along each path.
#'   It is kept numeric where possible (its dimnames, e.g. times, or else its
#'   index), so that each path is drawn as a line with a colour gradient. If
#'   the dimnames are not numeric, then the groups are discrete and drawn as
#'   points instead.
#'   - The second dimension becomes a facet wrap (e.g. one panel per subject).
#'   - The third dimension (if any) makes this a facet grid, as its columns.
#'
#'   Control this via the `xy` argument. Tile and heatmap types are exempt,
#'   since they need the array values as their fill.
#'
#'   Note that contingency tables created by \code{\link[base]{table}} (e.g.,
#'   `HairEyeColor` or `Titanic`) are of class `"table"` and hence do not
#'   dispatch to this method.
#'
#' @inheritParams tinyplot.matrix
#' @param x an object of class `"array"`.
#' @param facet must be `NULL` for arrays with three or more dimensions, since
#'   the facets are then determined by the array dimensions.
#' @param xy which dimension (if any) holds x/y pairs; see Details. The default
#'   `NULL` auto-detects it, `FALSE` opts out, and a dimension index or name
#'   selects it explicitly. The selected dimension must have length 2, and its
#'   dimnames (if any) are used as the axis titles.
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
#' # x/y pairs: a length-2 dimension is plotted as x vs y
#' if (getRversion() >= "4.5.0") {
#'   # hip vs knee angle through the gait cycle, coloured by time and
#'   # faceted by boy
#'   tinyplot(gait[, 1:9, ])
#'   # opt out, to plot the angles against time instead
#'   tinyplot(gait[, 1:9, ], type = "l", xy = FALSE, legend = FALSE)
#' }
#'
#' @export
tinyplot.array = function(x, type = NULL, legend = NULL, facet = NULL, xlab = NULL, ylab = NULL, xy = NULL, ...) {
  dep = deparse1(substitute(x))
  nd = length(dim(x))
  if (nd > 4L) {
    stop(
      "Arrays with more than 4 dimensions are not supported. ",
      "Please subset or reshape your array first.",
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

  ## Split off a length-2 dimension holding x/y pairs (if any), leaving two
  ## arrays of one dimension less: the x values, and the matching y values
  y = NULL
  k = array_xy_dim(x, xy = xy, type = type)
  if (!is.null(k)) {
    si = slice.index(x, k)
    xlvls = dimnames(x)[[k]]
    if (is.null(xlvls)) {
      ## e.g. "arr[, , 1]" and "arr[, , 2]"
      xlvls = vapply(1:2, function(i) {
        idx = character(nd)
        idx[k] = i
        paste0(dep, "[", paste(idx, collapse = ", "), "]")
      }, character(1))
    }
    if (is.null(xlab)) xlab = xlvls[1]
    if (is.null(ylab)) ylab = xlvls[2]
    y = array(x[si == 2L], dim(x)[-k], dimnames(x)[-k])
    x = array(x[si == 1L], dim(x)[-k], dimnames(x)[-k])
  }

  if (is.null(k) && nd <= 2L) {
    assert_choice(facet, "by", null.ok = TRUE)
  } else if (!is.null(facet)) {
    stop(
      "`facet` must be NULL for arrays with 3 or more dimensions (or x/y ",
      "pairs), since the facets are determined by the array dimensions.",
      call. = FALSE
    )
  }
  array_plot(
    x, y = y, type = type, legend = legend, facet = facet,
    xlab = xlab, ylab = ylab, dep = dep, ...
  )
}


## Which dimension of an array (if any) holds x/y pairs. Auto-detected as the
## only length-2 dimension besides the first (which orders the paths), unless
## the user sets `xy` explicitly: a dimension index or name, or FALSE to opt
## out. Tile and heatmap types are skipped, since they need the array values
## as their fill, leaving nothing to split into x and y.
array_xy_dim = function(x, xy = NULL, type = NULL) {
  if (isFALSE(xy)) return(NULL)
  dims = dim(x)
  grid_type = is_grid_type(type)
  if (is.null(xy)) {
    k = setdiff(which(dims == 2L), 1L)
    if (length(dims) < 3L || length(k) != 1L || grid_type) return(NULL)
    return(k)
  }
  k = if (is.character(xy)) match(xy, names(dimnames(x))) else xy
  if (length(k) != 1L || is.na(k) || !k %in% seq_along(dims)) {
    stop(
      "`xy` must be FALSE, or a single dimension index or name.",
      call. = FALSE
    )
  }
  if (dims[k] != 2L) {
    stop("The `xy` dimension must have length 2.", call. = FALSE)
  }
  if (grid_type) {
    stop(
      "`xy` is not supported for \"tile\" or \"heatmap\" types, since they ",
      "need the array values as their fill.",
      call. = FALSE
    )
  }
  k
}
