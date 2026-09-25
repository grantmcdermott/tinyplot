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
#'   The `"tile"` and `"heatmap"` types are an exception, since the matplot
#'   convention makes little sense for them. Instead the matrix is laid out as a
#'   grid---columns along the x-axis, rows along the y-axis---with the matrix
#'   *values* supplied as the fill. The y-axis is reversed so that the first row
#'   sits at the top, matching how one reads a matrix (cf.
#'   \code{\link[stats]{heatmap}} and \code{\link[graphics]{image}}); pass an
#'   explicit `ylim` to override. Both axis
#'   titles are suppressed, since the dimnames already label the ticks, and so
#'   is the legend, since the fill merely re-encodes the matrix's own values.
#'   Pass an explicit `legend` (or `xlab`/`ylab`) to override either. See
#'   Examples.
#'
#' @param x an object of class `"matrix"`.
#' @param type plot type passed on to `tinyplot`. Defaults to `"p"` (points).
#' @param legend specification passed on to `tinyplot`. The default is to draw a
#'   legend when the matrix has named columns, and to suppress it otherwise. For
#'   `"tile"` and `"heatmap"` types it is suppressed by default.
#' @param facet specification of `facet` passed on to `tinyplot`. The only
#'   accepted non-`NULL` value is the `"by"` convenience string, which facets
#'   the plot by matrix column.
#' @param xlab,ylab axis labels passed on to `tinyplot`. `ylab` defaults to the
#'   deparsed matrix name. `xlab` defaults to `"Index"` when the matrix has no
#'   row names; when it does, the row names already label the ticks so the
#'   x-axis title is suppressed. For `"tile"` and `"heatmap"` types both
#'   titles default to `NA`, since the dimnames label both axes.
#' @param ... further arguments passed to `tinyplot`.
#'
#' @inherit tinyplot return
#'
#' @seealso \code{\link{tinyplot.array}}, \code{\link[graphics]{matplot}}
#'
#' @examples
#' # basic use
#' tinyplot(VADeaths)
#' tinyplot(VADeaths, type = "b")
#' tinyplot(VADeaths, type = "b", legend = "direct", theme = "socviz")
#' tinyplot(VADeaths, type = "b", legend = FALSE, facet = "by", theme = "socviz")
#'
#' # digression: equivalent "o" plot to an example in `?matplot`
#' sines = outer(1:20, 1:4, function(x, y) sin(x / 20 * pi * y))
#' tinyplot(sines, type = "o", pch = "by", lty = "by", col = rainbow(ncol(sines)))
#' 
#' # back to VADeaths running example, we can pass down other types too...
#' 
#' # heatmap
#' tinyplot(VADeaths, type = "heatmap", theme = "heatmap", col = "white")
#'
#' # barplot(s)
#' tinyplot(VADeaths, type = "barplot", beside = TRUE)
#' tinyplot(t(VADeaths), type = "barplot", beside = TRUE)
#' tinyplot(VADeaths, type = "barplot", facet = "by", legend = FALSE)
#' 
#' @export
tinyplot.matrix = function(x, type = NULL, legend = NULL, facet = NULL, xlab = NULL, ylab = NULL, ...) {
  assert_choice(facet, "by", null.ok = TRUE)
  array_plot(
    x, type = type, legend = legend, facet = facet, xlab = xlab, ylab = ylab,
    dep = deparse1(substitute(x)), ...
  )
}


## Internal workhorse shared by the matrix and array methods. Converts an array
## with 2-4 dimensions to long form and passes it on to tinyplot.default(). The
## first two dimensions follow the matrix conventions
## documented in ?tinyplot.matrix; dimensions 3 and 4 (if any) are mapped to
## facets, as a wrap and a grid, respectively.
array_plot = function(x, type = NULL, legend = NULL, facet = NULL,
                      xlab = NULL, ylab = NULL, ylim = NULL, dep = NULL, ...) {
  ## Default to points. We set this explicitly (rather than relying on
  ## tinyplot's auto-inference) because the x-axis row labels are passed as a
  ## factor, which would otherwise be inferred as a boxplot.
  if (is.null(type)) type = "p"
  dims = dim(x)
  dnms = dimnames(x)
  ## names(dimnames(x)), if any, e.g. for arrays built from a table
  dvars = names(dnms)
  dvar = function(k) {
    v = dvars[k]
    if (is.null(v) || is.na(v) || !nzchar(v)) NULL else v
  }
  ## position of each value along dimension k, as a factor labelled by the
  ## dimnames (if any)
  dim_factor = function(k, ordered = FALSE) {
    i = as.vector(slice.index(x, k))
    lvls = dnms[[k]]
    if (is.null(lvls)) {
      factor(i, levels = seq_len(dims[k]), ordered = ordered)
    } else {
      factor(lvls[i], levels = lvls, ordered = ordered)
    }
  }

  ## Tile and heatmap types need a different mapping to the matplot convention
  ## below: they want the matrix laid out as a grid (columns on x, rows on y)
  ## with the *values* supplied as the fill, rather than a series per column
  ## with the values on y. Detect via the resolved type name, so that both the
  ## convenience strings and the type_*() constructors are covered.
  tname = if (inherits(type, "tinyplot_type")) type[["name"]] else type
  if (is.character(tname) && length(tname) == 1L &&
      tname %in% c("tile", "heatmap")) {
    ## Note the row levels are *not* reversed here. Row 1 belongs at the *top*
    ## of a matrix display (cf. `heatmap()`, `image()`), but we get that by
    ## defaulting the y-axis to reversed below, which keeps it overridable via
    ## `ylim`. Reversing the levels *and* the axis would cancel out.
    xx = dim_factor(2)
    yy = dim_factor(1)
    by = as.vector(x)
    ## Both axes are labelled by the matrix dimnames, so axis titles would be
    ## redundant. Ditto the legend: the fill encodes the matrix's own values, so
    ## a colourbar adds little for a bare `tinyplot(m, type = "heatmap")` call.
    ## Users who want one can still ask for it explicitly.
    if (is.null(xlab)) xlab = dvar(2) %||% NA
    if (is.null(ylab)) ylab = dvar(1) %||% NA
    if (is.null(legend)) legend = FALSE
    ## Applies to "tile" as well as "heatmap": the matrix *layout* is what
    ## implies the orientation here, not the choice of type. (type_heatmap()
    ## additionally defaults to this on its own, for the formula method; the two
    ## are idempotent and so compose safely.)
    if (is.null(ylim)) ylim = "reverse"
  } else {
    if (dims[2] == 1L) {
      ## a single column is a simple index plot, so there is nothing to group
      ## (or facet) by
      by = NULL
      legend = FALSE
      if (identical(facet, "by")) facet = NULL
    } else {
      by = dim_factor(2)
      if (is.null(dnms[[2]])) {
        legend = FALSE
      } else if (is.null(legend)) {
        legend = list(title = dvar(2))
      }
    }
    ## If the matrix has row names, use them for the x-axis tick labels via an
    ## ordered factor (preserving row order). Otherwise fall back to a plain
    ## numeric index.
    if (is.null(dnms[[1]])) {
      xx = as.vector(slice.index(x, 1))
      ## no row names: x is a plain numeric index, so label it as such
      if (is.null(xlab)) xlab = dvar(1) %||% "Index"
    } else {
      xx = dim_factor(1, ordered = TRUE)
      ## row names already label the ticks, so an "Index" title is redundant
      if (is.null(xlab)) xlab = dvar(1) %||% NA
    }
    yy = as.vector(x)
    if (is.null(ylab)) ylab = dep
  }

  ## Higher dimensions become facets: the 3rd dimension as a wrap, or (with a
  ## 4th) as the rows of a grid whose columns are the 4th dimension, i.e. the
  ## same as a `dim3 ~ dim4` facet formula.
  if (length(dims) > 2L) {
    fvars = function(k, f) facet_var_list(f, dvar(k) %||% paste0("dim", k))
    f3 = dim_factor(3)
    if (length(dims) == 3L) {
      facet = f3
      attr(facet, "facet_vars") = list(x = fvars(3, f3))
    } else {
      f4 = dim_factor(4)
      facet = facet_grid_factor(f4, f3, fvars(4, f4), fvars(3, f3))
    }
  }

  tinyplot.default(
    x = xx, y = yy,
    type = type,
    by = by,
    facet = facet,
    legend = legend,
    xlab = xlab,
    ylab = ylab,
    ylim = ylim,
    ...
  )
}
