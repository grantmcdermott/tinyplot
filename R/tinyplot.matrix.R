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
## first two dimensions follow the matrix conventions documented in
## ?tinyplot.matrix, i.e. x-axis and `by`, and any further dimensions are mapped
## to facets, as a wrap and a grid, respectively. If `y` is supplied (an array
## of the same shape), then `x` and `y` hold x/y pairs instead. Each dimension
## then shifts down a role: the 1st becomes `by`, drawn along each path, and the
## 2nd and 3rd (if any) the facets.
array_plot = function(x, y = NULL, type = NULL, legend = NULL, facet = NULL,
                      xlab = NULL, ylab = NULL, ylim = NULL, dep = NULL, ...) {
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
  ## the dimensions that become facets
  fdims = seq_along(dims)[-seq_len(if (is.null(y)) 2L else 1L)]
  ## facet = FALSE folds these into the `by` groups instead
  fold = isFALSE(facet) && length(fdims) > 0L
  if (isFALSE(facet)) facet = NULL
  ## `by` groups (and legend title) spanning one or more dimensions
  group_by = function(ks) {
    if (length(ks) == 1L) return(dim_factor(ks))
    interaction(lapply(ks, dim_factor), sep = ":", lex.order = TRUE)
  }
  group_title = function(ks) {
    v = unlist(lapply(ks, dvar))
    if (length(v) == length(ks)) paste(v, collapse = ":")
  }

  ## x/y pairs aside, tile and heatmap types need a different mapping to the
  ## matplot convention below: they want the matrix laid out as a grid
  ## (columns on x, rows on y) with the *values* supplied as the fill, rather
  ## than a series per column with the values on y. Detect via the resolved
  ## type name, so that both the convenience strings and the type_*()
  ## constructors are covered.
  if (!is.null(y)) {
    xx = as.vector(x)
    yy = as.vector(y)
    if (fold) {
      ## Without facets, each path needs its own (discrete) `by` group, so
      ## that the 1st dimension merely orders the points along it.
      by = group_by(fdims)
      if (is.null(type)) type = "l"
      if (is.null(legend)) legend = list(title = group_title(fdims))
    } else {
      ## The 1st dimension orders the points along each path, e.g. time, so we
      ## keep it numeric where possible (the index, or numeric dimnames). That
      ## way `by` is continuous and the path is drawn as a single colour
      ## gradient by type "l". Otherwise, fall back to discrete groups and
      ## points.
      lvls = dnms[[1]]
      lvls = if (is.null(lvls)) {
        seq_len(dims[1])
      } else {
        type.convert(lvls, as.is = TRUE)
      }
      by = if (is.numeric(lvls)) {
        lvls[as.vector(slice.index(x, 1))]
      } else {
        dim_factor(1, ordered = TRUE)
      }
      if (is.null(type)) type = if (is.numeric(by)) "l" else "p"
      if (is.null(legend)) legend = list(title = dvar(1))
    }
  } else if (is_grid_type(type)) {
    if (fold) {
      stop(
        "`facet = FALSE` is not supported for \"tile\" or \"heatmap\" types.",
        call. = FALSE
      )
    }
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
    ## Default to points. We set this explicitly (rather than relying on
    ## tinyplot's auto-inference) because the x-axis row labels are passed as
    ## a factor, which would otherwise be inferred as a boxplot.
    if (is.null(type)) type = "p"
    ## group by the columns (unless there is just one) and any folded facets
    bdims = c(if (dims[2] > 1L) 2L, if (fold) fdims)
    if (!length(bdims)) {
      ## a single column is a simple index plot, so there is nothing to group
      ## (or facet) by
      by = NULL
      legend = FALSE
      if (identical(facet, "by")) facet = NULL
    } else {
      by = group_by(bdims)
      if (all(vapply(dnms[bdims], is.null, NA))) {
        legend = FALSE
      } else if (is.null(legend)) {
        legend = list(title = group_title(bdims))
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

  ## The remaining dimensions become facets: the first as a wrap, or (with a
  ## second) as the rows of a grid whose columns are the second, i.e. the same
  ## as a `rows ~ cols` facet formula.
  if (length(fdims) && !fold) {
    fvars = function(k, f) facet_var_list(f, dvar(k) %||% paste0("dim", k))
    fr = dim_factor(fdims[1])
    if (length(fdims) == 1L) {
      facet = fr
      attr(facet, "facet_vars") = list(x = fvars(fdims[1], fr))
    } else {
      fc = dim_factor(fdims[2])
      facet = facet_grid_factor(
        fc, fr, fvars(fdims[2], fc), fvars(fdims[1], fr)
      )
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


## Tile and heatmap types, which lay out a matrix (slice) as a grid
is_grid_type = function(type) {
  tname = if (inherits(type, "tinyplot_type")) type[["name"]] else type
  is.character(tname) && length(tname) == 1L && tname %in% c("tile", "heatmap")
}
