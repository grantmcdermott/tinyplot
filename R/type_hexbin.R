#' Hexagonal binning plot type
#'
#' @description Type function for hexagonal bin plots (a 2D analogue of a
#'   histogram). Point density is aggregated into a regular hexagonal lattice
#'   and each occupied cell is drawn as a hexagon whose fill colour encodes the
#'   number of points it contains. This is a lightweight (vectorized), pure R
#'   implementation of the canonical hexagonal binning algorithm of Carr et al.
#'   (1987).
#'
#' @details Hexagonal binning partitions the plotting region into a regular
#'   lattice of hexagons. Following the original Carr et al. (1987) algorithm, 
#'   rows are spaced \eqn{\sqrt{3}/2} apart in scaled space, with odd rows
#'   offset by half a cell in the x-direction, while each point is assigned to
#'   its nearest lattice node.
#'
#'   What the fill colour encodes depends on whether a `by` variable is supplied
#'   (e.g. `y ~ x | z`):
#'
#'   * **No `by` (default).** The fill encodes the *cell count*, mapped to a
#'     continuous colour gradient with an automatic colourbar legend.
#'   * **Discrete `by`.** The fill encodes the *modal* (most frequent) level of
#'     the `by` variable within each cell, drawn with a discrete qualitative
#'     palette and legend. Useful for mapping the dominant category across a
#'     dense scatter.
#'   * **Numeric `by`.** The fill encodes a per-cell summary of the `by`
#'     variable---by default the mean, or any other function passed via `fun`
#'     (e.g. `fun = sum`). Like the original no-`by` case, the legend is drawn
#'     as a continuous gradient. This is the hexagonal analogue of a binned
#'     heatmap.
#'
#'   In all three modes, the `mincnt`/`maxcnt` filters still apply to the raw
#'   cell *count*, so sparsely populated cells can be excluded regardless of
#'   what the colour encodes.
#'
#'   Because the hexagon size is derived from the data ranges, hexagons will
#'   only appear perfectly regular when the plot region's aspect ratio matches
#'   `shape`. Adjust `shape` (or the device dimensions) if you need visually
#'   regular hexagons.
#'
#'   The \CRANpkg{tinyplot} implementation presented here is a compact
#'   (vectorised) translation of the original Carr et al. algorithm into base R.
#'   Users requiring the fuller feature set of a dedicated hexbinning package
#'   should consult \CRANpkg{hexbin} as a canonical reference, which is built on
#'   top of compiled Fortran code. Note that we have validated our cell counts
#'   against this package to confirm they match exactly.
#'
#' @param xbins Integer. The number of bins partitioning the range of the
#'   x-axis. Default is `30`.
#' @param shape Numeric. The aspect ratio (height/width) of the plotting region
#'   used to compute the hexagon geometry. Default is `1`.
#' @param mincnt,maxcnt Integer. Cells with counts outside the
#'   `[mincnt, maxcnt]` range are omitted. Defaults keep all occupied cells.
#' @param border Colour for the hexagon borders, passed to
#'   \code{\link[graphics]{polygon}}. The default `NA` omits the border entirely.
#'   Set a colour (e.g. `"black"`) for outlined hexagons. Passing the sentinel
#'   string `"fill"` matches the border to each cell's fill colour, which
#'   produces perfectly seamless tiling.
#' @param fun Function used to summarise a *numeric* `by` variable within each
#'   cell (see Details). Defaults to `NULL`, which is equivalent to `mean`. Has
#'   no effect when `by` is absent (count mode) or discrete (modal mode).
#'
#' @references
#' Carr, D. B., Littlefield, R. J., Nicholson, W. L., and Littlefield, J. S.
#' (1987). \cite{Scatterplot Matrix Techniques for Large N}. Journal of the
#' American Statistical Association, 82(398), 424-436. Available:
#' https://doi.org/10.2307/2289444
#'
#' Carr, D., Lewin-Koh, N., Maechler, M., and Sarkar, D. (2024).
#' \cite{hexbin: Hexagonal Binning Routines}. R package version 1.28.5.
#' Available: https://doi.org/10.32614/CRAN.package.hexbin. The \CRANpkg{hexbin}
#' package provides the canonical implementation of this algorithm, and was a
#' valuable reference for validating our cell counts.
#'
#' @examples
#' set.seed(1234)
#' dat = data.frame(x = rnorm(20000), y = rnorm(20000))
#'
#' # "hexbin" type convenience string
#' tinyplot(y ~ x, data = dat, type = "hexbin")
#'
#' # Use type_hexbin() to pass extra arguments
#' tinyplot(y ~ x, data = dat, type = type_hexbin(xbins = 40))
#'
#' # tinyplot's default palette logic maps darker colours (the end of the
#' # spectrum) to higher densities. For hexbin plots it can sometimes be more
#' # visually pleasing to reverse this, which users can do manually by passing a
#' # reversed palette.
#' tinyplot(
#'   y ~ x, data = dat, type = "hexbin",
#'   palette = hcl.colors(100, palette = "viridis", rev = TRUE)
#' )
#'
#' # Passing a `by` grouping variable will colour cells according to a summary
#' # of this variable (in each hex cell) instead of density count. The default
#' # summary function depends on whether `by` is discrete or continuous:
#' 
#' # 1) Discrete grouping variable: each cell is coloured by its mode.
#' dat$g = cut(dat$x, breaks = c(-Inf, -1, 1, Inf), labels = c("lo", "mid", "hi"))
#' tinyplot(y ~ x | g, data = dat, type = "hexbin")
#' 
#' # 2) Continuous grouping variable: each cell is coloured by its mean. 
#' #    Example: Create a long version of the `volcano` dataset, and plot its
#' #    elevations onto a gridded terrain map.
#' volc = local({
#'   v = setNames(stack(as.data.frame(volcano)), c("elevation", "y"))
#'   v$y = as.numeric(gsub("^V", "", v$y))
#'   v$x = seq_len(nrow(volcano))
#'   v
#' })
#' tinyplot(
#'   y ~ x | elevation, data = volc,
#'   type = "hexbin", xbins = 50,
#'   palette = terrain.colors(100, rev = TRUE)
#' )
#'
#' @export
type_hexbin = function(xbins = 30, shape = 1, mincnt = 1, maxcnt = Inf,
                       border = NA, fun = NULL) {
  out = list(
    draw = draw_hexbin(border = border),
    data = data_hexbin(xbins = xbins, shape = shape, mincnt = mincnt,
                       maxcnt = maxcnt, fun = fun),
    name = "hexbin"
  )
  class(out) = "tinyplot_type"
  return(out)
}


# Pure base R hexagonal binning, implementing the standard lattice of Carr et
# al. (1987). Cell counts have been validated against hexbin::hexbin() and match
# exactly. Returns a data.frame with one row per occupied cell: cell centre
# (x, y) and count. If a `z` vector is supplied, an additional `stat` column
# holds a per-cell summary of `z`: `fun(z)` when `z` is numeric (default
# `mean`), or the modal (most frequent) level when `z` is discrete.
hexbin_base = function(x, y, xbins, shape, xbnds, ybnds, z = NULL, fun = NULL) {
  ok = is.finite(x) & is.finite(y)
  x = x[ok]; y = y[ok]
  if (!is.null(z)) z = z[ok]
  if (length(x) == 0L) {
    out = data.frame(x = numeric(0), y = numeric(0), count = integer(0))
    if (!is.null(z)) out[["stat"]] = z[0L]
    return(out)
  }

  sx = xbins / diff(xbnds)
  sy = (xbins * shape) / diff(ybnds)
  ry = sqrt(3) / 2                       # row spacing in scaled v-space

  u = (x - xbnds[1L]) * sx
  v = (y - ybnds[1L]) * sy

  # For each point, consider the two lattice rows that straddle it and assign
  # the point to whichever lattice node (accounting for the odd-row x-offset)
  # is closest.
  cand = function(i) {
    off = ifelse(i %% 2 == 0, 0, 0.5)
    j = round(u - off)
    d = (u - (j + off))^2 + (v - i * ry)^2
    list(i = i, j = j, d = d)
  }
  i_lo = floor(v / ry)
  a = cand(i_lo)
  b = cand(i_lo + 1L)
  pick_a = a[["d"]] <= b[["d"]]
  i = ifelse(pick_a, a[["i"]], b[["i"]])
  j = ifelse(pick_a, a[["j"]], b[["j"]])

  # Aggregate cell counts on a purely integer key, avoiding a costly round-trip
  # through character keys (paste/table/strsplit). `i` is always >= 0 and `j`
  # may be negative, so shift `j` to be non-negative and pack both into a single
  # integer per cell. We then recover the (i, j) coordinates by first-occurrence
  # rather than by decoding, so the packing only needs to be collision-free.
  jmin = min(j)
  span = max(j) - jmin + 1L
  key = i * span + (j - jmin)
  cell = match(key, ukey <- unique(key))
  first = match(ukey, key)
  ii = i[first]
  jj = j[first]
  count = tabulate(cell)
  off = ifelse(ii %% 2 == 0, 0, 0.5)

  out = data.frame(
    x = (jj + off) / sx + xbnds[1L],
    y = (ii * ry) / sy + ybnds[1L],
    count = as.integer(count)
  )

  # Optional per-cell summary of `z`.
  if (!is.null(z)) {
    if (is.numeric(z)) {
      # Aggregate `z` within each cell via `fun` (default mean).
      fun = if (is.null(fun)) mean else match.fun(fun)
      agg = tapply(z, cell, fun)
      out[["stat"]] = as.numeric(agg[as.character(seq_len(nrow(out)))])
    } else {
      # Discrete `z`: assign each cell its modal (most frequent) level. Ties are
      # broken by factor-level order (which.max returns the first maximum).
      z = as.factor(z)
      modal = tapply(as.integer(z), cell, function(idx) {
        tab = tabulate(idx, nbins = nlevels(z))
        which.max(tab)
      })
      lvl = modal[as.character(seq_len(nrow(out)))]
      out[["stat"]] = factor(levels(z)[lvl], levels = levels(z))
    }
  }

  out
}


data_hexbin = function(xbins = 30, shape = 1, mincnt = 1, maxcnt = Inf,
                       fun = NULL) {
  hxbins = xbins
  hshape = shape
  hmincnt = mincnt
  hmaxcnt = maxcnt
  hfun = fun

  fun = function(settings, ...) {
    env2env(settings, environment(), c(
      "datapoints", "by", "null_by", "facet", "legend_args", "type_info"
    ))

    # Three fill modes, dispatched on the (optional) `by` variable:
    #   * no `by`         -> colour encodes the cell count (continuous gradient)
    #   * numeric `by`    -> colour encodes `fun(by)` per cell (default mean)
    #   * discrete `by`   -> colour encodes the modal `by` level (discrete)
    # In every mode `mincnt`/`maxcnt` still filter on the raw cell *count*.
    z = if (null_by) NULL else datapoints[["by"]]
    z_mode = !is.null(z)
    z_discrete = z_mode && !is.numeric(z)

    # Common bounds across all facets so cells align. Guard against degenerate
    # (zero-width) ranges -- e.g. a single point, or all-identical x/y -- which
    # would otherwise blow up the lattice scaling to Inf.
    xbnds = range(datapoints[["x"]], finite = TRUE)
    ybnds = range(datapoints[["y"]], finite = TRUE)
    if (!all(is.finite(c(xbnds, ybnds)))) {
      stop("`type_hexbin` requires finite `x` and `y` values.", call. = FALSE)
    }
    if (diff(xbnds) == 0) xbnds = xbnds + c(-0.5, 0.5)
    if (diff(ybnds) == 0) ybnds = ybnds + c(-0.5, 0.5)

    # Hexagon geometry: half-width `dx` and quarter-height `dy` of a pointy-top
    # hexagon in data units, derived from the lattice scaling (Carr et al. 1987).
    sx = hxbins / diff(xbnds)
    sy = (hxbins * hshape) / diff(ybnds)
    inner = 0.5
    outer = (2 * inner) / sqrt(3)
    dx = inner / sx
    dy = outer / (2 * sy)

    # For a discrete `by`, factor it up front (with global levels) so cells
    # across facets share a consistent set of levels when combined below.
    if (z_discrete) datapoints[["by"]] = as.factor(datapoints[["by"]])

    # Bin within each facet, keeping shared bounds.
    has_facet = !is.null(datapoints[["facet"]])
    fac = if (has_facet) datapoints[["facet"]] else rep("", nrow(datapoints))
    parts = split(datapoints, fac)
    parts = Filter(function(k) nrow(k) > 0, parts)

    # Index by position, not by name: the no-facet split key is "" and
    # `parts[[""]]` would return NULL (an easy trap).
    part_names = names(parts)
    binned = lapply(seq_along(parts), function(idx) {
      k = parts[[idx]]
      hb = hexbin_base(k[["x"]], k[["y"]], hxbins, hshape, xbnds, ybnds,
                       z = if (z_mode) k[["by"]] else NULL, fun = hfun)
      if (nrow(hb) == 0L) return(NULL)
      # Recover the facet label from the split key, not from `k` (which carries
      # no `facet` column in the no-facet case).
      hb[["facet"]] = if (has_facet) k[["facet"]][1L] else part_names[idx]
      hb
    })
    binned = do.call(rbind, binned)

    # Count filtering.
    if (!is.null(binned)) {
      keep = binned[["count"]] >= hmincnt & binned[["count"]] <= hmaxcnt
      binned = binned[keep, , drop = FALSE]
    }
    if (is.null(binned) || nrow(binned) == 0L) {
      stop(
        "`type_hexbin` produced no cells to plot. Check the data and the ",
        "`mincnt`/`maxcnt` range.",
        call. = FALSE
      )
    }

    # Choose what the fill encodes, per mode:
    #   * count     -> numeric `by` (triggers by_continuous = TRUE -> gradient)
    #   * numeric z -> the per-cell `fun(z)` summary (also continuous gradient)
    #   * discrete z-> the per-cell modal level (a factor -> discrete legend)
    by = if (z_mode) binned[["stat"]] else binned[["count"]]
    # Drop discrete levels that are nobody's mode, so the legend only lists
    # categories that actually colour a cell.
    if (z_discrete) by = droplevels(by)
    datapoints = data.frame(
      x = binned[["x"]],
      y = binned[["y"]],
      by = by,
      facet = binned[["facet"]]
    )
    null_by = FALSE

    # Extend plot limits to cover the full hexagons (centres +/- half extent),
    # so edge hexagons are not clipped. lim_args() unions x/xmin/xmax.
    datapoints[["xmin"]] = datapoints[["x"]] - dx
    datapoints[["xmax"]] = datapoints[["x"]] + dx
    datapoints[["ymin"]] = datapoints[["y"]] - 2 * dy
    datapoints[["ymax"]] = datapoints[["y"]] + 2 * dy

    # Hexagon vertex offsets passed to the draw function.
    type_info = list(
      hexX = c(dx, dx, 0, -dx, -dx, 0),
      hexY = c(dy, -dy, -2 * dy, -dy, dy, 2 * dy)
    )

    # Legend title. In count mode default to "Count"; in `by`-summary mode leave
    # it unset so the usual default (the deparsed `by` variable name) is used.
    if (!z_mode) legend_args[["title"]] = legend_args[["title"]] %||% "Count"

    # Discrete mode draws a categorical legend, so use filled-square keys (pch
    # 22) like the other area/fill types (rect, polygon, boxplot, ...). The
    # continuous modes render a colourbar, where these have no effect.
    if (z_discrete) {
      legend_args[["pch"]] = legend_args[["pch"]] %||% 22
      legend_args[["pt.cex"]] = legend_args[["pt.cex"]] %||% 3.5
      # Space the large square keys so they don't overlap (cf. type_ridge).
      legend_args[["y.intersp"]] = legend_args[["y.intersp"]] %||% 1.25
    }

    env2env(environment(), settings, c(
      "datapoints", "by", "null_by", "legend_args", "type_info"
    ))
  }
  return(fun)
}


draw_hexbin = function(border = NA) {
  hborder = border
  fun = function(ix, iy, icol, ibg, ilty = par("lty"), ilwd = par("lwd"),
                 type_info, ...) {
    hexX = type_info[["hexX"]]
    hexY = type_info[["hexY"]]
    n = length(ix)
    if (n == 0L) return(invisible(NULL))

    # Expand each cell centre into a 6-vertex hexagon, NA-separated so a single
    # polygon() call draws them all. polygon() recycles col/border per
    # sub-polygon, so one colour per cell.
    px = rep(c(hexX, NA), n) + rep(ix, each = 7L)
    py = rep(c(hexY, NA), n) + rep(iy, each = 7L)

    fill = if (!is.null(ibg)) ibg else icol
    # "fill" sentinel matches borders to fills for seamless tiling (hexbin default).
    brd = if (identical(hborder, "fill")) fill else hborder
    polygon(
      x = px, y = py,
      col = fill,
      border = brd,
      lty = ilty,
      lwd = ilwd
    )
  }
  return(fun)
}
