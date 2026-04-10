#' Conditional density plot type
#'
#' @description Type function for producing conditional density plots, which
#'   visualize how the conditional distribution of a categorical variable `y`
#'   changes over a numerical variable `x`. This is similar to a spinogram (see
#'   [`type_spineplot()`]) but uses kernel density smoothing instead of
#'   discretizing `x` into bins.
#' @param ylevels a character or numeric vector specifying the ordering of the
#'   levels of the `y` variable (if character) or the corresponding indexes
#'   (if numeric) for the plot.
#' @param tol.ylab convenience tolerance parameter for y-axis annotation. If
#'   the distance between two labels drops under this threshold, they are
#'   plotted equidistantly.
#' @param col a vector of fill colors of the same length as `levels(y)`.
#' @param yaxlabels character vector for annotation of y axis, defaults to
#'   `levels(y)`.
#' @param weights an optional numeric vector of weights.
#' @inheritParams type_density
#' @param from,to the left and right-most points of the grid at which the
#'   density is to be estimated; the defaults are `NULL` (i.e., inferred from
#'   the data range).
#' @examples
#' # NASA space shuttle o-ring failures
#' fail = factor(c(2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 2, 1, 2, 1, 1, 1,
#'                  1, 2, 1, 1, 1, 1, 1),
#'                levels = 1:2, labels = c("no", "yes"))
#' temperature = c(53, 57, 58, 63, 66, 67, 67, 67, 68, 69, 70, 70,
#'                  70, 70, 72, 73, 75, 75, 76, 76, 78, 79, 81)
#'
#' # Basic CD plot
#' tinyplot(fail ~ temperature, type = "cdplot")
#'
#' # Custom bandwidth
#' tinyplot(fail ~ temperature, type = type_cdplot(bw = 2))
#'
#' # Colored by y levels
#' tinyplot(fail ~ temperature | fail, type = "cdplot")
#'
#' # Compare with spinogram
#' tinyplot(fail ~ temperature, type = type_spineplot(breaks = 3))
#'
#' # Iris example
#' tinyplot(Species ~ Sepal.Width, data = iris, type = "cdplot")
#'
#' @importFrom stats density
#' @importFrom grDevices gray.colors
#' @export
type_cdplot = function(
    bw = "nrd0",
    joint.bw = c("mean", "full", "none"),
    adjust = 1,
    kernel = c("gaussian", "epanechnikov", "rectangular", "triangular", "biweight", "cosine", "optcosine"),
    n = 512,
    from = NULL,
    to = NULL,
    ylevels = NULL,
    tol.ylab = 0.05,
    col = NULL,
    yaxlabels = NULL,
    weights = NULL
) {
  kernel = match.arg(kernel, c("gaussian", "epanechnikov", "rectangular", "triangular", "biweight", "cosine", "optcosine"))
  if (is.logical(joint.bw)) {
    joint.bw = ifelse(joint.bw, "mean", "none")
  }
  joint.bw = match.arg(joint.bw, c("mean", "full", "none"))
  out = list(
    data = data_cdplot(
      bw = bw, joint.bw = joint.bw, adjust = adjust, kernel = kernel,
      n = n, from = from, to = to,
      ylevels = ylevels, yaxlabels = yaxlabels, weights = weights
    ),
    draw = draw_cdplot(tol.ylab = tol.ylab, col = col, yaxlabels = yaxlabels),
    name = "cdplot"
  )
  class(out) = "tinyplot_type"
  return(out)
}


data_cdplot = function(
    bw = "nrd0", joint.bw = "mean", adjust = 1, kernel = "gaussian",
    n = 512, from = NULL, to = NULL,
    ylevels = NULL, yaxlabels = NULL, weights = NULL
) {
  fun = function(settings, ...) {
    env2env(settings, environment(), c(
      "datapoints", "xlim", "ylim", "facet", "facet.args", "by",
      "xaxb", "yaxb", "null_by", "null_facet", "null_palette",
      "col", "bg", "axes", "xaxt", "yaxt"
    ))

    ## process weights
    if (!is.null(weights)) {
      ny = length(datapoints$y)
      if (length(weights) != ny && length(weights) != 1L) {
        stop(sprintf("'weights' must have either length 1 or %s", ny))
      }
    }

    ## process y variable
    if (!is.factor(datapoints$y)) datapoints$y = factor(datapoints$y)
    if (is.null(ylim)) ylim = c(0, 1)

    ## reorder y levels if requested
    y_by = identical(datapoints$y, datapoints$by)
    if (!is.null(ylevels)) {
      ylevels_resolved = if (is.numeric(ylevels)) levels(datapoints$y)[ylevels] else ylevels
      if (anyNA(ylevels_resolved) || !all(ylevels_resolved %in% levels(datapoints$y))) {
        warning("not all 'ylevels' correspond to levels of 'y'")
      }
      datapoints$y = factor(datapoints$y, levels = ylevels_resolved)
      if (y_by) datapoints$by = datapoints$y
    }

    ## adjust facet margins
    if (!is.null(facet) && is.null(facet.args[["fmar"]])) {
      facet.args[["fmar"]] = c(2, 2, 2, 2)
    }

    ## save y levels and global x range before splitting
    y_levels_orig = levels(datapoints$y)
    x_range = range(as.numeric(datapoints$x), na.rm = TRUE)

    ## split data
    if (isTRUE(y_by)) {
      datapoints = split(datapoints, list(datapoints$facet))
    } else {
      datapoints = split(datapoints, list(datapoints$by, datapoints$facet))
    }
    datapoints = Filter(function(k) nrow(k) > 0, datapoints)

    ## bandwidth selection (following type_ridge/type_density pattern)
    if (joint.bw == "none" || is.numeric(bw)) {
      dens_bw = bw
    } else {
      if (joint.bw == "mean") {
        bws = sapply(datapoints, function(dat) bw_fun(kernel = bw, dat$x))
        ws = sapply(datapoints, nrow)
        dens_bw = weighted.mean(bws, ws)
      } else if (joint.bw == "full") {
        dens_bw = bw_fun(kernel = bw, unlist(sapply(datapoints, `[[`, "x")))
      }
    }

    ## compute conditional densities per group
    ## Algorithm follows R-devel graphics:::cdplot.default (improved version):
    ## 1. Compute density for each individual y-level
    ## 2. Weight by marginal proportions, sum to get total density
    ## 3. Drop zero-density points (avoids 0/0 artifacts)
    ## 4. Cumulative sums / total density = conditional probabilities
    datapoints = Map(function(dat) {
      x_num = as.numeric(dat$x)
      y_fac = dat$y
      ny = nlevels(y_fac)

      ## normalize weights
      w = if (is.null(weights)) NULL else weights / sum(weights)

      ## unconditional density for bandwidth and x grid
      ## use global x range (from/to override) so all facets share the same grid
      d_from = if (!is.null(from)) from else x_range[1L]
      d_to = if (!is.null(to)) to else x_range[2L]
      dx = density(x_num, bw = dens_bw, adjust = adjust, kernel = kernel,
                   n = n, from = d_from, to = d_to, weights = w)
      x1 = dx$x

      ## marginal proportions of y
      if (is.null(weights)) {
        yprop = cumsum(proportions(table(y_fac)))
      } else {
        yprop = cumsum(proportions(tapply(weights, y_fac, FUN = sum, na.rm = TRUE)))
        yprop[is.na(yprop)] = 0
      }

      ## compute density for each individual level
      y1 = matrix(0, nrow = ny, ncol = n)
      for (i in seq_len(ny)) {
        yi = y_fac %in% levels(y_fac)[i]
        wi = if (is.null(weights)) NULL else weights[yi] / sum(weights[yi])
        y1[i, ] = density(x_num[yi], bw = dx$bw, adjust = 1, kernel = kernel,
                          n = n, from = min(dx$x), to = max(dx$x), weights = wi)$y
      }

      ## weight by marginal proportions and compute total density
      y1 = y1 * diff(c(0, yprop))
      dx_y = colSums(y1)

      ## drop x coordinates with negligible total density
      ## (relative threshold avoids artifacts from near-zero division)
      dx_y_pos = dx_y > max(dx_y) * 1e-5
      dx_y = dx_y[dx_y_pos]
      x1 = x1[dx_y_pos]
      y1 = y1[-ny, dx_y_pos, drop = FALSE]

      ## cumulative densities divided by total density
      y1[] = apply(y1, 2L, cumsum)
      y1 = sweep(y1, 2L, dx_y, `/`, check.margin = FALSE)

      ## boundary matrix: (ny+1) rows
      y1 = rbind(0, y1, 1)

      ## trim polygons to this group's observed x range (not global)
      keep = x1 >= min(x_num) & x1 <= max(x_num)
      x1 = x1[keep]
      y1 = y1[, keep, drop = FALSE]
      n_trimmed = length(x1)

      ## build polygon-ready datapoints: one set of rows per band
      bands = lapply(seq_len(ny), function(band) {
        data.frame(
          by = dat$by[1],
          facet = dat$facet[1],
          x = x1,
          y = (y1[band, ] + y1[band + 1L, ]) / 2,
          ymin = y1[band, ],
          ymax = y1[band + 1L, ]
        )
      })
      out = do.call(rbind, bands)
      attr(out, "ny") = ny
      attr(out, "n_trimmed") = n_trimmed
      attr(out, "y1_first_col") = y1[, 1L]
      return(out)
    }, dat = datapoints)

    ny = attr(datapoints[[1]], "ny")
    n_pts_list = lapply(datapoints, attr, "n_trimmed")
    y1_first_col = lapply(datapoints, attr, "y1_first_col")
    datapoints = do.call(rbind, datapoints)

    ## y-axis labels (use levels saved before splitting)
    ylabs_src = if (!is.null(yaxlabels)) {
      rep_len(yaxlabels, ny)
    } else {
      y_levels_orig
    }
    if (!is.null(yaxb)) {
      ylabs_src[!(ylabs_src %in% yaxb)] = ""
    }

    ## handle y_by coloring
    if (isTRUE(y_by)) {
      datapoints$by = factor(unlist(lapply(n_pts_list, function(np) rep(ylabs_src, each = np))))
    }

    ## grayscale flag
    grayscale = null_by && null_palette && is.null(.tpar[["palette.qualitative"]])

    x = datapoints$x
    y = c(datapoints$ymin, datapoints$ymax)
    ymin = datapoints$ymin
    ymax = datapoints$ymax
    by_out = if (null_by) by else datapoints$by
    facet_out = if (null_facet) facet else datapoints$facet

    ## save original axis settings before overriding
    axes_orig = axes
    xaxt_orig = xaxt
    yaxt_orig = yaxt

    axes = FALSE
    frame.plot = FALSE
    xaxt = "n"
    yaxt = "n"
    xaxs = "i"
    yaxs = "i"
    if (is.null(xlim)) xlim = x_range

    type_info = list(
      ny = ny,
      n_pts = n_pts_list,
      yaxlabels = ylabs_src,
      y1_first_col = y1_first_col,
      axes = axes_orig,
      xaxt = xaxt_orig,
      yaxt = yaxt_orig,
      grayscale = grayscale,
      y_by = y_by
    )

    ## legend customizations
    settings$legend_args[["pch"]] = settings$legend_args[["pch"]] %||% 22
    settings$legend_args[["pt.cex"]] = settings$legend_args[["pt.cex"]] %||% 3.5
    settings$legend_args[["pt.lwd"]] = settings$legend_args[["pt.lwd"]] %||% 0
    settings$legend_args[["y.intersp"]] = settings$legend_args[["y.intersp"]] %||% 1.25
    settings$legend_args[["seg.len"]] = settings$legend_args[["seg.len"]] %||% 1.25

    env2env(environment(), settings, c(
      "x", "y", "ymin", "ymax", "col", "bg", "datapoints",
      "by_out", "facet_out", "axes", "frame.plot", "xaxt", "yaxt",
      "xaxs", "yaxs", "xlim", "ylim",
      "type_info", "facet.args"
    ))
    # fix naming: env2env uses the local variable names
    settings$by = by_out
    settings$facet = facet_out
  }
  return(fun)
}


draw_cdplot = function(tol.ylab = 0.05, col = NULL, yaxlabels = NULL) {
  fun = function(ix, iy, ixmin, ixmax, iymin, iymax, ilty, ilwd, icol, ibg,
                 flip,
                 facet_window_args,
                 type_info,
                 ifacet,
                 ...) {

    if (is.null(yaxlabels)) yaxlabels = type_info[["yaxlabels"]]
    ny = type_info[["ny"]]
    n_pts = type_info[["n_pts"]][[ifacet]]
    grayscale = type_info[["grayscale"]]
    y_by = type_info[["y_by"]]
    y1_first_col = type_info[["y1_first_col"]][[ifacet]]

    ## graphical parameters (same logic as spineplot)
    if (is.null(col)) {
      if (is.null(ibg)) ibg = icol
      if (isFALSE(y_by)) {
        ibg = if (isTRUE(grayscale)) gray.colors(ny) else seq_palette(ibg, ny)
      }
      ibg = rep_len(ibg, ny)
    } else {
      ibg = rep_len(col, ny)
    }

    ## draw filled polygon bands
    ## After flip_datapoints, x/y and xmin/ymin etc. are already swapped.
    ## Non-flip: ix = eval grid, iymin/iymax = band boundaries
    ## Flip: iy = eval grid, ixmin/ixmax = band boundaries (iymin/iymax are NULL)
    flipped = isTRUE(flip)
    band_lo = if (flipped) ixmin else iymin
    band_hi = if (flipped) ixmax else iymax
    grid_vals = if (flipped) iy else ix

    ## detect gaps in the evaluation grid (from dropped low-density points)
    ## and split into contiguous runs
    first_band_idx = seq_len(n_pts)
    gv = grid_vals[first_band_idx]
    diffs = diff(gv)
    median_step = median(diffs)
    gap_pos = which(diffs > 3 * median_step)
    ## build run boundaries: list of (start, end) index pairs
    starts = c(1L, gap_pos + 1L)
    ends = c(gap_pos, n_pts)

    ## draw each contiguous run as a separate polygon + boundary line
    border_col = par("fg")
    for (r in seq_along(starts)) {
      run = starts[r]:ends[r]

      ## first pass: fill polygons without borders
      for (band in seq_len(ny)) {
        idx = run + (band - 1L) * n_pts
        bg = grid_vals[idx]
        bl = band_lo[idx]
        bh = band_hi[idx]
        if (flipped) {
          polygon(x = c(bh, rev(bl)), y = c(bg, rev(bg)),
                  col = ibg[band], border = NA)
        } else {
          polygon(x = c(bg, rev(bg)), y = c(bh, rev(bl)),
                  col = ibg[band], border = NA)
        }
      }

      ## second pass: draw boundary lines between bands
      for (band in seq_len(ny - 1L)) {
        idx = run + (band - 1L) * n_pts
        bg = grid_vals[idx]
        bh = band_hi[idx]
        if (flipped) {
          lines(x = bh, y = bg, col = border_col)
        } else {
          lines(x = bg, y = bh, col = border_col)
        }
      }
    }

    ## axes
    if (type_info[["axes"]]) {
      ## x-axis: standard numeric
      x_side = if (flip) 2 else 1
      y_side = if (flip) 1 else 2
      prob_side = if (flip) 3 else 4

      if (!(type_info[["xaxt"]] == "n")) {
        axis(x_side)
      }

      ## y-axis: category labels (no ticks)
      if (!(type_info[["yaxt"]] == "n")) {
        equidist = any(diff(y1_first_col) < tol.ylab)
        yat = if (equidist) {
          seq.int(1 / (2 * ny), 1 - 1 / (2 * ny), by = 1 / ny)
        } else {
          (y1_first_col[-1L] + y1_first_col[-(ny + 1L)]) / 2
        }
        axis(y_side, at = yat, labels = yaxlabels, tick = FALSE)

        ## probability axis on opposite side
        if (is_facet_position(
          if (flip) "bottom" else "right", ifacet, facet_window_args
        )) {
          axis(prob_side)
        }
      }
    }
    box()
  }
  return(fun)
}
