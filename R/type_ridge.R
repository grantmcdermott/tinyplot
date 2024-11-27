#' Ridge plot type
#'
#' @description Type function for producing ridge plots (also known as joy plots),
#' which display density distributions for multiple groups with vertical offsets.
#' This function uses `tinyplot` scaffolding, which enables added functionality
#' such as grouping and faceting.
#'
#' The line color is controlled by the `col` argument in the `tinyplot()` call.
#' The fill color is controlled by the `bg` argument in the `tinyplot()` call.
#'
#'
#' @param scale Numeric. Controls the scaling factor of each plot.
#' Values greater than 1 means that plots overlap.
#' @param gradient Logical or character. Should a color gradient be used
#' to shade the area under the density? If a character specification is
#' used, then it can either be of length 1 and specify the palette to be
#' used with `gradient = TRUE` corresponding to `gradient = "viridis"`.
#' If a character vector of length greater than 1 is used, then it
#' should specifify the colors in the palette, e.g.,
#' `gradient = hcl.colors(512)`.
#' @param breaks Numeric. If a color gradient is used for shading, the
#' breaks between the colors can be modified. The default is to use
#' equidistant breaks spanning the range of the `x` variable.
#' @param probs Numeric. Instead of specifying the same `breaks` on the
#' x-axis for all groups, it is possible to specify group-specific quantiles
#' at the specified `probs`. The quantiles are computed based on the density
#' (rather than the raw original variable). Only one of `breaks` or
#' `probs` must be specified.
#' @param bw,kernel,... Arguments passed to \code{\link[stats]{density}}.
#' @param raster Logical. Should the ridges and color gradient, if relevant,
#' be drawn via a coercion to \code{\link[graphics]{rasterImage}}? Note that
#' this will result in potential smoothness artifacts at high resolutions.
#' Defaults to `FALSE`, in which case the ridges and color gradient will be
#' passed in vectorised fashion to \code{\link[graphics]{polygon}}.
#' @param col Character string denoting the outline (border) color for all
#' of the ridge densities. Note that a singular value is expected; if multiple
#' colors are provided then only the first will be used. This argument is mostly
#' useful for the aesthetic effect of drawing a common outline color in
#' combination with gradient fills. See Examples.
#' @param alpha Numeric in the range `[0,1]` for adjusting the alpha
#' transparency of the density fills. In most cases, will default to a value of
#' 1, i.e. fully opaque. But for some `by` grouped plots (excepting the special
#' cases where `by==y` or `by==x`), will default to 0.6.
#'
#' @examples
#' ## default ridge plot
#' tinyplot(Species ~ Sepal.Width, data = iris, type = "ridge")
#'
#' ## use the same bandwidth for all densities
#' tinyplot(Species ~ Sepal.Width, data = iris,
#'   type = type_ridge(bw = bw.nrd0(iris$Sepal.Width)))
#'
#' ## customized ridge plot without overlap of densities
#' tinyplot(Species ~ Sepal.Width, data = iris,
#'   type = type_ridge(scale = 1),
#'   bg = "light blue", col = "black")
#'
#' ## by grouping is also supported. two special cases of interest:
#'
#' # 1) by == y (color by y groups)
#' tinyplot(Species ~ Sepal.Width | Species, data = iris, type = "ridge")
#'
#' # 2) by == x (gradient coloring along x)
#' tinyplot(Species ~ Sepal.Width | Sepal.Width, data = iris, type = "ridge")
#'
#' # aside: pass explicit `type_ridge(col = <col>)` arg to set a common border
#' # color
#' tinyplot(Species ~ Sepal.Width | Sepal.Width, data = iris,
#'   type = type_ridge(col = "white"))
#'
#' ## gradient coloring along the x-axis can also be invoked manually without
#' ## a legend (the following lines are all equivalent)
#' tinyplot(Species ~ Sepal.Width, data = iris, type = type_ridge(gradient = TRUE))
#' # tinyplot(Species ~ Sepal.Width, data = iris, type = type_ridge(gradient = "viridis"))
#' # tinyplot(Species ~ Sepal.Width, data = iris, type = type_ridge(gradient = hcl.colors(512)))
#'
#' ## highlighting only the center 50% of the density (between 25% and 75% quantile)
#' tinyplot(Species ~ Sepal.Width, data = iris, col = "white", type = type_ridge(
#'   gradient = hcl.colors(3, "Dark Mint")[c(2, 1, 2)], probs = c(0.25, 0.75)))
#'
#' ## highlighting the probability distribution by the color gradient
#' tinyplot(Species ~ Sepal.Width, data = iris, type = type_ridge(
#'   gradient = hcl.colors(250, "Dark Mint")[c(250:1, 1:250)], probs = 0:500/500))
#'
#' ## with faceting and color gradient
#' airq = transform(airquality, Late = ifelse(Day > 15, "Late", "Early"))
#' tinyplot(Month ~ Ozone, facet = ~ Late, data = airq,
#'   type = type_ridge(gradient = TRUE),
#'   grid = TRUE, axes = "t", col = "white")
#'
#' @export
type_ridge = function(scale = 1.5, gradient = FALSE, breaks = NULL, probs = NULL, bw = "nrd0", kernel = "gaussian", ..., raster = FALSE, col = NULL, alpha = NULL) {
  density_args = list(bw = bw, kernel = kernel, ...)
  data_ridge = function() {
    fun = function(datapoints, yaxt = NULL, ...) {
      get_density = function(k) {
        out = do.call("density", c(list(x = k$x), density_args))
        out = data.frame(x = out$x, ymax = out$y, ymin = 0, y = k$y[1])
        out$ymax = out$ymax / max(out$ymax) * scale
        out$facet = k$facet[1]
        out$by = k$by[1]
        return(out)
      }
      #  catch for special cases
      anyby = length(unique(datapoints$by)) != 1
      x_by = identical(datapoints$x, datapoints$by)
      y_by = identical(datapoints$y, datapoints$by)
      if (x_by) {
        gradient = TRUE
        datapoints$by = ""
      } else if (y_by) {
        datapoints$by = ""
      } else if (anyby && is.null(alpha)) {
        alpha = 0.6
      }
      # flag for (non-gradient) interior fill adjustment
      fill_by = anyby || y_by
      if (isTRUE(x_by)) fill_by = FALSE
      # if (isTRUE(anyby) && is.null(alpha)) alpha = 0.6
      ##
      d = split(datapoints, list(datapoints$y, datapoints$by, datapoints$facet))
      d = lapply(d, function(k) tryCatch(get_density(k), error = function(e) NULL))
      d = do.call(rbind, Filter(function(x) !is.null(x), d))
      d = split(d, d$facet)
      offset_z = function(k) {
        ksplit = split(k, k$y)
        for (idx in seq_along(ksplit)) {
          ksplit[[idx]]$ymax = ksplit[[idx]]$ymax + idx - 1
          ksplit[[idx]]$ymin = ksplit[[idx]]$ymin + idx - 1
        }
        k = do.call(rbind, ksplit)
        return(k)
      }
      d = do.call(rbind, lapply(d, offset_z))
      
      if (y_by) {
        d$y = factor(d$y)
        d$by = factor(d$y, levels = rev(levels(d$y)))
      } else if (x_by) {
        d$by = d$x
      }
      
      # Manual breaks flag. Only used if gradient is on
      manbreaks = !is.null(breaks) || !is.null(probs)

      ## use color gradient?
      xlim = range(d$x, na.rm = TRUE)
      if (!is.null(probs)) {
        if (!is.null(breaks)) {
          warning("only one of 'breaks' and 'quantile' must be specified")
          probs = NULL
        } else {
          if (probs[1L] > 0) probs = c(0, probs)
          if (probs[length(probs)] < 1) probs = c(probs, 1)
        }
      }
      if (!isFALSE(gradient)) {
        dotspal = list(...)[["palette"]]
        palette = if (!is.null(dotspal)) dotspal else gradient
        gradient = TRUE
        if (isTRUE(palette)) palette = "viridis"

        if (length(palette) > 1L || !is.character(palette)) {
          ## color vector already given
          if (is.null(breaks) && is.null(probs)) {
            breaks = seq(from = xlim[1L], to = xlim[2L], length.out = length(palette) + 1L)
          } else {
            npal = pmax(length(breaks), length(probs)) - 1L
            if (length(palette) != npal) {
              warning("length of 'palette' does not match 'breaks'/'probs'")
              palette = rep_len(palette, npal)
            }
            if (isTRUE(raster)) raster = npal > 20L
          }
        } else {
          ## only palette name given
          npal = if (is.null(breaks) && is.null(probs)) 512L else pmax(length(breaks), length(probs)) - 1L
          palette = hcl.colors(npal, palette = palette)
          if (is.null(breaks) && is.null(probs)) breaks = seq(from = xlim[1L], to = xlim[2L], length.out = npal + 1L)
          if (isTRUE(raster)) raster = npal > 20L
        }
      } else {
        palette = NULL
        if (!is.null(breaks) || !is.null(probs)) gradient = TRUE
      }
      if (!is.null(breaks)) {
        breaks[1L] = pmin(breaks[1L], xlim[1L])
        breaks[length(breaks)] = pmax(breaks[length(breaks)], xlim[2L])
      }

      out = list(
        datapoints = d,
        yaxt = "n",
        ylim = c(min(d$ymin), max(d$ymax)),
        type_info = list(
          gradient = gradient,
          palette = palette,
          breaks = breaks,
          probs = probs,
          manbreaks = manbreaks,
          yaxt = yaxt,
          raster = raster,
          x_by = x_by,
          y_by = y_by,
          fill_by = fill_by,
          col = col,
          alpha = alpha
        )
      )
      return(out)
    }
    return(fun)
  }

  draw_ridge = function() {
    fun = function(ix, iy, iz, ibg, icol, iymin, iymax, type_info, ...) {
      d = data.frame(x = ix, y = iy, ymin = iymin, ymax = iymax)
      dsplit = split(d, d$y)
      if (is.null(ibg)) {
        ibg = if (isTRUE(type_info[["fill_by"]])) seq_palette(icol, n = 2)[2] else "gray"
      }
      if (!is.null(type_info[["alpha"]]) && is.null(type_info[["palette"]])) {
        ibg = adjustcolor(ibg, alpha.f = type_info[["alpha"]])
      }
      if (!is.null(type_info[["col"]])) icol = type_info[["col"]]
      draw_segments = if (type_info[["raster"]]) segmented_raster else segmented_polygon
      for (i in rev(seq_along(dsplit))) {
        if (type_info[["gradient"]]) {
          with(
            dsplit[[i]],
            draw_segments(
              x, ymax, ymin = ymin[1L],
              breaks = type_info[["breaks"]],
              probs = type_info[["probs"]],
              manbreaks = type_info[["manbreaks"]],
              col = if (is.null(type_info[["palette"]])) ibg else type_info[["palette"]],
              # border = if (is.null(type_info[["palette"]])) icol else "transparent",
              alpha = alpha
            )
          )
        }
        with(dsplit[[i]], polygon(x, ymax, col = if (type_info[["gradient"]]) "transparent" else ibg, border = NA))
        with(dsplit[[i]], lines(x, ymax, col = icol))
      }
      lab = if (is.factor(d$y)) levels(d$y) else unique(d$y)
      if (isTRUE(type_info[["y_by"]])) {
        # avoid duplicating the y-axis labs for the special y==by case
        # val = match(lab, levels(d$y)) - 1
        val = match(d$y[1], levels(d$y))
        lab = lab[val]
        val = val - 1
      } else {
        val = cumsum(rep(1, length(lab))) - 1
      }
      tinyAxis(x = d$y, side = 2, at = val, labels = lab, type = type_info[["yaxt"]])
    }
    return(fun)
  }

  out = list(
    draw = draw_ridge(),
    data = data_ridge(),
    name = "ridge"
  )
  class(out) = "tinyplot_type"
  return(out)
}

## auxiliary function for drawing shaded segmented polygon
segmented_polygon = function(x, y, ymin = 0, breaks = range(x), probs = NULL, manbreaks = FALSE, col = "lightgray", border = "transparent", alpha = NULL) {

  if (!is.null(probs)) {
    ## map quantiles to breaks
    if (!(missing(breaks) || is.null(breaks))) stop("only one of 'breaks' and 'probs' must be specified")
    breaks = quantile.density(list(x = x, y = y - ymin), probs = probs)
  }

  ## sanity check
  if (breaks[1L] > x[1L] || breaks[length(breaks)] < x[length(x)]) stop("'breaks' do no span range of 'x'")

  # ## recycle color (if necessary) rather use colorRampPalette below
  # col = rep_len(col, length(breaks) - 1L)
  
  # Create individual polygons
  if (isFALSE(manbreaks)) {
    # Special case for length(breaks)==length(x). We can take a fully vectorised
    # shortcut
    xx = c(rbind(x[-length(x)], x[-1], x[-1], x[-length(x)], NA))
    yy = c(rbind(y[-length(y)], y[-1], ymin, ymin, NA))
  } else {
    # For other cases, we'll do a bit more work to make sure that the polygons
    # overlap
    bvals = do.call(c, sapply(seq_along(breaks[-1]), function(b) tail(x[x<breaks[b]], 1)))
    bidx = match(bvals, x)
    bidx = c(1, bidx, length(x))
    xx = do.call(c, sapply(
      seq_along(bidx[-1]),
      function(b) {
        xx = x[bidx[b]:bidx[b+1]]
        c(xx, rev(xx), NA)
      }
    ))
    yy = do.call(c, sapply(
      seq_along(bidx[-1]),
      function(b) {
        yy = y[bidx[b]:bidx[b+1]]
        c(yy, rep(ymin, length(yy)), NA)
      }
    ))
    # Catch for missing lower breaks if individual density doesn't span the full
    # range of x (and discrete color fills are supplied)
    # Aside: We only need to fix the lower missing breaks, since missing top
    # breaks will be ignored in the final polygon call anyway
    lwrmissings = breaks < x[bidx][1]
    if (any(lwrmissings[-1])) {
      lwrmissings = breaks[lwrmissings] 
      xxtra = c(rbind(lwrmissings[-length(lwrmissings)], lwrmissings[-1], lwrmissings[-1], lwrmissings[-length(lwrmissings)], NA))
      yytra = c(rbind(rep(ymin, length(lwrmissings)-1), ymin, ymin, ymin, NA))
      xx = c(xxtra, xx)
      yy = c(yytra, yy)
    }
  }
  
  # Drop trailing NAs
  xx = xx[1:(length(xx)-1)]
  yy = yy[1:(length(yy)-1)]
  
  if (!is.null(alpha)) col = adjustcolor(col, alpha.f = alpha)
  
  # Color ramp for cases where the breaks don't match 
  if (isFALSE(length(col)==1)) {
    col = rev(col) ## uncomment to make extreme cols dark
    if (isTRUE(manbreaks)) {
      if (!is.null(breaks) && length(breaks)-1 != length(col)) {
        xrange = range(xx, na.rm = TRUE)
        idx = which(breaks >= xrange[1] & breaks < xrange[2])
        idx = c(idx, length(idx)+1)
        col = col[idx]
        col = colorRampPalette(col, alpha = TRUE)(length(x)) # support alpha?
      }
    } else if (isFALSE(manbreaks) || length(col) > length(x) || length(x) %% length(col) != 0) {
      xrange = range(xx, na.rm = TRUE)
      idx = which(breaks >= xrange[1] & breaks < xrange[2])
      idx = c(idx, length(idx)+1)
      col = col[idx]
      col = colorRampPalette(col, alpha = TRUE)(length(x)) # support alpha?
    }
  }
  
  ## draw all polygons
  polygon(xx, yy, col = col, border = border)
}

#' @importFrom graphics rasterImage
#' @importFrom grDevices as.raster
segmented_raster = function(x, y, ymin = 0, breaks = range(x), probs = NULL, manbreaks = FALSE, col = "lightgray", border = "transparent", alpha = NULL) {
  ## set up raster matrix on x-grid and 500 y-pixels 
  n = length(x) - 1L
  m = 500L ## FIXME: hard-coded?
  r = matrix(1:n, ncol = n, nrow = m, byrow = TRUE)

  ## map quantiles to breaks
  if (!is.null(probs)) {
    if (!(missing(breaks) || is.null(breaks))) stop("only one of 'breaks' and 'probs' must be specified")
    breaks = quantile.density(list(x = x, y = y - ymin), probs = probs)
  }

  if (!is.null(alpha)) col = adjustcolor(col, alpha.f = alpha)
  ## map colors to intervals and fill colors by column
  col = col[cut(x, breaks = breaks, include.lowest = TRUE)]
  r[] = col[r]

  ## clip raster pixels above density line
  ymax = max(y)
  ix = cbind(as.vector(row(r)), as.vector(col(r)))
  ix = ix[seq(from = ymax, to = ymin, length.out = m)[row(r)] > y[col(r)], , drop = FALSE]
  r[ix] = NA

  ## plot density and add raster gradient
  rasterImage(as.raster(r), min(x), ymin, max(x), ymax, interpolate = length(breaks) >= 20L) ## FIXME: improve quality for "few" breaks?
}

## auxiliary function for determining quantiles based on density function
#' @importFrom stats approx median
quantile.density = function(x, probs = seq(0, 1, 0.25), ...) {
  ## sanity check for probabilities
  if (any(probs < 0 | probs > 1)) stop("'probs' outside [0,1]")

  ## probability density function, extrapolated to zero, use midpoints
  n = length(x$x)
  pdf = x$y
  pdf = c(0, pdf, 0)

  ## x variable, also extrapolated, use midpoints
  x = x$x
  delta = median(diff(x))
  x = c(x[1L] - delta, x, x[n] + delta)

  ## numerical integration of density
  cdf = c(0, cumsum(diff(x) * (pdf[-1L] + pdf[-(n + 2L)])/2))
  cdf = cdf/cdf[n + 2L]

  ## approximate quantiles
  approx(cdf, x, xout = probs, rule = 2)$y  
}
