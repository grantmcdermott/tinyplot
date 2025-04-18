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
#' @param scale Numeric. Controls the scaling factor of each plot.
#' Values greater than 1 means that plots overlap.
#' @param joint.max character indicating how to scale the maximum of the densities:
#'   The default `"all"` indicates that all densities are scaled jointly relative to
#'   the same maximum so that the areas of all densities are comparable.
#'   Alternatively, `"facet"` indicates that the maximum is computed within
#'   each facet so that the areas of the densities are comparable within each
#'   facet but not necessarily across facets. Finally, `"by"` indicates that
#'   each row (in each facet) is scaled separately, so that the areas of the
#'   densities for `by` groups in the same row are comparable but not necessarily
#'   across rows.
#' @param breaks Numeric. If a color gradient is used for shading, the
#' breaks between the colors can be modified. The default is to use
#' equidistant breaks spanning the range of the `x` variable.
#' @param probs Numeric. Instead of specifying the same `breaks` on the
#' x-axis for all groups, it is possible to specify group-specific quantiles
#' at the specified `probs`. The quantiles are computed based on the density
#' (rather than the raw original variable). Only one of `breaks` or
#' `probs` must be specified.
#' @param ylevels a character or numeric vector specifying in which order
#' the levels of the y-variable should be plotted.
#' @inheritParams stats::density
#' @param kernel a character string giving the smoothing kernel to be used. This
#'   must partially match one of `"gaussian"`, `"rectangular"`, `"triangular"`,
#'   `"epanechnikov"`, `"biweight"`, `"cosine"` or `"optcosine"`, with default
#'   `"gaussian"`, and may be abbreviated to a unique prefix (single letter).
#'
#'   `"cosine"` is smoother than `"optcosine"`, which is the usual 'cosine'
#'   kernel in the literature and almost MSE-efficient. However, `"cosine"` is
#'   the version used by S.
#' @param joint.bw character string indicating whether (and how) the smoothing
#'   bandwidth should be computed from the joint data distribution. The default
#'   of `"mean"` will compute the joint bandwidth as the mean of the individual
#'   subgroup bandwidths (weighted by their number of observations). Choosing
#'   `"full"` will result in a joint bandwidth computed from the full
#'   distribution (merging all subgroups). For `"none"` the individual bandwidth
#'   will be computed independently for each subgroup. Also accepts a logical
#'   argument, where `TRUE` maps to `"mean"` and `FALSE` maps to `"none"`. See
#'   \code{\link{type_density}} for some discussion of practical considerations.
#' @param gradient Logical or character. Should a gradient fill be used to
#'   shade the area under the density? If a character specification is used,
#'   then it can either be of length 1 and specify the palette to be used with
#'   `gradient = TRUE` corresponding to `gradient = "viridis"`. If a character
#'   vector of length greater than 1 is used, then it should specify the
#'   colors in the palette, e.g., `gradient = hcl.colors(512)`.
#' @param raster Logical. Should the `gradient` fill be drawn using
#'   \code{\link[graphics]{rasterImage}}? Defaults to `FALSE`, in which case the
#'   `gradient` fill will instead be drawn using
#'   \code{\link[graphics]{polygon}}. See the `Technical note on gradient fills`
#'   section below.
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
#' @section Technical note on gradient fills:
#' 
#' `tinyplot` uses two basic approaches for drawing gradient fills in ridge line
#' plots, e.g., if `type_ridge(gradient = TRUE)`.
#' 
#' The first (and default) polygon-based approach involves dividing up the main
#' density region into many smaller polygons along the x-axis. Each of these
#' smaller polygons inherits a different color "segment" from the underlying
#' palette swatch, which in turn creates the effect of a continuous gradient
#' when they are all plotted together. Internally, this polygon-based approach
#' is vectorized (i.e., all of the sub-polygons are plotted simultaneously). It
#' is thus efficient from a plotting perspective and generally also performs
#' well from an aesthetic perspective. However, it can occasionally produce
#' undesirable plotting artifacts on some graphics devices---e.g., thin but
#' visible vertical lines---if alpha transparency is being used at the same 
#' time.
#' 
#' For this reason, we also offer an alternative raster-based approach for
#' gradient fills that users can invoke via
#' `type_ridge(gradient = TRUE, raster = TRUE)`. The essential idea is that we
#' coerce the density polygon into a raster representation (using
#' \code{\link[graphics]{rasterImage}}) and achieve the gradient effect via
#' color interpolation. The trade-off this time is potential smoothness
#' artifacts around the top of the ridge densities at high resolutions, since we
#' have converted a vector object into a raster object.
#' 
#' Again, we expect that the choice between these two approaches will only
#' matter for ridge plots that combine gradient fills with alpha transparency
#' (and on certain graphics devices). We recommend that users experiment to
#' determine which approach is optimal for their device.
#'
#' @examples
#' aq = transform(
#'   airquality,
#'   Month = factor(month.abb[Month], levels = month.abb[5:9]),
#'   Month2 = factor(month.name[Month], levels = month.name[5:9]),
#'   Late = ifelse(Day > 15, "Late", "Early")
#'   )
#' 
#' # default ridge plot (using the "ridge" convenience string)
#' tinyplot(Month ~ Temp, data = aq, type = "ridge")
#' 
#' # for ridge plots, we recommend pairing with the dedicated theme(s), which
#' # facilitate nicer y-axis labels, grid lines, etc.
#' 
#' tinytheme("ridge")
#' tinyplot(Month ~ Temp, data = aq, type = "ridge")
#' 
#' tinytheme("ridge2") # removes the plot frame (but keeps x-axis line)
#' tinyplot(Month ~ Temp, data = aq, type = "ridge")
#' 
#' # the "ridge(2)" themes are especially helpful for long y labels, due to
#' # dyanmic plot adjustment
#' tinyplot(Month2 ~ Temp, data = aq, type = "ridge")
#' 
#' # pass customization arguments through type_ridge()... for example, use
#' # the scale argument to change/avoid overlap of densities (more on scaling
#' # further below)
#' 
#' tinyplot(Month ~ Temp, data = aq, type = type_ridge(scale = 1))
#'   
#' ## by grouping is also supported. two special cases of interest:
#'
#' # 1) by == y (color by y groups)
#' tinyplot(Month ~ Temp | Month, data = aq, type = "ridge")
#'
#' # 2) by == x (gradient coloring along x)
#' tinyplot(Month ~ Temp | Temp, data = aq, type = "ridge")
#'
#' # aside: pass explicit `type_ridge(col = <col>)` arg to set a different
#' # border color
#' tinyplot(Month ~ Temp | Temp, data = aq, type = type_ridge(col = "white"))
#'
#' # gradient coloring along the x-axis can also be invoked manually without
#' # a legend (the next two tinyplot calls are equivalent)
#' 
#' # tinyplot(Month ~ Temp, data = aq, type = type_ridge(gradient = "agsunset"))
#' tinyplot(Month ~ Temp, data = aq, type = type_ridge(gradient = TRUE))
#' 
#' # aside: when combining gradient fill with alpha transparency, it may be
#' # better to use the raster-based approach (test on your graphics device)
#' 
#' tinyplot(Month ~ Temp, data = aq,
#'   type = type_ridge(gradient = TRUE, alpha = 0.5),
#'   main = "polygon fill (default)")
#' tinyplot(Month ~ Temp, data = aq,
#'   type = type_ridge(gradient = TRUE, alpha = 0.5, raster = TRUE),
#'   main = "raster fill")
#'
#' # highlighting only the center 50% of the density (i.e., 25%-75% quantiles)
#' tinyplot(Month ~ Temp, data = aq, type = type_ridge(
#'   gradient = hcl.colors(3, "Dark Mint")[c(2, 1, 2)],
#'   probs = c(0.25, 0.75), col = "white"))
#'
#' # highlighting the probability distribution by color gradient
#' # (darkest point = median)
#' tinyplot(Month ~ Temp, data = aq, type = type_ridge(
#'   gradient = hcl.colors(250, "Dark Mint")[c(250:1, 1:250)],
#'   probs = 0:500/500))
#'
#' # faceting also works, although we recommend switching (back) to the "ridge"
#' # theme for faceted ridge plots
#' 
#' tinytheme("ridge")
#' tinyplot(Month ~ Ozone, facet = ~ Late, data = aq,
#'   type = type_ridge(gradient = TRUE))
#' 
#' ## use the joint.max argument to vary the maximum density used for
#' ## determining relative scaling...
#' 
#' # jointly across all densities (default) vs. per facet
#' tinyplot(Month ~ Temp, facet = ~ Late, data = aq,
#'   type = type_ridge(scale = 1))
#' tinyplot(Month ~ Temp, facet = ~ Late, data = aq,
#'   type = type_ridge(scale = 1, joint.max = "facet"))
#' 
#' # jointly across all densities (default) vs. per by row
#' tinyplot(Month ~ Temp | Late, data = aq,
#'   type = type_ridge(scale = 1))
#' tinyplot(Month ~ Temp | Late, data = aq,
#'   type = type_ridge(scale = 1, joint.max = "by"))
#'   
#' # restore the default theme
#' tinytheme()
#'
#' @export
type_ridge = function(
    scale = 1.5,
    joint.max = c("all", "facet", "by"),
    breaks = NULL,
    probs = NULL,
    ylevels = NULL,
    bw = "nrd0",
    joint.bw =  c("mean", "full", "none"),
    adjust = 1,
    kernel = c("gaussian", "epanechnikov", "rectangular", "triangular", "biweight", "cosine", "optcosine"),
    n = 512,
    # more args from density here?
    gradient = FALSE,
    raster = FALSE,
    col = NULL,
    alpha = NULL
    ) {
  
  kernel = match.arg(kernel, c("gaussian", "epanechnikov", "rectangular", "triangular", "biweight", "cosine", "optcosine"))
  if (is.logical(joint.bw)) {
    joint.bw = ifelse(joint.bw, "mean", "none")
  }
  joint.bw = match.arg(joint.bw, c("mean", "full", "none"))

  out = list(
    draw = draw_ridge(),
    data = data_ridge(bw = bw, adjust = adjust, kernel = kernel, n = n,
                      joint.bw = joint.bw,
                      scale = scale,
                      joint.max = joint.max,
                      gradient = gradient,
                      breaks = breaks,
                      probs = probs,
                      ylevels = ylevels,
                      raster = raster,
                      col = col,
                      alpha = alpha
                      ),
    name = "ridge"
  )
  class(out) = "tinyplot_type"
  return(out)
}

#
## Underlying data_ridge function
data_ridge = function(bw = "nrd0", adjust = 1, kernel = "gaussian", n = 512,
                      joint.bw = "mean",
                      scale = 1.5,
                      joint.max = "all",
                      gradient = FALSE,
                      breaks = NULL,
                      probs = NULL,
                      ylevels = NULL,
                      raster = FALSE,
                      col = NULL,
                      alpha = NULL
                      ) {
  fun = function(datapoints, yaxt = NULL, null_by, ...) {
    #  catch for special cases
    anyby = !null_by
    x_by = anyby && identical(datapoints$x, datapoints$by)
    y_by = anyby && identical(datapoints$y, datapoints$by)
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

    ## reorder levels of y-variable if requested
    if (!is.null(ylevels)) {
      if (!is.factor(datapoints$y)) datapoints$y = factor(datapoints$y)
      datapoints$y = factor(datapoints$y, levels = if(is.numeric(ylevels)) levels(datapoints$y)[ylevels] else ylevels)
      if (y_by) datapoints$by = datapoints$y
    }

    ##
    datapoints = split(datapoints, list(datapoints$y, datapoints$by, datapoints$facet))

    if (joint.bw == "none" || is.numeric(bw)) {
        dens_bw = bw
    } else {
        if (joint.bw == "mean") {
            # Use weighted mean of subgroup bandwidths
            bws = sapply(datapoints, function(dat) bw_fun(kernel = bw, dat$x))
            ws = sapply(datapoints, nrow)
            dens_bw = weighted.mean(bws, ws)
        } else if (joint.bw == "full") {
            dens_bw = bw_fun(kernel = bw, unlist(sapply(datapoints, `[[`, "x")))
        }
    }

    datapoints = lapply(datapoints, function(dat) {
      dens = density(dat$x, bw = dens_bw, kernel = kernel, n = n)
      out = data.frame(
        by = dat$by[1], # already split
        facet = dat$facet[1], # already split
        x = dens$x,
        y = dat$y[1],
        ymin = 0L,
        ymax = dens$y
      )
      return(out)
    })
    datapoints = do.call(rbind, datapoints)

    if (is.character(joint.max)) {
      joint.max = match.arg(joint.max, c("all", "facet", "by"))
      joint.max = switch(joint.max,
        "all" = rep.int(1, nrow(datapoints)),
        "facet" = datapoints$facet,
        "by" = interaction(datapoints$facet, datapoints$y)
      )
      joint.max = ave(datapoints$ymax, joint.max, FUN = max)
    }
    datapoints$ymax = datapoints$ymax / joint.max * scale

    datapoints = split(datapoints, datapoints$facet)
    offset_z = function(k) {
      ksplit = split(k, k$y)
      for (idx in seq_along(ksplit)) {
        ksplit[[idx]]$ymax = ksplit[[idx]]$ymax + idx - 1
        ksplit[[idx]]$ymin = ksplit[[idx]]$ymin + idx - 1
      }
      k = do.call(rbind, ksplit)
      return(k)
    }
    datapoints = do.call(rbind, lapply(datapoints, offset_z))

    if (y_by) {
      datapoints$y = factor(datapoints$y)
      datapoints$by = factor(datapoints$y, levels = rev(levels(datapoints$y)))
    } else if (x_by) {
      datapoints$by = datapoints$x
    }

    # Manual breaks flag. Only used if gradient is on
    manbreaks = !is.null(breaks) || !is.null(probs)

    ## use color gradient?
    xlim = range(datapoints$x, na.rm = TRUE)
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
      if (isTRUE(palette)) {
        palette = if (!is.null(.tpar[["palette.sequential"]])) .tpar[["palette.sequential"]] else "viridis"
      }

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
    
    if (is.null(col) && (!anyby || x_by)) col = "black"

    out = list(
      datapoints = datapoints,
      yaxt = "n",
      ylim = c(min(datapoints$ymin), max(datapoints$ymax)),
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


#
## Underlying draw_ridge function
draw_ridge = function() {
  fun = function(ix, iy, iz, ibg, icol, iymin, iymax, type_info, ...) {
    ridge_theme = identical(.tpar[["tinytheme"]], "ridge") || identical(.tpar[["tinytheme"]], "ridge2")
    d = data.frame(x = ix, y = iy, ymin = iymin, ymax = iymax)
    dsplit = split(d, d$y)
    if (is.null(ibg)) {
      default_bg = if (!ridge_theme && !is.null(.tpar[["palette.qualitative"]])) seq_palette(by_col(), n = 2)[2] else "gray"
      ibg = if (isTRUE(type_info[["fill_by"]])) seq_palette(icol, n = 2)[2] else default_bg
    }
    if (!is.null(type_info[["alpha"]]) && is.null(type_info[["palette"]])) {
      ibg = adjustcolor(ibg, alpha.f = type_info[["alpha"]])
    }
    if (!is.null(type_info[["col"]])) icol = type_info[["col"]]
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
    if (ridge_theme) abline(h = val, col = .tpar[["grid.col"]])
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
            alpha = type_info[["alpha"]]
          )
        )
      }
      with(dsplit[[i]], polygon(x, ymax, col = if (type_info[["gradient"]]) "transparent" else ibg, border = NA))
      with(dsplit[[i]], lines(x, ymax, col = icol))
    }
    # tinyAxis(x = d$y, side = 2, at = val, labels = lab, type = type_info[["yaxt"]], padj = padj)
    if (ridge_theme) {
      tinyAxis(x = d$y, side = 2, at = val, labels = lab, type = type_info[["yaxt"]],
               padj = 0,
               mgp = c(3, 1, 0) - c(0.5, 0.5 + 0.3, 0),
               tcl = 0)
      if (identical(.tpar[["tinytheme"]], "ridge2")) axis(1, labels = FALSE)
    } else {
      tinyAxis(x = d$y, side = 2, at = val, labels = lab, type = type_info[["yaxt"]])
    }
  }
  return(fun)
}


#
## Auxiliary functions

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
  border = if (is.null(alpha)) col else adjustcolor(col = col, alpha.f = alpha/2)
  
  ## draw all polygons
  polygon(xx, yy, col = col, border = border, lwd = 0.5)
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
  col = rev(col) ## uncomment to make extreme cols dark
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

#' @importFrom stats median approx
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
