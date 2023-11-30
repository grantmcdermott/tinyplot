#' @title Lightweight extension of the base R plotting function
#'   
#' @description Extends base R's
#'   default plotting function, particularly as it applies to scatter and
#'   line plots with grouped data. For example, `plot2` makes it easy to plot
#'   different categories of a dataset in a single function call and highlight
#'   these categories (groups) using modern colour palettes. Coincident with
#'   this grouping support, `plot2` also produces automatic legends with scope
#'   for further customization. While the package also offers several other
#'   enhancements, it tries as far as possible to be a drop-in replacement
#'   for the equivalent base plot function. Users should generally be able to
#'   swap a valid `plot` call with `plot2` without any changes to the output.
#' 
#' @md
#' @param x,y the x and y arguments provide the x and y coordinates for the
#'   plot. Any reasonable way of defining the coordinates is acceptable. See
#'   the function xy.coords for details. If supplied separately, they must be
#'   of the same length.
#' @param by grouping variable(s). By default, groups will be represented
#'   through colouring of the plot elements. However, this can be turned off
#'   and other plot parameters (e.g., line types) can also take on grouping
#'   behaviour via the special "by" keyword. See Examples.
#' @param facet the faceting variable that you want arrange separate plot
#'   windows by. Also accepts the special "by" convenience keyword, in which
#'   case facets will match the grouping variable(s) above.
#' @param formula a `formula` that optionally includes grouping variable(s)
#'   after a vertical bar, e.g. `y ~ x | z`. One-sided formulae are also
#'   permitted, e.g. `~ y | z`. Note that the `formula` and `x` arguments
#'   should not be specified in the same call.
#' @param data a data.frame (or list) from which the variables in formula
#'   should be taken. A matrix is converted to a data frame.
#' @param type character string giving the type of plot desired. Options are:
#'   - The same set of 1-character values supported by plot: "p" for points, "l"
#'   for lines, "b" for both points and lines, "c" for empty points joined by
#'   lines, "o" for overplotted points and lines, "s" and "S" for stair steps
#'   and "h" for histogram-like vertical lines. "n" does not produce
#'   any points or lines.
#'   - Additional plot2 types: "density" for densities, "pointrange" or
#'   "errorbar" for segement intervals, and "ribbon" or "area" for polygon
#'   intervals (where area plots are a special case of ribbon plots with `ymin`
#'   set to 0 and `ymax` set to `y`; see below).
#' @param xlim the x limits (x1, x2) of the plot. Note that x1 > x2 is allowed
#'   and leads to a ‘reversed axis’. The default value, NULL, indicates that
#'   the range of the `finite` values to be plotted should be used.
#' @param ylim the y limits of the plot.
#' @param log a character string which contains "x" if the x axis is to be
#'   logarithmic, "y" if the y axis is to be logarithmic and "xy" or "yx" if 
#'   both axes are to be logarithmic.
#' @param main a main title for the plot, see also `title`.
#' @param sub a subtitle for the plot.
#' @param xlab a label for the x axis, defaults to a description of x.
#' @param ylab a label for the y axis, defaults to a description of y.
#' @param ann a logical value indicating whether the default annotation (title
#'   and x and y axis labels) should appear on the plot.
#' @param axes a logical value indicating whether both axes should be drawn on
#'   the plot. Use `graphical parameter` "xaxt" or "yaxt" to suppress just one of
#'   the axes.
#' @param frame.plot a logical indicating whether a box should be drawn around
#'   the plot. Can also use `frame` as an acceptable argument alias.
#' @param grid argument for plotting a background panel grid, one of either:
#'    - a logical (i.e., `TRUE` to draw the grid), or
#'    - a panel grid plotting function like `grid()`.
#'   Note that this argument replaces the `panel.first` and `panel.last`
#'   arguments from base `plot()` and tries to make the process more seemless
#'   with better default behaviour. Default is not to draw a grid.
#' @param asp the y/xy/x aspect ratio, see `plot.window`.
#' @param palette one of the following options:
#'    - NULL (default), in which case the palette will be determined by the
#'    the user's default graphics palette, e.g. "R4". See `?palette()`. Note
#'    that some internal checking is done to make sure that resulting colours
#'    match the number of groups. For larger group numbers, the "viridis"
#'    palette will be used instead.
#'    - A convenience string corresponding to one of the many palettes listed by
#'    either `palette.pals()` or `hcl.pals()`. Note that the string can be
#'    case-insensitive (e.g., "Okabe-Ito" and "okabe-ito" are both valid).
#'    - A palette-generating function. This can be "bare" (e.g.,
#'    `palette.colors`) or "closed" with a set of named arguments (e.g.,
#'    `palette.colors(palette = "Okabe-Ito", alpha = 0.5)`). Note that any
#'    unnamed arguments will be ignored and the key `n` argument, denoting the
#'    number of colours, will automatically be spliced in as the number of
#'    groups.
#' @param legend one of the following options:
#'    - NULL (default), in which case the legend will be determined by the
#'    grouping variable. If there is no group variable (i.e., `by` is NULL) then
#'    no legend is drawn. If a grouping variable is detected, then an automatic
#'    legend is drawn to the _outer_ right of the plotting area. Note that the
#'    legend title and categories will automatically be inferred from the `by`
#'    argument and underlying data.
#'    - A convenience string indicating the legend position. The string should
#'    correspond to one of the position keywords supported by the base `legend`
#'    function, e.g. "right", "topleft", "bottom", etc. In addition, `plot2`
#'    supports adding a trailing exclamation point to these keywords, e.g.
#'    "right!", "topleft!", or "bottom!". This will place the legend _outside_
#'    the plotting area and adjust the margins of the plot accordingly. Finally,
#'    users can also turn off any legend printing by specifying "none".
#'    - Logical value, where TRUE corresponds to the default case above (same 
#'    effect as specifying NULL) and FALSE turns the legend off (same effect as
#'    specifying "none").
#'    - A list or, equivalently, a dedicated `legend()` function with supported
#'    legend arguments, e.g. "bty", "horiz", and so forth.
#' @param col plotting color. Character, integer, or vector of length equal to
#'   the number of categories in the `by` variable. See `col`. Note that the
#'   default behaviour in `plot2` is to vary group colors along any variables
#'   declared in the `by` argument. Thus, specifying colors manually should not
#'   be necessary unless users wish to override the automatic colors produced by
#'   this grouping process. Typically, this would only be done if grouping
#'   features are deferred to some other graphical parameter (i.e., passing the
#'   "by" keyword to one of `pch`, `lty`, or `bg`; see below.)
#' @param pch plotting "character", i.e., symbol to use. Character, integer, or
#'   vector of length equal to the number of categories in the `by` variable.
#'   See `pch`. In addition, users can supply a special `pch = "by"` convenience
#'   argument, in which case the characters will automatically loop over the
#'   number groups. This automatic looping will begin at the global character
#'   value (i.e., `par("pch")`) and recycle as necessary.
#' @param lty line type. Character, integer, or vector of length equal to the
#'   number of categories in the `by` variable. See `lty`. In addition, users
#'   can supply a special `lty = "by"` convenience argument, in which case the
#'   line type will automatically loop over the number groups. This automatic
#'   looping will begin at the global line type value (i.e., `par("lty")`) and
#'   recycle as necessary.
#' @param bg background fill color for the open plot symbols 21:25 (see
#'   `points.default`), as well as ribbon and area plot types. For the latter
#'   group---including filled density plots---an automatic alpha transparency
#'   adjustment will be applied (see the `ribbon_alpha` argument further below).
#'   Users can also supply a special `bg = "by"` convenience argument, in which
#'   case the background fill will inherit the automatic group coloring
#'   intended for the `col` argument. Note that this grouped inheritance will
#'   persist even if the `col` defaults are themselves overridden. For example,
#'   `plot2(y ~ x | z, data = fakedata, pch = 22, col = "blue", bg = "by")`
#'   will yield filled squares with a blue border.
#' @param fill alias for `bg`. If non-NULL values for both `bg` and `fill` are
#'   provided, then the latter will be ignored in favour of the former.
#' @param cex character expansion. A numerical vector (can be a single value)
#'   giving the amount by which plotting characters and symbols should be scaled
#'   relative to the default. Note that NULL is equivalent to 1.0, while NA
#'   renders the characters invisible.
#' @param par_restore a logical value indicating whether the `par` settings
#'   prior to calling `plot2` should be restored on exit. Defaults to FALSE,
#'   which makes it possible to add elements to the plot after it has been
#'   drawn. However, note the the outer margins of the graphics device may have
#'   been altered to make space for the `plot2` legend. Users can opt out of
#'   this persistent behaviour by setting to TRUE instead. (Another option would
#'   be calling `dev.off()` to reset all `par` settings to their defaults.)
#' @param subset,na.action,drop.unused.levels arguments passed to `model.frame`
#'   when extracting the data from `formula` and `data`.
#' @param ymin,ymax minimum and maximum coordinates of interval plot types. Only
#'   used when the `type` argument is one of "pointrange", "errorbar", or
#'   "ribbon".
#' @param ribbon_alpha numeric factor modifying the opacity alpha of any ribbon
#'   shading; typically in `[0, 1]`. Default value is 0.2. Only used when
#'   `type = "ribbon"`, or when the `bg` fill argument is specified in a density
#'   plot (since filled density plots are converted to ribbon plots internally).
#' @param add logical. If TRUE, then elements are added to the current plot rather
#'   than drawing a new plot window. Note that the automatic legend for the
#'   added elements will be turned off.
#' @param ... other `graphical` parameters (see `par` and also the "Details"
#'   section of `plot`).
#'   
#' @importFrom grDevices adjustcolor palette palette.colors palette.pals hcl.colors hcl.pals
#' @importFrom graphics arrows axis box grconvertX lines par plot.default plot.new plot.window points polygon segments title
#' @importFrom utils modifyList
#' 
#' @examples
#' 
#' # save graphics paramaters to restore them later
#' op <- par()
#' 
#' 
#' # plot2 should be a drop-in replacement for (most) regular plot calls. For
#' # example:
#' 
#' par(mfrow = c(1, 2))
#' plot(0:10, main = "plot")
#' plot2(0:10, main = "plot2")
#' 
#' par(mfrow = c(2, 2))
#' plot(airquality$Day, airquality$Temp, main = "plot")
#' plot(Temp ~ Day, data = airquality, main = "plot (formula)")
#' plot2(airquality$Day, airquality$Temp, main = "plot2")
#' plot2(Temp ~ Day, data = airquality, main = "plot2 (formula)")
#' 
#' 
#' # restore graphics parameters
#' par(op)  
#' 
#' 
#' # Unlike vanilla plot, however, plot2 allows you to characterize groups 
#' # (using either the `by` argument or equivalent `|` formula syntax).
#' # Notice that we also get an automatic legend.
#' 
#' plot2(airquality$Day, airquality$Temp, by = airquality$Month)
#' plot2(Temp ~ Day | Month, airquality)
#'
#' # Use standard base plotting arguments to adjust features of your plot.
#' # For example, change `pch` (plot character) to get filled points.
#' 
#' plot2(
#'   Temp ~ Day | Month,
#'   data = airquality,
#'   pch = 16
#' )
#' 
#' # Converting to a grouped line plot is a simple matter of adjusting the
#' # `type` argument.
#' 
#' plot2(
#'   Temp ~ Day | Month,
#'   data = airquality,
#'   type = "l"
#' )
#' 
#' # The (automatic) legend position and look can be customized using
#' # appropriate arguments. Note the trailing "!" in the `legend` position
#' # argument below. This tells `plot2` to place the legend _outside_ the plot
#' # area.
#' 
#' plot2(
#'   Temp ~ Day | Month,
#'   data = airquality,
#'   type = "l",
#'   legend = legend("bottom!", title = "Month of the year", bty = "o")
#' )
#' 
#' # Regular legend position keywords without the exclamation point (i.e., for
#' # inside the plot area) should still work. Grouped density plot example:
#' 
#' plot2(
#'   density(airquality$Temp),
#'   by = airquality$Month, 
#'   legend = legend("topright", bty="o", title = "Month")
#' )
#' 
#' # The default group colours are inherited from either the "R4" or "Viridis"
#' # palettes, depending on the number of groups. However, all palettes listed
#' # by `palette.pals()` and `hcl.pals()` are supported as convenience strings,
#' # or users can supply a valid palette-generating function for finer control
#' # over transparency etc.
#' 
#' plot2(
#'   Temp ~ Day | Month,
#'   data = airquality,
#'   type = "l",
#'   palette = "Tableau 10"
#' )
#'
#' # It's possible to further customize the look of you plots using familiar
#' # arguments and base plotting theme settings (e.g., via `par`).
#'
#' par(family = "HersheySans")
#' plot2(
#'   Temp ~ Day | Month,
#'   data = airquality,
#'   type = "b", pch = 16,
#'   palette = palette.colors(palette = "Tableau 10", alpha = 0.5),
#'   main = "Daily temperatures by month",
#'   frame = FALSE, grid = TRUE
#' )
#' 
#' # For nice out-of-the-box themes, we recommend the `basetheme` package.
#' 
#' par(family = "") # revert global font change from above
#' 
#' library(basetheme)
#' basetheme("royal") # or "clean", "dark", "ink", "brutal", etc.
#' 
#' plot2(
#'   Temp ~ Day | Month,
#'   data = airquality,
#'   type = "b", pch = 15:19,
#'   palette = "Tropic",
#'   main = "Daily temperatures by month"
#' )
#' 
#' basetheme(NULL)  # back to default theme
#' 
#' @rdname plot2
#' @export
plot2 =
  function(x, ...) {
    UseMethod("plot2")
  }

#' @rdname plot2
#' @export
plot2.default = function(
    x,
    y = NULL,
    by = NULL,
    facet = NULL,
    data = NULL,
    type = "p",
    xlim = NULL,
    ylim = NULL,
    log = "",
    main = NULL, 
    sub = NULL,
    xlab = NULL,
    ylab = NULL,
    ann = par("ann"),
    axes = TRUE,
    frame.plot = axes,
    asp = NA,
    grid = NULL,
    palette = NULL,
    legend = NULL,
    pch = NULL,
    lty = NULL,
    col = NULL,
    bg = NULL,
    fill = NULL,
    cex = 1,
    par_restore = FALSE,
    ymin = NULL,
    ymax = NULL,
    ribbon_alpha = 0.2,
    add = FALSE,
    ...) {
  
  dots = list(...)
  
  if (isTRUE(add)) {
    legend = FALSE
    # main = sub = NULL
  }
  
  # Capture deparsed expressions early, before x, y and by are evaluated
  x_dep = deparse1(substitute(x))
  y_dep = if (is.null(y)) {
    deparse1(substitute(x))
  } else {
    deparse1(substitute(y))
  }
  by_dep = deparse1(substitute(by))
  facet_dep = deparse1(substitute(facet))
  if (!is.null(facet) && length(facet)==1 && facet=="by") facet = by

  ## Catch for density type: recycle through plot.density
  if (type == "density") {
    fargs = mget(ls(environment(), sorted = FALSE))
    fargs = utils::modifyList(fargs, dots)
    if (!is.null(fargs[["y"]])) {
      fargs[["y"]] = NULL
      message("\nNote: A `y` argument has been supplied, but will be ignored for density plots.\n")
    }
    fargs$type = "l"
    # explicitly turn off `default.density(x = ...)` title for
    # type = "density" plots (to make consistent with regular plot)
    if (is.null(fargs$main)) fargs$main = NA
    ## Catch for atomic density type to avoid "by" as legend title
    if (is.null(fargs[["legend.args"]][["title"]])) {
      fargs[["legend.args"]][["title"]] = by_dep
    }
    ## Another catch for bespoke legend position (if originally passed via the formula method)
    if (!is.null(fargs[["legend"]]) && !is.null(fargs[["legend.args"]])) {
      if (names(fargs[["legend"]])[1] == "") names(fargs[["legend"]])[1] = "x"
      fargs[["legend.args"]] = utils::modifyList(fargs[["legend"]], fargs[["legend.args"]])
      fargs[["legend"]] = NULL
    }
    fargs$y = fargs$ymin = fargs$ymax = fargs$ylab = fargs$xlab = NULL
    return(do.call(plot2.density, args = fargs))
  }
  
  if (is.null(y)) {
    ## Special catch for interval plots without a specified y-var
    if (type %in% c("pointrange", "errorbar", "ribbon")) {
      ymin_dep = deparse(substitute(ymin))
      ymax_dep = deparse(substitute(ymax))
      y_dep = paste0("[", ymin_dep, ", ", ymax_dep, "]")
      y = rep(NA, length(x))
      if (is.null(ylim)) ylim = range(c(ymin, ymax))
    } else {
      y = x
      x = seq_along(x)
      xlab = "Index"
    }
  }
  
  if (is.null(xlab)) xlab = x_dep
  if (is.null(ylab)) ylab = y_dep
    
  if (type == "area") {
    ymax = y
    ymin = rep.int(0, length(y))
    type = "ribbon"
  }

  xlabs = NULL
  if (type %in% c("pointrange", "errorbar", "ribbon")) {
    if (is.character(x)) x = as.factor(x)
    if (is.factor(x)) {
      ## Need to maintain order that was observed in the original data
      ## (i.e., no new sorting by factor)
      xlvls = unique(x)
      x = factor(x, levels = xlvls)
      xlabs = seq_along(xlvls)
      names(xlabs) = xlvls
      x = as.integer(x)
    }
    if (type == "ribbon") {
      if (is.null(by) && is.null(facet)) {
        xord = order(x) 
      } else if (is.null(facet)) {
        xord = order(by, x)
        by = by[xord]
      } else if (is.null(by)) {
        xord = order(facet, x)
        facet = facet[xord]
      } else {
        xord = order(by, facet, x)
        by = by[xord]
        facet = facet[xord]
      }
      x = x[xord]
      y = y[xord]
      ymin = ymin[xord]
      ymax = ymax[xord]
      rm(xord)
    }
  }

  xy = grDevices::xy.coords(x = x, y = y)
  if (is.null(xlim)) xlim = range(xy$x[is.finite(xy$x)])
  if (is.null(ylim)) ylim = range(xy$y[is.finite(xy$y)])

  if (!is.null(ymin)) ylim[1] = min(c(ylim, ymin))
  if (!is.null(ymax)) ylim[2] = max(c(ylim, ymax))


  if (!is.null(by)) {
    split_data = list(x=x, y=y)
    split_data[["ymin"]] = ymin
    split_data[["ymax"]] = ymax
    split_data[["facet"]] = facet
    split_data = lapply(split_data, split, by)
    split_data = do.call(function(...) Map("list", ...), split_data)
  } else {
    split_data = list(list(x=x, y=y, ymin=ymin, ymax=ymax, facet=facet))
  }
  
  ngrps = length(split_data)
    
  pch = by_pch(ngrps = ngrps, type = type, pch = pch)
  
  lty = by_lty(ngrps = ngrps, type = type, lty = lty)

  # palette = substitute(palette)
  col = by_col(
    ngrps = ngrps,
    col = col,
    palette = substitute(palette)
  )
  if (is.null(bg) && !is.null(fill)) bg = fill
  if (!is.null(bg) && bg == "by") {
    bg = by_col(
      ngrps = ngrps,
      col = NULL,
      palette = substitute(palette)
    )
  } else {
    bg = rep(bg, ngrps)
  }
  if (type == "ribbon") {
    if (!is.null(bg)) {
      bg = adjustcolor(bg, ribbon_alpha)
    } else if (!is.null(col)) {
      bg = adjustcolor(col, ribbon_alpha)
    }
  }
  
  # Save current graphical parameters
  opar = par(no.readonly = TRUE)
  
  ## place and draw the legend
  
  legend.args = dots[["legend.args"]]
  if (is.null(legend.args)) legend.args = list(x = NULL)
  legend = substitute(legend)
  
  if (isFALSE(legend)) {
    legend = "none"
    legend.args[["x"]] = "none"
  }
  if (isTRUE(legend)) {
    legend = NULL
  }
  
  if (is.null(by)) {
    if (is.null(legend)) {
      legend = "none"
      legend.args[["x"]] = "none"
    }
  }
  
  if ((is.null(legend) || legend != "none") && isFALSE(add)) {
    
    if (ngrps>1) {
      lgnd_labs = names(split_data)
    } else {
      lgnd_labs = ylab
    }
    
    draw_legend(
      legend = legend,
      legend.args = legend.args,
      by_dep = by_dep,
      lgnd_labs = lgnd_labs,
      type = type,
      pch = pch,
      lty = lty,
      col = col,
      bg = bg,
      cex = cex
      )
    
  } else if (legend.args[["x"]]=="none" && isFALSE(add)) {
    
    plot.new()
    
  }
  
  # Titles. Note that we include a special catch for the main title if legend is
  # "top!" (and main is specified in the first place).
  adj_title = !is.null(legend) && (legend == "top!" || (!is.null(legend.args[["x"]]) && legend.args[["x"]]=="top!"))
  if (is.null(main) || isFALSE(adj_title)) {
    title(
      main = main,
      sub = sub
    )
  } else {
    # Bump main up to make space for the legend beneath it
    title(main = main, line = 5, xpd = NA)
  }
  
  if (!is.null(facet)) {
    nfacets = unique(facet)
    par(mfrow = c(1, length(nfacets)))
    ifacet = seq_along(nfacets)
    # Bump extra space for top facet margins if a main title is also present
    if (!is.null(main)) {
      omar = par("mar")
      omar[3] = omar[3] + 2
      par(mar = omar) 
    }
    
  } else {
    nfacets = ifacet = 1
  }
  
  # Facets, axes, etc.
  for (ii in ifacet) {
    
    # See: https://github.com/grantmcdermott/plot2/issues/65
    par(mfg = c(1, ii))
    
    ## Set the plot window
    ## Problem: Passing extra args through ... (e.g., legend.args) to plot.window
    ## triggers an annoying warning about unrecognized graphical params.
    # plot.window(
    #   xlim = xlim, ylim = ylim, 
    #   asp = asp, log = log,
    #   # ...
    # )
    ## Solution: Only pass on relevant args using name checking and do.call.
    ## Idea borrowed from here: https://stackoverflow.com/a/4128401/4115816
    if (isFALSE(add)) {
      pdots = dots[names(dots) %in% names(formals(plot.default))]
      do.call(
        "plot.window",
        c(list(xlim = xlim, ylim = ylim, asp = asp, log = log), pdots)
      )
      
      # axes, plot.frame and grid
      if (isTRUE(axes)) {
        if (type %in% c("pointrange", "errorbar", "ribbon") && !is.null(xlabs)) {
          graphics::Axis(x, side = 1, at = xlabs, labels = names(xlabs))
        } else {
          graphics::Axis(x, side = 1)
        }
        if (isTRUE(frame.plot) || ii == 1) graphics::Axis(y, side = 2)
      }
      if (frame.plot) box()
      if (!is.null(grid)) {
        if (is.logical(grid)) {
          ## If grid is TRUE create a default grid. Rather than just calling the default grid()
          ## abline(... = pretty(extendrange(...)), ...) is used. Reason: pretty() is generic
          ## and works better for axes based on date/time classes. Exception: For axes in logs,
          ## resort to using grid() which is like handled better there.
          if (isTRUE(grid)) {
            gnx = gny = NULL
            if (!par("xlog")) {
              graphics::abline(v = pretty(grDevices::extendrange(x)), col = "lightgray", lty = "dotted", lwd = par("lwd"))
              gnx = NA
            }
            if (!par("ylog")) {
              graphics::abline(h = pretty(grDevices::extendrange(y)), col = "lightgray", lty = "dotted", lwd = par("lwd"))
              gny = NA
            }
            grid(nx = gnx, ny = gny)
          }
        } else {
          grid
        }
      }
      
      # facet titles
      if (!is.null(facet)) {
        mtext(paste(nfacets[[ii]]), side = 3)
      }
      # title(xlab = xlab, ylab = ylab)
      title(xlab = xlab)
      if (isTRUE(frame.plot) || ii == 1) title(ylab = ylab)
    
  }
    
    # # title(xlab = xlab, ylab = ylab)
    # title(xlab = xlab)
    # if (isTRUE(frame.plot) || ii == 1) title(ylab = ylab)
      
  }
  
  ## Now, we can finally draw all of the plot elements (points, lines, etc.)
  ## We'll do this via two nested loops:
  ##  1) Outer loop over groups, and 
  ##  2) Inner loop over facets

  ## Outer loop over the "by" groups
  for (i in seq_along(split_data)) {
    
    # Split group-level data again to grab any facets
    idata = split_data[[i]]
    ifacet = idata[["facet"]]
    if (!is.null(ifacet)) {
      idata[["facet"]] = NULL ## Don't need this anymore since we'll be splitting by ifacet
      ## Need extra catch for non-groupby data that also doesn't have ymin or
      ## ymax vars
      if (is.null(by)) {
        if (is.null(idata[["ymin"]])) idata[["ymin"]] = NULL
        if (is.null(idata[["ymax"]])) idata[["ymax"]] = NULL
      }
      idata = lapply(idata, split, ifacet)
      idata = do.call(function(...) Map("list", ...), idata)
    } else {
      idata = list(idata)
    }
    
    ## Inner loop over the "facet" variables
    for (ii in seq_along(idata)) {
      xx = idata[[ii]]$x
      yy = idata[[ii]]$y
      yymin = idata[[ii]]$ymin
      yymax = idata[[ii]]$ymax
      
      # Set the facet "window" manually
      # See: https://github.com/grantmcdermott/plot2/issues/65
      par(mfg = c(1, ii))
      
      
      # Draw the individual plot elements...
      
      ## polygons before lines
      if (type == "ribbon") {
        polygon(
          x = c(xx, rev(xx)),
          y = c(yymin, rev(yymax)),
          col = bg[i],
          border = FALSE
        )
      }
      
      ## segments/arrows before points
      if (type == "pointrange") {
        segments(
          x0 = xx,
          y0 = yymin,
          x1 = xx,
          y1 = yymax,
          col = col[i],
          lty = lty[i]
        )
      }
      if (type == "errorbar") {
        arrows(
          x0 = xx,
          y0 = yymin,
          x1 = xx,
          y1 = yymax,
          col = col[i],
          lty = lty[i],
          length = 0.05,
          angle = 90,
          code = 3
        )
      }
      
      ## now draw the points/lines
      if (type %in% c("p", "pointrange", "errorbar")) {
        points(
          x = xx,
          y = yy,
          col = col[i],
          bg = bg[i],
          # type = type, ## rather hardcode "p" to avoid warning message about "pointrange"
          type = "p",
          pch = pch[i],
          lty = lty[i],
          cex = cex
        )
      } else if (type %in% c("l", "o", "b", "c", "h", "s", "S", "ribbon")) {
        rtype = type == "ribbon"
        if (rtype) type = "l"
        lines(
          x = xx,
          y = yy,
          col = col[i],
          type = type,
          pch = pch[i],
          lty = lty[i]
        )
        if (rtype) type = "ribbon"
      } else {
        stop("`type` argument not supported.", call. = FALSE)
      }
      
    }
    
  }
  
  
  if (par_restore || !is.null(facet)) {
    on.exit(par(opar), add = TRUE)
  }
  
}

#' @rdname plot2
#' @importFrom stats as.formula model.frame
#' @export
plot2.formula = function(
    x = NULL,
    data = parent.frame(),
    type = "p",
    xlim = NULL,
    ylim = NULL,
    # log = "",
    main = NULL, 
    sub = NULL,
    xlab = NULL,
    ylab = NULL,
    ann = par("ann"),
    axes = TRUE,
    frame.plot = axes,
    asp = NA,
    grid = NULL,
    pch = NULL,
    col = NULL,
    lty = NULL,
    par_restore = FALSE,
    formula = NULL,
    subset = NULL,
    na.action = NULL,
    drop.unused.levels = TRUE,
    ...
    ) {
  
  ## formula for variables must be specified through 'x' or 'formula' but not both
  if (missing(x)) {
    if (missing(formula)) {
      stop("plot formula must be specified by either 'x' or 'formula' argument")
    }
  } else {
    if (missing(formula)) {
      formula = x
    } else {
      warning("only one of the arguments 'x' and 'formula' should be specified, defaulting to the 'formula' argument")
    }
  }

  ## catch one-sided formula ~ x or ~ x | z with no "y" variable
  if (!inherits(formula, "formula")) formula = as.formula(formula)
  no_y = length(formula) == 2L
  fml_rhs = if (no_y) 2L else 3L

  ## convert y ~ x | z to y ~ x + z for standard formula parsing
  if (length(formula[[fml_rhs]]) == 3L) {
    if (formula[[fml_rhs]][[1L]] == as.name("|")) {
      formula[[fml_rhs]][[1L]] = as.name("+")
    }
  }

  # placeholder for legend title
  legend.args = list(x = NULL)

  ## set up model frame
  m = match.call(expand.dots = FALSE)
  m = m[c(1L, match(c("formula", "data", "subset", "na.action", "drop.unused.levels"), names(m), 0L))]
  m$formula = formula
  ## need stats:: for non-standard evaluation
  m[[1L]] = quote(stats::model.frame)
  mf = eval.parent(m)
  
  ## extract variables: x, y (if any), by (if any)
  if (no_y) {
    y_loc = NULL
    x_loc = 1L
  } else {
    y_loc = 1L
    x_loc = 2L
  }
  if (NCOL(mf) < x_loc) stop("formula should specify at least one variable on the right-hand side")
  y = if (no_y) NULL else mf[, y_loc]
  x = mf[, x_loc]
  by_loc <- x_loc + 1L
  if (NCOL(mf) < by_loc) {
    by = bylab = NULL  
  } else if (NCOL(mf) == by_loc) {
    by = mf[, by_loc]
    bylab = names(mf)[by_loc]
    legend.args[["title"]] = bylab
    if (!inherits(by, "factor")) by = as.factor(by)
  } else if (NCOL(mf) > by_loc) {
    by = do.call("interaction", mf[, -c(y_loc, x_loc)])
    bylab = sprintf("interaction(%s)", paste(names(mf)[-c(y_loc, x_loc)], collapse = ", "))
    legend.args[["title"]] = bylab
  }

  ## nice axis and legend labels
  if (no_y) {
    if (is.null(ylab)) ylab = names(mf)[x_loc]
    if (is.null(xlab)) xlab = "Index"
  } else {
    if (is.null(ylab)) ylab = names(mf)[y_loc]
    if (is.null(xlab)) xlab = names(mf)[x_loc]
  }
  
  plot2.default(
    x = x, y = y, by = by,
    data = data,
    type = type,
    xlim = xlim,
    ylim = ylim,
    # log = "",
    main = main,
    sub = sub,
    xlab = xlab,
    ylab = ylab,
    ann = ann,
    axes = axes,
    frame.plot = frame.plot,
    asp = asp,
    grid = grid,
    legend.args = legend.args,
    pch = pch,
    col = col,
    lty = lty,
    par_restore = par_restore,
    ...
    )

}

#' @importFrom methods as
#' @importFrom stats update
#' @rdname plot2
#' @export
plot2.density = function(
    x = NULL,
    by = NULL,
    facet = NULL,
    type = c("l", "area"),
    xlim = NULL,
    ylim = NULL,
    # log = "",
    main = NULL,
    sub = NULL,
    xlab = NULL,
    ylab = NULL,
    ann = par("ann"),
    axes = TRUE,
    frame.plot = axes,
    asp = NA,
    grid = NULL,
    pch = NULL,
    col = NULL,
    lty = NULL,
    bg = NULL,
    fill = NULL,
    par_restore = FALSE,
    ...
  ) {

  type = match.arg(type)
  ## override if bg = "by"
  if (!is.null(bg) || !is.null(fill)) type = "area"

  if (inherits(x, "density")) {
    object = x
    legend.args = list(x = NULL)
    # Grab by label to pass on legend title to plot2.default
    legend.args[["title"]] = deparse(substitute(by))
  } else {
    ## An internal catch for non-density objects that were forcibly
    ## passed to plot2.density (e.g., via a one-side formula)
    object = stats::density(x)
    legend.args = list(...)[["legend.args"]]
  }

  by_names = facet_names = NULL
  if (is.null(by) && is.null(facet)) {
    x = object$x
    y = object$y
  } else {
    x = eval(str2lang(object$data.name), envir = parent.frame())
    if (is.null(facet) || identical(by, facet)) {
      split_x = split(x, f = by) 
    } else if (is.null(by)) {
      split_x = split(x, f = facet)
    } else {
      split_x = split(x, f = list(by, facet), sep = "::")
    }
    # joint bandwidth
    bw_type = as.list(object$call[-1])[["bw"]]
    if (is.null(bw_type)) bw_type = stats::bw.nrd0 else bw_type = str2lang(paste0("bw.", bw))
    xs_mask = vapply(split_x, length, numeric(1)) > 1
    bws = vapply(split_x[xs_mask], bw_type, numeric(1))
    bw = mean(bws, na.rm = TRUE)
    #
    split_object = lapply(split_x, function(xx) update(object, x = xx, bw = bw))
    by_names = names(split_object)
    if (all(grepl("::", by_names))) {
      by_names = strsplit(by_names, "::")
      facet_names = sapply(by_names, `[[`, 2)
      facet_names = tryCatch(as(facet_names, class(facet)), error = function(e) facet_names)
      by_names = sapply(by_names, `[[`, 1)
    } else if (identical(by, facet)) {
      facet_names = by_names ## yuck
    } else if (is.null(by) && !is.null(facet)) {
      facet_names = names(split_object)
    }
    by_names = tryCatch(as(by_names, class(by)), error = function(e) if (class(by)=="factor") as.factor(by_names) else by_names)
    # need to coerce facet variables to factors for faceting to work properly later on
    facet_names = tryCatch(as.factor(facet_names), error = function(e) facet_names)
    
    split_object = lapply(seq_along(split_object), function(ii) {
      lst = list(
        x = split_object[[ii]]$x,
        y = split_object[[ii]]$y,
        n = split_object[[ii]]$n
      )
      if (!is.null(by)) {
        lst$by = rep_len(by_names[ii], length.out = length(lst$x))
      } else {
        lst$by = NULL
      }
      if (!is.null(facet)) {
        lst$facet = rep_len(facet_names[ii], length.out = length(lst$x))
      } else {
        lst$facet = NULL
      }
      return(lst)
    })
    ## combine element by element
    res = do.call(Map, c(c, split_object))
    ## now pull out the individual vectors
    x = res[["x"]]
    y = res[["y"]]
    by = res[["by"]]
    facet = res[["facet"]]
    bw = sprintf("%.4g", bw)
    n = res[["n"]]
    if (is.null(xlab)) {
      if (length(by_names) > 3) {
        n = c(n[1:3], "...")
      }
      n = paste0("[", paste(n, collapse = ", "), "]")
      xlab = paste0("N = ", n, "   Joint Bandwidth = ", bw)
    }
  }
  if (type == "area") {
    ymin = rep(0, length(y))
    ymax = y
    # set extra legend params to get bordered boxes with fill
    legend.args[["x.intersp"]] = 1.25
    legend.args[["lty"]] = 0
    legend.args[["pt.lwd"]] = 1
  }

  ## axes range
  if (is.null(xlim)) xlim = range(x)
  if (is.null(ylim)) ylim = range(y)

  ## nice labels and titles
  if (is.null(ylab)) ylab = "Density"
  if (is.null(xlab)) xlab = paste0("N = ", object$n, "   Bandwidth = ", sprintf("%.4g", object$bw))
  if (is.null(main)) main = paste0(paste(object$call, collapse = "(x = "), ")")

  plot2.default(
    x = x, y = y, by = by, facet = facet,
    type = type,
    xlim = xlim,
    ylim = ylim,
    # log = "",
    main = main,
    sub = sub,
    xlab = xlab,
    ylab = ylab,
    ann = ann,
    axes = axes,
    frame.plot = frame.plot,
    asp = asp,
    grid = grid,
    legend.args = legend.args,
    pch = pch,
    col = col,
    bg = bg,
    fill = fill,
    lty = lty,
    par_restore = par_restore,
    ...
    )

}

