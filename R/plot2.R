#' @title Lightweight extension of the base R plotting function
#'   
#' @description Extends base R's
#'   default plotting function, particularly as it applies to scatter and
#'   line plots with grouped data. For example, `plot2` makes it easy to plot
#'   different categories of a dataset in a single function call and highlight
#'   these categories (groups) using modern colour palettes. Coincident with
#'   this grouping support, `plot2` also produces automatic legends with scope
#'   for further customization. While the package also offers several other
#'   minor enhancements, it tries as far as possible to be a drop-in replacement
#'   for the equivalent base plot function. Users should generally be able to
#'   swap a valid `plot` call with `plot2` without any changes to the output.
#' 
#' @md
#' @param x,y the x and y arguments provide the x and y coordinates for the
#'   plot. Any reasonable way of defining the coordinates is acceptable. See
#'   the function xy.coords for details. If supplied separately, they must be
#'   of the same length.
#' @param by the grouping variable that you want to categorize (i.e., colour)
#'   the plot by.
#' @param formula a `formula` that may also include a grouping variable after a
#'   "|", such as `y ~ x | z`. Note that the `formula` and `x` arguments should
#'   not be specified in the same call.
#' @param data a data.frame (or list) from which the variables in formula
#'   should be taken. A matrix is converted to a data frame.
#' @param type character string giving the type of plot desired. Options are:
#'   - The same set of 1-character values supported by plot: "p" for points, "l"
#'   for lines, "b" for both points and lines, "c" for empty points joined by
#'   lines, "o" for overplotted points and lines, "s" and "S" for stair steps
#'   and "h" for histogram-like vertical lines. "n" does not produce
#'   any points or lines.
#'   - Additional plot2 types: "pointrange", "errorbar", and "ribbon" for
#'   drawing these interval plot types. (Note that specifying "ribbon" for
#'   objects of class `density` will yield a density plot with a shaded
#'   interior.)
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
#' @param bg background (fill) color for the open plot symbols 21:25: see
#'   `points.default`. In addition, users can supply a special `bg = "by"`
#'   convenience argument, in which case the background color will inherit the
#'   automatic group coloring intended for the `col` parameter.
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
#'   shading; typically in [0, 1]. Default value is 0.2. Only used when
#'   `type = "ribbon"`.
#' @param ... 	other `graphical` parameters (see `par` and also the "Details"
#'   section of `plot`).
#'   
#' @importFrom grDevices adjustcolor palette palette.colors palette.pals hcl.colors hcl.pals
#' @importFrom graphics axis box grconvertX lines par plot.new plot.window points title
#' 
#' @examples
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
#' #' plot2(airquality$Day, airquality$Temp, main = "plot2")
#' plot2(Temp ~ Day, data = airquality, main = "plot2 (formula)")
#' 
#' dev.off() # reset to default layout
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
    cex = 1,
    par_restore = FALSE,
    ymin = NULL,
    ymax = NULL,
    ribbon_alpha = 0.2,
    ...) {
  
  dots = list(...)
  
  # Capture deparsed expressions early, before x, y and by are evaluated
  x_dep = deparse(substitute(x))
  y_dep = if (is.null(y)) {
    deparse(substitute(x))
  } else {
    deparse(substitute(y))
  }
  by_dep = deparse(substitute(by))
  
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
      if (is.null(by)) {
        xord = order(x) 
      } else {
        xord = order(by, x)
        by = by[xord]
      }
      x = x[xord]
      y = y[xord]
      ymin = ymin[xord]
      ymax = ymax[xord]
      rm(xord)
    }
  }
  
  if (is.null(xlim)) xlim = range(x, na.rm = TRUE)
  if (is.null(ylim)) ylim = range(y, na.rm = TRUE)

  if (!is.null(ymin)) ylim[1] = min(c(ylim, ymin))
  if (!is.null(ymax)) ylim[2] = max(c(ylim, ymax))


  if (!is.null(by)) {
    l = list(x=x, y=y)
    l[["ymin"]] = ymin
    l[["ymax"]] = ymax
    split_data = lapply(l, split, by)
    split_data = do.call(function(...) Map("list", ...), split_data)
  } else {
    split_data = list(list(x=x, y=y, ymin = ymin, ymax = ymax))
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
  
  # legend
  
  w = h = outer_right = outer_bottom = NULL
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
  
  if (is.null(legend)) {
    legend.args[["x"]] = "right!"
  } else if (is.character(legend)) {
    legend.args = utils::modifyList(legend.args, list(x = legend))
  } else if (class(legend) %in% c("call", "name")) {
    largs = as.list(legend)
    if (is.null(largs[["x"]])) {
      lnms = names(largs)
      # check second position b/c first will be a symbol 
      if (is.null(lnms)) {
        largs = stats::setNames(largs, c("", "x"))
      } else if (length(largs)>=2 && lnms[2] == "") {
        lnms[2] = "x"
        largs = stats::setNames(largs, lnms)
      } else {
        largs[["x"]] = "right!"
      }
    }
    # Finally, combine with any pre-existing legend args (e.g., title from the by label)
    legend.args = utils::modifyList(legend.args, largs, keep.null = TRUE)
  }
  ## Use `!exists` rather than `is.null` for title in case user specified no title
  if (!exists("title", where = legend.args)) legend.args[["title"]] = by_dep
  if (is.null(legend.args[["pch"]])) legend.args[["pch"]] = pch
  if (is.null(legend.args[["lty"]])) legend.args[["lty"]] = lty
  if (is.null(legend.args[["col"]])) legend.args[["col"]] = col
  if (is.null(legend.args[["bty"]])) legend.args[["bty"]] = "n"
  if (is.null(legend.args[["horiz"]])) legend.args[["horiz"]] = FALSE
  if (is.null(legend.args[["xpd"]])) legend.args[["xpd"]] = NA
  if (is.null(legend.args[["pt.bg"]])) legend.args[["pt.bg"]] = bg
  if (
    type %in% c("p", "pointrange", "errorbar") &&
      (length(col) == 1 || length(cex) == 1) &&
      is.null(legend.args[["pt.cex"]])
  ) {
    legend.args[["pt.cex"]] = cex
  }
  if (type=="ribbon") {
    if (is.null(legend.args[["pch"]])) legend.args[["pch"]] = 22
    if (is.null(legend.args[["pt.cex"]])) legend.args[["pt.cex"]] = 3.5
    if (is.null(legend.args[["pt.lwd"]])) legend.args[["pt.lwd"]] = 0
    # if (is.null(legend.args[["pt.bg"]])) legend.args[["pt.bg"]] = adjustcolor(col, alpha = 0.2)
    if (is.null(legend.args[["x.intersp"]])) {
      if (isTRUE(legend.args[["horiz"]])) {
        legend.args[["x.intersp"]] = 1
      } else {
        legend.args[["x.intersp"]] = 1.25
      }
    }
    if (is.null(legend.args[["y.intersp"]])) legend.args[["y.intersp"]] = 1.25
    if (is.null(legend.args[["seg.len"]])) legend.args[["seg.len"]] = 1.25
  }
  
  if (legend.args[["x"]] != "none") {
    
    if (ngrps>1) {
      lgnd_labs = names(split_data)
    } else {
      lgnd_labs = ylab
    }
    if (is.null(legend.args[["legend"]])) {
      legend.args[["legend"]] = lgnd_labs
    } else if (length(lgnd_labs) != length(eval(legend.args[["legend"]]))) {
      warning(
        "\nUser-supplied legend labels do not match the number of groups.\n",
        "Defaulting to automatic labels determined by the group splits in `by`,\n"
        )
      legend.args[["legend"]] = lgnd_labs
    }
    
    # Catch to avoid recursive offsets, e.g., repeated plot2 calls with
    # "bottom!" legend position.
    par(omd = c(0,1,0,1))
    
    ## Legend to outer side of plot
    if (grepl("right!$|left!$", legend.args[["x"]])) {
      
      outer_right = grepl("right!$", legend.args[["x"]])
      
      # Margins of the plot (the first is the bottom margin)
      if (outer_right) {
        par(mar=c(par("mar")[1:3], 0.1)) # remove right inner margin space
      } else if (par("mar")[4]==0.1) {
        par(mar=c(par("mar")[1:3], 2.1)) # revert right margin if outer left
      }
      
      plot.new()
      
      # legend.args[["x"]] = "left"
      ## Switch position anchor (we'll adjust relative to the _opposite_ side below)
      if (outer_right) legend.args[["x"]] = gsub("right!$", "left", legend.args[["x"]])
      if (!outer_right) legend.args[["x"]] = gsub("left!$", "right", legend.args[["x"]])
      
      legend.args[["horiz"]] = FALSE
      
      lgnd = legend(
        0, 0, 
        bty    = legend.args[["bty"]],
        legend = legend.args[["legend"]],
        pch    = legend.args[["pch"]],
        lty    = legend.args[["lty"]],
        col    = legend.args[["col"]],
        title  = legend.args[["title"]],
        xpd    = legend.args[["xpd"]],
        plot   = FALSE
      )
      # calculate side margin width in ndc
      w = grconvertX(lgnd$rect$w, to="ndc") - grconvertX(0, to="ndc")
      ## differing adjustments depending on side
      if (outer_right) {
        w = w*1.5
        par(omd = c(0, 1-w, 0, 1))
        legend.args[["inset"]] = c(1.025, 0)
      } else {
        w = w + grconvertX(par("mgp")[1], from = "lines", to = "ndc") # extra space for y-axis title
        par(omd = c(w, 1, 0, 1))
        legend.args[["inset"]] = c(1+w, 0)
      }
      
    ## Legend at the outer top or bottom of plot
    } else if (grepl("bottom!$|top!$", legend.args[["x"]])) {
      
      outer_bottom = grepl("bottom!$", legend.args[["x"]])
      
      # Catch to reset right margin if previous legend position was "right!"
      if (par("mar")[4]== 0.1) par(mar=c(par("mar")[1:3], 2.1)) 
      
      plot.new()
      
      ## Switch position anchor (we'll adjust relative to the _opposite_ side below)
      if (outer_bottom) legend.args[["x"]] = gsub("bottom!$", "top", legend.args[["x"]])
      if (!outer_bottom) legend.args[["x"]] = gsub("top!$", "bottom", legend.args[["x"]])
      
      legend.args[["horiz"]] = TRUE
      
      # Catch for horizontal ribbon legend that we might have missed above 
      if (type=="ribbon" && isTRUE(legend.args[["horiz"]])) legend.args[["x.intersp"]] = .5
      
      lgnd = legend(
        0, 0,
        bty    = legend.args[["n"]],
        legend = legend.args[["legend"]],
        horiz  = legend.args[["horiz"]],
        pch    = legend.args[["pch"]],
        lty    = legend.args[["lty"]],
        col    = legend.args[["col"]],
        title  = legend.args[["title"]],
        plot   = FALSE
      )
      # calculate bottom margin height in ndc
      h = grconvertX(lgnd$rect$h, to="ndc") - grconvertX(0, to="ndc")
      ## differing adjustments depending on side
      if (outer_bottom) {
        legend.args[["inset"]] = c(0, 1+2*h)
        par(omd = c(0,1,0+h,1))
      } else {
        legend.args[["inset"]] = c(0, 1)
        par(omd = c(0,1,0,1-h))
      }
      
    } else {
      # Catch to reset right margin if previous legend position was "right!"
      if (par("mar")[4] == 0.1) par(mar=c(par("mar")[1:3], par("mar")[2]-2)) 
      legend.args[["inset"]] = 0
      plot.new()
    }
    
    do.call("legend", legend.args)
    
  } else if(legend.args[["x"]]=="none") {
    
    plot.new()
    
  }
  
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
  pdots = dots[names(dots) %in% names(formals(graphics::plot.default))]
  do.call(
    "plot.window",
    c(list(xlim = xlim, ylim = ylim, asp = asp, log = log), pdots)
  )
  
  # axes, plot.frame and grid
  if (axes) {
    if (type %in% c("pointrange", "errorbar", "ribbon") && !is.null(xlabs)) {
      axis(1, at = xlabs, labels = names(xlabs))
    } else {
      axis(1)
    }
    axis(2)
  }
  if (frame.plot) box()
  if (!is.null(grid)) {
    if (is.logical(grid)) {
      if (isTRUE(grid)) grid()
    } else {
      grid
    }
  }

  # polygons before lines
  if (type == "ribbon") {
    invisible(
      lapply(
        seq_along(split_data),
        function(i) {
          graphics::polygon(
            x = c(split_data[[i]]$x, rev(split_data[[i]]$x)),
            y = c(split_data[[i]]$ymin, rev(split_data[[i]]$ymax)),
            # col = adjustcolor(col[i], ribbon_alpha),
            col = bg[i],
            border = FALSE
          )
        }
      )
    )
  } 
  ## segments/arrows before points
  if (type == "pointrange") { 
    invisible(
      lapply(
        seq_along(split_data),
        function(i) {
          graphics::segments(
            x0 = split_data[[i]]$x,
            y0 = split_data[[i]]$ymin,
            x1 = split_data[[i]]$x,
            y1 = split_data[[i]]$ymax,
            col = col[i],
            lty = lty[i]
          )
        }
      )
    )
  } 
  if (type == "errorbar") {
    invisible(
      lapply(
        seq_along(split_data),
        function(i) {
          graphics::arrows(
            x0 = split_data[[i]]$x,
            y0 = split_data[[i]]$ymin,
            x1 = split_data[[i]]$x,
            y1 = split_data[[i]]$ymax,
            col = col[i],
            lty = lty[i],
            length = 0.05,
            angle = 90,
            code = 3
          )
        }
      )
    )
  } 
  
  ## now draw the points/lines
  if (type %in% c("p", "pointrange", "errorbar")) {
    invisible(
      lapply(
        seq_along(split_data),
        function(i) {
          points(
            x = split_data[[i]]$x,
            y = split_data[[i]]$y,
            col = col[i],
            bg = bg[i],
            # type = type, ## rather hardcode "p" to avoid warning message about "pointrange"
            type = "p",
            pch = pch[i],
            lty = lty[i],
            cex = cex
          )
        }
      )
    )
  } else if (type %in% c("l", "o", "b", "c", "h", "s", "S", "ribbon")) {
    if (type=="ribbon") type = "l"
    invisible(
      lapply(
        seq_along(split_data),
        function(i) {
          lines(
            x = split_data[[i]]$x,
            y = split_data[[i]]$y,
            col = col[i],
            type = type,
            pch = pch[i],
            lty = lty[i]
          )
        }
      )
    )
  } else {
    stop("`type` argument not supported.", call. = FALSE)
  }

  # Titles. Note that we include a special catch for the main title if legend is
  # "top!" (and main is specified in the first place).
  if (is.null(main) || is.null(outer_bottom) || isTRUE(outer_bottom)) {
    title(
      xlab = xlab,
      ylab = ylab,
      main = main,
      sub = sub
    )
  } else {
    title(
      xlab = xlab,
      ylab = ylab,
      sub = sub
    )
    # Bump main up to make space for the legend beneath it
    title(main = main, line = 5, xpd = NA)
  }
  
  
  if (par_restore) {
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

  ## convert y ~ x | z to y ~ x + z for standard formula parsing
  if (!inherits(formula, "formula")) formula = as.formula(formula)
  if (length(formula[[3L]]) == 3L) {
    if (formula[[3L]][[1L]] == as.name("|")) {
      formula[[3L]][[1L]] = as.name("+")
    }
  }
  
  # placeholder for legend title
  legend.args = list(x = NULL)

  ## extract x, y, by (if any) from formula
  m = match.call(expand.dots = FALSE)
  m = m[c(1L, match(c("formula", "data", "subset", "na.action", "drop.unused.levels"), names(m), 0L))]
  m$formula = formula
  ## need stats:: for non-standard evaluation
  m[[1L]] = quote(stats::model.frame)
  mf = eval.parent(m)
  if (NCOL(mf) < 2L) {
    stop("plot formula should specify exactly at least two variables")
  } 
  y = mf[,1L]
  x = mf[,2L]
  by = bylab = NULL
  if (NCOL(mf) == 3L) {
    by = mf[,3L]
    bylab = names(mf)[3L]
    legend.args[["title"]] = bylab
    if (!inherits(by, "factor")) {
      by = as.factor(by)
    }
  } else if (NCOL(mf) > 3L) {
    by = do.call("interaction", mf[, -(1L:2L)])
    bylab = sprintf("interaction(%s)", paste(names(mf)[-(1L:2L)], collapse = ", "))
    legend.args[["title"]] = bylab
  }

  ## nice axis and legend labels
  if (is.null(ylab)) ylab = names(mf)[1L]
  if (is.null(xlab)) xlab = names(mf)[2L]
  
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
    type = c("l", "ribbon"),
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
    par_restore = FALSE,
    ...
    ) {
  
  type = match.arg(type)
  ## override if bg = "by"
  if (!is.null(bg)) type = "ribbon"
  
  object = x
  legend.args = list(x = NULL)
  # Grab by label to pass on legend title to plot2.default
  legend.args[["title"]] = deparse(substitute(by))
  
  if (is.null(by)) {
    x = object$x
    y = object$y
  } else {
    x = eval(str2lang(object$data.name), envir = parent.frame())
    split_x = split(x, f = by)
    # joint bandwidth
    bw_type = as.list(object$call[-1])[["bw"]]
    if (is.null(bw_type)) bw_type = stats::bw.nrd0 else bw_type = str2lang(paste0("bw.", bw))
    xs_mask = vapply(split_x, length, numeric(1)) > 1
    bws = vapply(split_x[xs_mask], bw_type, numeric(1))
    bw = mean(bws, na.rm = TRUE)
    #
    split_object = lapply(split_x, function(xx) update(object, x = xx, bw = bw))
    by_names = names(split_object)
    by_names = tryCatch(as(by_names, class(by)), error = function(e) by_names)
    split_object = lapply(seq_along(split_object), function(ii) {
      lst = list(
        x = split_object[[ii]]$x,
        y = split_object[[ii]]$y,
        n = split_object[[ii]]$n
      )
      lst$by = rep_len(by_names[ii], length.out = length(lst$x))
      return(lst)
    })
    ## combine element by element
    res = do.call(Map, c(c, split_object))
    ## now pull out the individual vectors
    x = res[["x"]]
    y = res[["y"]]
    by = res[["by"]]
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
  if (type=="ribbon") {
    ymin = rep(0, length(y))
    ymax = y
    # set extra legend params to get bordered boxes with fill
    legend.args[["x.intersp"]] = 1.25
    legend.args[["lty"]] = 0
    legend.args[["pt.lwd"]] = 1
    # legend.args[["seg.len"]] = 2
  } else {
    ymin = ymax = NULL
  }
  
  ## axes range
  if (is.null(xlim)) xlim = range(x)
  if (is.null(ylim)) ylim = range(y)

  ## nice labels and titles
  if (is.null(ylab)) ylab = "Density"
  if (is.null(xlab)) xlab = paste0("N = ", object$n, "   Bandwidth = ", sprintf("%.4g", object$bw))
  if (is.null(main)) main = paste0(paste(object$call, collapse = "(x = "), ")")

  plot2.default(
    x = x, y = y, by = by,
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
    lty = lty,
    par_restore = par_restore,
    ymin = ymin,
    ymax = ymax,
    ...
    )

}
