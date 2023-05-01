#' @title Lightweight extension of the base R plotting function
#'   
#' @description `plot2` extends the functionality of base R's
#'   default (2D) `plot` function, particularly as it applies to scatter and
#'   line plots with grouped data. For example, `plot2` makes it easy to plot
#'   different categories of a dataset in a single function call and highlight
#'   these categories (groups) using modern colour palettes. Coincident with
#'   this grouping support, `plot2` also produces automatic legends with scope
#'   for further customization. While the package also offers several other
#'   minor enhancements, it tries as far as possible to be a drop-in replacement
#'   for the equivalent base plot function. Users should generally be able to
#'   swap a valid `plot` call with `plot2` without any changes to the output.
#' 
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
#' @param type 1-character string giving the type of plot desired. The
#'   following values are possible, for details, see plot: "p" for points, "l"
#'   for lines, "b" for both points and lines, "c" for empty points joined by
#'   lines, "o" for overplotted points and lines, "s" and "S" for stair steps
#'   and "h" for histogram-like vertical lines. Finally, "n" does not produce
#'   any points or lines.
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
#'   the plot.
#' @param grid a panel grid plotting function like `grid()`. This argument
#'   replaces the `panel.first` and `panel.last` arguments from base `plot()`
#'   and tries to make the process more seemless with better default behaviour.
#' @param asp the y/xy/x aspect ratio, see `plot.window`.
#' @param palette one of the following options:
#'    - NULL (default), in which case the palette will be determined by the
#'    number of groups. If this number is equal to 8 or less, then R's default
#'    ("R4") colour palette will be used. For larger group numbers, the
#'    "Viridis" palette will be used instead.
#'    - A convenience string corresponding to one of the many palettes listed by
#'    either `palette.pals()` or `hcl.pals()`. Note that the string can be
#'    case-insensitive (e.g., "Okabe-Ito" and "okabe-ito" are both valid).
#'    - A palette-generating function. This can be "bare" (e.g.,
#'    `palette.colors`) or "closed" with a set of named arguments (e.g.,
#'    `palette.colors(palette = "Okabe-Ito", alpha = 0.5)`). Note that any
#'    unnamed arguments will be ignored and the key `n` argument, denoting the
#'    number of colours, will automatically be spliced in as the number of
#'    groups.
#' @param legend.position one of the position keywords supported by `legend`.
#'   In addition, `plot2` supports adding an exclamation point to two keywords
#'   in particular: "bottom!" and "right!". These will place the legend outside
#'   of the plotting area and adjust the margins of the plot accordingly. If no
#'   legend is desired, then the user can also specify "none".
#' @param legend.args list of additional arguments passed on to `legend`. At
#'   the moment, only "bty", "horiz", "xpd", and "title" are supported.
#' @param pch plotting "character", i.e., symbol to use. Character, integer, or
#'   vector of length equal to the number of categories in the `by` variable.
#'   See `pch`. In addition, users can supply a special `pch = "by"` convenience
#'   argument, in which case the characters will automatically loop over the
#'   number groups. This automatic looping will begin at the global character
#'   value (i.e., `par("pch")`) and recycle as necessary.
#' @param col plotting color. Character, integer, or vector of length equal to
#'   the number of categories in the `by` variable. See `col`.
#' @param lty line type. Character, integer, or vector of length equal to the
#'   number of categories in the `by` variable. See `lty`. In addition, users
#'   can supply a special `lty = "by"` convenience argument, in which case the
#'   line type will automatically loop over the number groups. This automatic
#'   looping will begin at the global line type value (i.e., `par("lty")`) and
#'   recycle as necessary.
#' @param par_restore a logical value indicating whether the `par` settings
#'   prior to calling `plot2` should be restored on exit. Defaults to FALSE,
#'   which makes it possible to add elements to the plot after it has been
#'   drawn. However, note the the outer margins of the graphics device may have
#'   been altered to make space for the `plot2` legend. Users can opt out of
#'   this persistent behaviour by setting to TRUE instead. (Another option would
#'   be calling `dev.off()` to reset all `par` settings to their defaults.)
#' @param subset,na.action,drop.unused.levels arguments passed to `model.frame`
#'   when extracting the data from `formula` and `data`.
#' @param ... 	other `graphical` parameters (see `par` and also the "Details"
#'   section of `plot`).
#'   
#' @importFrom grDevices hcl.colors hcl.pals palette.colors palette.pals
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
#' # appropriate arguments. Note the trailing "!" in the `legend.position`
#' # argument below. This tells `plot2` to place the legend _outside_ the plot
#' # area.
#' 
#' plot2(
#'   Temp ~ Day | Month,
#'   data = airquality,
#'   type = "l",
#'   legend.position = "bottom!", # "right!" (default), "none", or "topleft", etc.
#'   legend.args = list(title = "Month of the year", bty = "o")
#' )
#' 
#' # Regular legend position keywords without the exclamation point (i.e., for
#' # inside the plot area) should still work. Grouped density plot example:
#' 
#' plot2(
#'   density(airquality$Temp),
#'   by = airquality$Month, 
#'   legend.position = "topright", legend.args = list(bty="o", title = "Month")
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
#'   frame.plot = FALSE, grid = grid()
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
    legend.position = NULL,
    legend.args = list(),
    pch = NULL,
    col = NULL,
    lty = NULL,
    par_restore = FALSE,
    ...) {
  
  if (is.null(y)) {
    y = x
    x = seq_along(x)
    xlab = "Index"
    }
  
  if (is.null(xlab)) xlab = deparse(substitute(x))
  if (is.null(ylab)) ylab = deparse(substitute(y))
  if (is.null(legend.args$title)) ltitle = deparse(substitute(by))
    
  if (is.null(xlim)) xlim = range(x, na.rm = TRUE)
  if (is.null(ylim)) ylim = range(y, na.rm = TRUE)
  
  if (!is.null(by)) {
    split_data = lapply(list(x=x, y=y), split, by)
    split_data = do.call(function(...) Map("list", ...), split_data)
  } else {
    split_data = list(list(x=x, y=y))
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
  
  # Save current graphical parameters
  opar = par(no.readonly = TRUE)
  # par_restore = FALSE
  
  # legend
  
  bty = ifelse(!is.null(legend.args[["bty"]]), legend.args[["bty"]], "n")
  horiz = ifelse(!is.null(legend.args[["horiz"]]), legend.args[["horiz"]], FALSE)
  xpd = ifelse(!is.null(legend.args[["xpd"]]), legend.args[["xpd"]], NA)
  
  ltitle = w = h = NULL
  if(!is.null(legend.args[["title"]])) ltitle = legend.args[["title"]]

  if (is.null(legend.position)) {
    legend.position = ifelse(length(split_data)>1, "right!", "none")
  }
  
  if (legend.position!="none") {
    
    if (exists("title", where = legend.args)) {
      ltitle = legend.args[["title"]]
    } else if (!is.null(by)) {
      ltitle = deparse(substitute(by))
    }
    
    if (ngrps>1) {
      legend = names(split_data)
    } else {
      legend = ylab
    }
    
    if (legend.position=="right!") {
      
      # par_restore = TRUE
      # Margins of the plot (the first is the bottom margin)
      par(mar=c(par("mar")[1:3], 0.1)) # remove right inner margin space
    
      plot.new()
      
      pos_anchor = "left"
      horiz = FALSE
      
      lgnd = legend(
        0, 0, bty = "n", legend = legend,
        col = col,
        pch = pch,
        lty = lty,
        title = ltitle,
        plot = FALSE
      )
      # calculate right margin width in ndc
      w = grconvertX(lgnd$rect$w, to="ndc") - grconvertX(0, to="ndc")
      w = w*1.5
      inset = c(1.025, 0)
      par(omd = c(0, 1-w, 0, 1))
      
    } else if (legend.position=="bottom!") {
      
      # par_restore = TRUE
      # Catch to reset right margin if previous legend position was "right!"
      if (par("mar")[4]== 0.1) par(mar=c(par("mar")[1:3], par("mar")[2]-2)) 
      
      plot.new()
      
      pos_anchor = "top"
      horiz = TRUE
      
      lgnd = legend(
        0, 0, bty = "n", legend = legend,
        horiz = horiz,
        pch = pch,
        lty = lty,
        col = col,
        # title = ltitle,
        plot = FALSE
      )
      # calculate bottom margin height in ndc
      h = grconvertX(lgnd$rect$h, to="ndc") - grconvertX(0, to="ndc")
      inset = c(0, 1+2.5*h)
      par(omd = c(0,1,0+h,1))
      
    } else {
      pos_anchor = legend.position
      inset = 0
      horiz = horiz
      plot.new()
    }
    
    legend(
      x = pos_anchor, 
      inset = inset,
      legend = legend,
      bty = bty,
      horiz = horiz,
      pch = pch,
      lty = lty,
      col = col,
      xpd = xpd,
      title = ltitle
    )
    
  } else if(legend.position=="none") {
    
    plot.new()
    
  }
  
  # plot window
  plot.window(
    xlim = xlim, ylim = ylim, 
    asp = asp, log = log,
    ...
  )
  
  # axes, plot.frame and grid
  if (axes) {
    axis(1)
    axis(2)
  }
  if (frame.plot) box()
  if (!is.null(grid)) grid
  
  # draw the points/lines
  if (type == "p") {
    invisible(
      lapply(
        seq_along(split_data),
        function(i) {
          points(
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
  }
  if (type %in% c("l", "o", "b", "c", "h", "s", "S")) {
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
  }

  title(
    xlab = xlab,
    ylab = ylab,
    main = main,
    sub = sub
    )
  
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
    # palette = NULL,
    legend.position = NULL,
    legend.args = list(),
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
  by = NULL
  if (NCOL(mf) == 3L) {
    by = mf[,3L]
    bylab = names(mf)[3L]
    if (!inherits(by, "factor")) {
      by = as.factor(by)
    }
  } else if (NCOL(mf) > 3L) {
    by = do.call("interaction", mf[, -(1L:2L)])
    bylab = sprintf("interaction(%s)", paste(names(mf)[-(1L:2L)], collapse = ", "))
  }

  ## nice axis and legend labels
  if (is.null(ylab)) ylab = names(mf)[1L]
  if (is.null(xlab)) xlab = names(mf)[2L]
  if (!is.null(by) && !exists("title", where = legend.args)) legend.args$title = bylab
  
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
    # palette = palette,
    legend.position = legend.position,
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
    type = "l",
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
    # palette = NULL,
    legend.position = NULL,
    legend.args = list(),
    pch = NULL,
    col = NULL,
    lty = NULL,
    par_restore = FALSE,
    ...
    ) {
  
  object = x
  
  if (is.null(by)) {
    x = object$x
    y = object$y
  } else {
    if (!exists("title", where = legend.args)) legend.args$title = deparse(substitute(by))
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
    # palette = palette,
    legend.position = legend.position,
    legend.args = legend.args,
    pch = pch,
    col = col,
    lty = lty,
    par_restore = par_restore,
    ...
    )

}
