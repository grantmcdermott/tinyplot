#' @title Lightweight extension of the base R plotting function
#'  
#' @description
#' Enhances the base \code{\link[graphics]{plot}} function. Supported features
#' include automatic legends and facets for grouped data, additional plot types,
#' theme customization, and so on. Users can call either `tinyplot()`, or its
#' shorthand alias `plt()`.
#' 
#' @md
#' @param x,y the x and y arguments provide the x and y coordinates for the
#'   plot. Any reasonable way of defining the coordinates is acceptable; most
#'   likely the names of existing vectors or columns of data frames. See the
#'   'Examples' section below, or the function
#'   \code{\link[grDevices]{xy.coords}} for details. If supplied separately, `x`
#'   and `y` must be of the same length.
#' @param by grouping variable(s). The default behaviour is for groups to be
#'   represented in the form of distinct colours, which will also trigger an
#'   automatic legend. (See `legend` below for customization options.) However,
#'   groups can also be presented through other plot parameters (e.g., `pch` or
#'   `lty`) by passing an appropriate "by" keyword; see Examples. Note that
#'   continuous (i.e., gradient) colour legends are also supported if the user
#'   passes a numeric or integer to `by`.
#' @param facet the faceting variable(s) that you want arrange separate plot
#'   windows by. Can be specified in various ways:
#'   - In "atomic" form, e.g. `facet = fvar`. To facet by multiple variables in 
#'   atomic form, simply interact them, e.g.
#'   `interaction(fvar1, fvar2)` or `factor(fvar1):factor(fvar2)`.
#'   - As a one-sided formula, e.g. `facet = ~fvar`. Multiple variables can be
#'   specified in the formula RHS, e.g. `~fvar1 + fvar2` or `~fvar1:fvar2`. Note
#'   that these multi-variable cases are _all_ treated equivalently and
#'   converted to `interaction(fvar1, fvar2, ...)` internally. (No distinction
#'   is made between different types of binary operators, for example, and so
#'   `f1+f2` is treated the same as `f1:f2`, is treated the same as `f1*f2`,
#'   etc.)
#'   - As a two-side formula, e.g. `facet = fvar1 ~ fvar2`. In this case, the
#'   facet windows are arranged in a fixed grid layout, with the formula LHS
#'   defining the facet rows and the RHS defining the facet columns. At present
#'   only single variables on each side of the formula are well supported. (We
#'   don't recommend trying to use multiple variables on either the LHS or RHS
#'   of the two-sided formula case.)
#'   - As a special `"by"` convenience keyword, in which case facets will match
#'   the grouping variable(s) passed to `by` above.
#' @param facet.args an optional list of arguments for controlling faceting
#'   behaviour. (Ignored if `facet` is NULL.) Supported arguments are as
#'   follows:
#'   - `nrow`, `ncol` for overriding the default "square" facet window
#'   arrangement. Only one of these should be specified, but `nrow` will take
#'   precedence if both are specified together. Ignored if a two-sided formula
#'   is passed to the main `facet` argument, since the layout is arranged in a
#'   fixed grid.
#'   - `fmar` a vector of form `c(b,l,t,r)` for controlling the base margin
#'   between facets in terms of lines. Defaults to the value of `tpar("fmar")`,
#'   which should be `c(1,1,1,1)`, i.e. a single line of padding around each
#'   individual facet, assuming it hasn't been overridden by the user as part
#'   their global \code{\link[tinyplot]{tpar}} settings. Note some automatic
#'   adjustments are made for certain layouts, and depending on whether the plot
#'   is framed or not, to reduce excess whitespace. See
#'   \code{\link[tinyplot]{tpar}} for more details.
#'   - `cex`, `font`, `col`, `bg`, `border` for adjusting the facet title text
#'   and background. Default values for these arguments are inherited from
#'   \code{\link[tinyplot]{tpar}} (where they take a "facet." prefix, e.g. 
#'   `tpar("facet.cex")`). The latter function can also be used to set these
#'   features globally for all `tinyplot` plots.
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
#'   - Additional tinyplot types: "density" for densities, "polygon" for
#'   polygons,  "pointrange" or "errorbar" for segment intervals, and "polygon",
#'   "ribbon" or "area" for polygon intervals (where area plots are a special
#'   case of ribbon plots with `ymin` set to 0 and `ymax` set to `y`; see below).
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
#'   arguments from base `plot()` and tries to make the process more seamless
#'   with better default behaviour. The default behaviour is determined by (and
#'   can be set globally through) the value of `tpar("grid")`.
#' @param asp the y/xy/x aspect ratio, see `plot.window`.
#' @param palette one of the following options:
#'    - NULL (default), in which case the palette will be chosen according to
#'    the class and cardinality of the "by" grouping variable. For non-ordered
#'    factors or strings with a reasonable number of groups, this will inherit
#'    directly from the user's default \code{\link[grDevices]{palette}} (e.g.,
#'    "R4"). In other cases, including ordered factors and high cardinality, the
#'    "Viridis" palette will be used instead. Note that a slightly restricted
#'    version of the "Viridis" palette---where extreme color values have been
#'    trimmed to improve visual perception---will be used for ordered factors
#'    and continuous variables. In the latter case of a continuous grouping
#'    variable, we also generate a gradient legend swatch.
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
#'    function, e.g. "right", "topleft", "bottom", etc. In addition, `tinyplot`
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
#'   default behaviour in `tinyplot` is to vary group colors along any variables
#'   declared in the `by` argument. Thus, specifying colors manually should not
#'   be necessary unless users wish to override the automatic colors produced by
#'   this grouping process. Typically, this would only be done if grouping
#'   features are deferred to some other graphical parameter (i.e., passing the
#'   "by" keyword to one of `pch`, `lty`, `lwd`, or `bg`; see below.)
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
#' @param lwd line width. Numeric scalar or vector of length equal to the
#'   number of categories in the `by` variable. See `lwd`. In addition, users
#'   can supply a special `lwd = "by"` convenience argument, in which case the
#'   line width will automatically loop over the number of groups. This
#'   automatic looping will be centered at the global line width value (i.e.,
#`   par("lwd")`) and pad on either side of that.
#' @param bg background fill color for the open plot symbols 21:25 (see
#'   `points.default`), as well as ribbon and area plot types. For the latter
#'   group---including filled density plots---an automatic alpha transparency
#'   adjustment will be applied (see the `ribbon.alpha` argument further below).
#'   Users can also supply either one of two special convenience arguments that
#'   will cause the background fill to inherit the automatic grouped coloring
#'   behaviour of `col`:
#'   
#'   - `bg = "by"` will insert a background fill that inherits the main color
#'   mappings from `col`.
#'   - `by = <numeric[0,1]>` (i.e., a numeric in the range `[0,1]`) will insert
#'   a background fill that inherits the main color mapping(s) from `col`, but
#'   with added alpha-transparency.
#'   
#'   For both of these convenience arguments, note that the (grouped) `bg`
#'   mappings will persist even if the (grouped) `col` defaults are themselves
#'   overridden. This can be useful if you want to preserve the grouped palette
#'   mappings by background fill but not boundary color, e.g. filled points. See
#'   examples. 
#' @param fill alias for `bg`. If non-NULL values for both `bg` and `fill` are
#'   provided, then the latter will be ignored in favour of the former.
#' @param alpha a numeric in the range `[0,1]` for adjusting the alpha channel
#'   of the color palette, where 0 means transparent and 1 means opaque. Use
#'   fractional values, e.g. `0.5` for semi-transparency.
#' @param cex character expansion. A numerical vector (can be a single value)
#'   giving the amount by which plotting characters and symbols should be scaled
#'   relative to the default. Note that NULL is equivalent to 1.0, while NA
#'   renders the characters invisible.
#' @param restore.par a logical value indicating whether the
#'   \code{\link[graphics]{par}} settings prior to calling `tinyplot` should be
#'   restored on exit. Defaults to FALSE, which makes it possible to add
#'   elements to the plot after it has been drawn. However, note the the outer
#'   margins of the graphics device may have been altered to make space for the
#'   `tinyplot` legend. Users can opt out of this persistent behaviour by
#'   setting to TRUE instead. See also [get_saved_par] for another option to
#'   recover the original \code{\link[graphics]{par}} settings, as well as
#'   longer discussion about the trade-offs involved.
#' @param subset,na.action,drop.unused.levels arguments passed to `model.frame`
#'   when extracting the data from `formula` and `data`.
#' @param ymin,ymax minimum and maximum coordinates of interval plot types. Only
#'   used when the `type` argument is one of "pointrange", "errorbar", or
#'   "ribbon".
#' @param ribbon.alpha numeric factor modifying the opacity alpha of any ribbon
#'   shading; typically in `[0, 1]`. Only used when `type = "ribbon"`, or when
#'   the `bg` fill argument is specified in a density plot (since filled density
#'   plots are converted to ribbon plots internally). If an an applicable plot
#'   type is called but no explicit value is provided, then will default to
#'   `tpar("ribbon.alpha")` (i.e., probably `0.2` unless this has been
#'   overridden by the user in their global settings.)
#' @param add logical. If TRUE, then elements are added to the current plot rather
#'   than drawing a new plot window. Note that the automatic legend for the
#'   added elements will be turned off.
#' @param file character string giving the file path for writing a plot to disk.
#'   If specified, the plot will not be displayed interactively, but rather sent
#'   to the appropriate external graphics device (i.e.,
#'   \code{\link[grDevices]{png}}, \code{\link[grDevices]{jpeg}},
#'   \code{\link[grDevices]{pdf}}, or \code{\link[grDevices]{svg}}). As a point
#'   of convenience, note that any global parameters held in `(t)par` are
#'   automatically carried over to the external device and don't need to be
#'   reset (in contrast to the conventional base R approach that requires
#'   manually opening and closing the device). The device type is determined by
#'   the file extension at the end of the provided path, and must be one of
#'   ".png", ".jpg" (".jpeg"), ".pdf", or ".svg". (Other file types may be
#'   supported in the future.) The file dimensions can be controlled by the
#'   corresponding `width` and `height` arguments below, otherwise will fall
#'   back to the `"file.width"` and `"file.height"` values held in
#'   \code{\link[tinyplot]{tpar}} (i.e., both defaulting to 7 inches, and where
#'   the default resolution for bitmap files is also specified as 300
#'   DPI).
#' @param width numeric giving the plot width in inches. Together with `height`,
#'  typically used in conjunction with the `file` argument above, overriding the
#'  default values held in `tpar("file.width", "file.height")`. If either `width`
#'  or `height` is specified, but a corresponding `file` argument is not
#'  provided as well, then a new interactive graphics device dimensions will be
#'  opened along the given dimensions. Note that this interactive resizing may
#'  not work consistently from within an IDE like RStudio that has an integrated
#'  graphics windows.
#' @param height numeric giving the plot height in inches. Same considerations as
#'  `width` (above) apply, e.g. will default to `tpar("file.height")` if not
#'  specified. 
#' @param ... other graphical parameters. See \code{\link[graphics]{par}} or
#'   the "Details" section of \code{\link[graphics]{plot}}.
#'
#' @returns No return value, called for side effect of producing a plot.
#'   
#' @details
#' Disregarding the enhancements that it supports, `tinyplot` tries as far as
#' possible to mimic the behaviour and syntax logic of the original base
#' \code{\link[graphics]{plot}} function. Users should therefore be able to swap
#' out existing `plot` calls for `tinyplot` (or its shorthand alias `plt`),
#' without causing unexpected changes to the output.
#'   
#' @importFrom grDevices adjustcolor colorRampPalette extendrange palette palette.colors palette.pals hcl.colors hcl.pals xy.coords png jpeg pdf svg dev.off dev.new dev.list
#' @importFrom graphics abline arrows axis Axis box grconvertX grconvertY lines par plot.default plot.new plot.window points polygon segments title mtext text rect
#' @importFrom utils modifyList head tail
#' @importFrom stats na.omit
#' @importFrom tools file_ext
#' 
#' @examples
#' #' 
#' aq = transform(
#'   airquality,
#'   Month = factor(Month, labels = month.abb[unique(Month)])
#' )
#' 
#' # In most cases, `tinyplot` should be a drop-in replacement for regular
#' # `plot` calls. For example:
#' 
#' op = tpar(mfrow = c(1, 2))
#' plot(0:10, main = "plot")
#' tinyplot(0:10, main = "tinyplot")
#' tpar(op) # restore original layout
#' 
#' # Aside: `tinyplot::tpar()` is a (near) drop-in replacement for `par()`
#' 
#' # Unlike vanilla plot, however, tinyplot allows you to characterize groups 
#' # using either the `by` argument or equivalent `|` formula syntax.
#' 
#' with(aq, tinyplot(Day, Temp, by = Month)) ## atomic method
#' tinyplot(Temp ~ Day | Month, data = aq)   ## formula method
#' 
#' # (Notice that we also get an automatic legend.)
#' 
#' # You can also use the equivalent shorthand `plt()` alias if you'd like to
#' # save on a few keystrokes
#' 
#' plt(Temp ~ Day | Month, data = aq) ## shorthand alias
#'
#' # Use standard base plotting arguments to adjust features of your plot.
#' # For example, change `pch` (plot character) to get filled points and `cex`
#' # (character expansion) to increase their size.
#' 
#' tinyplot(
#'   Temp ~ Day | Month,
#'   data = aq,
#'   pch = 16,
#'   cex = 2
#' )
#' 
#' # We can add alpha transparency for overlapping points
#' 
#' tinyplot(
#'   Temp ~ Day | Month,
#'   data = aq,
#'   pch = 16,
#'   cex = 2,
#'   alpha = 0.3
#' )
#' 
#' # To get filled points with a common solid background color, use an 
#' # appropriate plotting character (21:25) and combine with one of the special
#' # `bg` convenience arguments.
#' tinyplot(
#'   Temp ~ Day | Month,
#'   data = aq,
#'   pch = 21,     # use filled circles
#'   cex = 2,
#'   bg = 0.3,     # numeric in [0,1] adds a grouped background fill with transparency
#'   col = "black" # override default color mapping; give all points a black border
#' )
#' 
#' # Converting to a grouped line plot is a simple matter of adjusting the
#' # `type` argument.
#' 
#' tinyplot(
#'   Temp ~ Day | Month,
#'   data = aq,
#'   type = "l"
#' )
#' 
#' # Similarly for other plot types, including some additional ones provided
#' # directly by tinyplot, e.g. density plots or internal plots (ribbons, 
#' # pointranges, etc.)
#' 
#' tinyplot(
#'   ~ Temp | Month,
#'   data = aq,
#'   type = "density",
#'   fill = "by"
#' )
#' 
#' # Facet plots are supported too. Facets can be drawn on their own...
#' 
#' tinyplot(
#'   Temp ~ Day,
#'   facet = ~ Month, 
#'   data = aq,
#'   type = "area",
#'   main = "Temperatures by month"
#' )
#' 
#' # ... or combined/contrasted with the by (colour) grouping.
#' 
#' aq = transform(aq, Summer = Month %in% c("Jun", "Jul", "Aug"))
#' tinyplot(
#'   Temp ~ Day | Summer,
#'   facet = ~ Month, 
#'   data = aq,
#'   type = "area",
#'   palette = "dark2",
#'   main = "Temperatures by month and season"
#' )
#' 
#' # Users can override the default square window arrangement by passing `nrow`
#' # or `ncol` to the helper facet.args argument. Note that we can also reduce
#' # axis label repetition across facets by turning the plot frame off.
#' 
#' tinyplot(
#'   Temp ~ Day | Summer,
#'   facet = ~ Month, facet.args = list(nrow = 1),
#'   data = aq,
#'   type = "area",
#'   palette = "dark2",
#'   frame = FALSE,
#'   main = "Temperatures by month and season"
#' )
#' 
#' # Use a two-sided formula to arrange the facet windows in a fixed grid.
#' # LHS -> facet rows; RHS -> facet columns
#' 
#' aq$hot = ifelse(aq$Temp>=75, "hot", "cold")
#' aq$windy = ifelse(aq$Wind>=15, "windy", "calm")
#' tinyplot(
#'  Temp ~ Day,
#'  facet = windy ~ hot,
#'  data = aq
#' )
#' 
#' # The (automatic) legend position and look can be customized using
#' # appropriate arguments. Note the trailing "!" in the `legend` position
#' # argument below. This tells `tinyplot` to place the legend _outside_ the plot
#' # area.
#' 
#' tinyplot(
#'   Temp ~ Day | Month,
#'   data = aq,
#'   type = "l",
#'   legend = legend("bottom!", title = "Month of the year", bty = "o")
#' )
#' 
#' # The default group colours are inherited from either the "R4" or "Viridis"
#' # palettes, depending on the number of groups. However, all palettes listed
#' # by `palette.pals()` and `hcl.pals()` are supported as convenience strings,
#' # or users can supply a valid palette-generating function for finer control
#' 
#' tinyplot(
#'   Temp ~ Day | Month,
#'   data = aq,
#'   type = "l",
#'   palette = "tableau"
#' )
#'
#' # It's possible to further customize the look of you plots using familiar
#' # arguments and base plotting theme settings (e.g., via `(t)par`).
#'
#' tpar(family = "HersheySans", las = 1)
#' tinyplot(
#'   Temp ~ Day | Month,
#'   data = aq,
#'   type = "b", pch = 16,
#'   palette = "tableau", alpha = 0.5,
#'   main = "Daily temperatures by month",
#'   frame = FALSE, grid = TRUE
#' )
#' 
#' # Note: For more examples and a detailed walkthrough, please see the
#' # introductory tinyplot tutorial available online:
#' # https://grantmcdermott.com/tinyplot/vignettes/intro_tutorial.html
#' 
#' @rdname tinyplot
#' @export
tinyplot =
  function(x, ...) {
    UseMethod("tinyplot")
  }

#' @rdname tinyplot
#' @export
tinyplot.default = function(
    x,
    y = NULL,
    by = NULL,
    facet = NULL,
    facet.args = NULL,
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
    lwd = NULL,
    col = NULL,
    bg = NULL,
    fill = NULL,
    alpha = NULL,
    cex = 1,
    restore.par = FALSE,
    ymin = NULL,
    ymax = NULL,
    ribbon.alpha = NULL,
    add = FALSE,
    file = NULL,
    width = NULL,
    height = NULL,
    ...) {
  
  dots = list(...)

  # Write plot to output file (if requested)
  if (!is.null(file)) {
    filepath = file
    filewidth = width
    fileheight = height
    if (is.null(filewidth)) filewidth = .tpar[["file.width"]]
    if (is.null(fileheight)) fileheight = .tpar[["file.height"]]
    fileres = .tpar[["file.res"]]
    # catch to close interactive device if one isn't already open
    fkdev = is.null(dev.list())
    # grab existing device pars to pass on to next one
    dop = par(no.readonly = TRUE)
    # close interactive device if not already open
    if (isTRUE(fkdev)) dev.off()
    exttype = file_ext(filepath)
    if (exttype == "jpg") exttype = "jpeg"
    switch(exttype,
      png = png(filepath, width = filewidth, height = fileheight, units = "in", res = fileres),
      jpeg = jpeg(filepath, width = filewidth, height = fileheight, units = "in", res = fileres),
      pdf = pdf(filepath, width = filewidth, height = fileheight),
      svg = svg(filepath, width = filewidth, height = fileheight),
      stop("\nUnsupported file extension. Only '.png', '.jpg', '.pdf', or '.svg' are allowed.\n")
    )
    dop$new = FALSE # catch for some interfaces
    par(dop)
    on.exit(dev.off(), add = TRUE)
    # else statement below for interactive plot with user-specified width/height
  } else if (!is.null(width) || !is.null(height)) {
    devwidth = width
    devheight = height
    # if one of width or height is missing, set equal to the other
    if (is.null(devwidth)) devwidth = devheight
    if (is.null(devheight)) devheight = devwidth
    # catch to close interactive device if one isn't already open
    fkdev = is.null(dev.list())
    # grab existing device pars to pass on to next one
    dop = par(no.readonly = TRUE)
    # close interactive device if not already open
    if (isTRUE(fkdev)) dev.off()
    dev.new(width = devwidth, height = devheight)
    dop$new = FALSE # catch for some interfaces
    par(dop)
  }
  
  # Adding to the previous plot?
  if (isTRUE(add)) {
    legend = FALSE
    # main = sub = xlab = ylab = NULL ## Rather do this later
  }
  
  # Save current graphical parameters
  opar = par(no.readonly = TRUE)
  if (restore.par || !is.null(facet)) {
    if (!is.null(file) || !is.null(width) || !is.null(height)) {
      opar$new = FALSE # catch for some interfaces
    }
    on.exit(par(opar), add = TRUE)
  }
  # set_orig_par(opar)
  set_saved_par(when = "before", opar)
  
  # catch for adding to existing facet plot
  # if (!is.null(facet) && isTRUE(add)) par(tpar("last_facet_par"))
  if (!is.null(facet) && isTRUE(add)) (
    # par(get_last_facet_par())
    par(get_saved_par(when = "after"))
  )
  
  # Capture deparsed expressions early, before x, y and by are evaluated
  x_dep = deparse1(substitute(x))
  y_dep = if (is.null(y)) {
    deparse1(substitute(x))
  } else {
    deparse1(substitute(y))
  }
  by_dep = deparse1(substitute(by))
  facet_dep = deparse1(substitute(facet))
  if (!is.null(facet) && length(facet)==1 && facet=="by") {
    by = as.factor(by) ## if by==facet, then both need to be factors
    facet = by
  } else if (!is.null(facet) && inherits(facet, "formula")) {
    facet = get_facet_fml(facet, data = data)
    if (isTRUE(attr(facet, "facet_grid"))) {
      facet.args[["nrow"]] = attr(facet, "facet_nrow")
    }
  }

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
    if (is.null(fargs[["legend_args"]][["title"]])) {
      fargs[["legend_args"]][["title"]] = by_dep
    }
    ## Another catch for bespoke legend position (if originally passed via the formula method)
    if (!is.null(fargs[["legend"]]) && !is.null(fargs[["legend_args"]])) {
      if (is.atomic(fargs[["legend"]])) {
        fargs[["legend"]] = list(x = fargs[["legend"]])
      } else if (!is.list(fargs[["legend"]])) {
        fargs[["legend"]] = as.list(fargs[["legend"]])
      }
      if (is.null(names(fargs[["legend"]])[1]) || names(fargs[["legend"]])[1] == "") {
        names(fargs[["legend"]])[1] = "x"
      }
      fargs[["legend_args"]] = modifyList(fargs[["legend"]], fargs[["legend_args"]])
      fargs[["legend"]] = NULL
    }
    fargs$y = fargs$ymin = fargs$ymax = fargs$ylab = fargs$xlab = NULL
    return(do.call(tinyplot.density, args = fargs))
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
  
  was_area_type = FALSE # flag to keep track for some legend adjustments below  
  if (type == "area") {
    ymax = y
    ymin = rep.int(0, length(y))
    type = "ribbon"
    was_area_type = TRUE 
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
        facet_grid = attr(facet, "facet_grid")
        xord = order(facet, x)
        facet = facet[xord]
        attr(facet, "facet_grid") = facet_grid
      } else {
        facet_grid = attr(facet, "facet_grid")
        xord = order(by, facet, x)
        by = by[xord]
        facet = facet[xord]
        attr(facet, "facet_grid") = facet_grid
      }
      x = x[xord]
      y = y[xord]
      ymin = ymin[xord]
      ymax = ymax[xord]
      rm(xord)
    }
  }

  xy = xy.coords(x = x, y = y)
  if (is.null(xlim)) xlim = range(xy$x[is.finite(xy$x)])
  if (is.null(ylim)) ylim = range(xy$y[is.finite(xy$y)])

  if (!is.null(ymin)) ylim[1] = min(c(ylim, ymin))
  if (!is.null(ymax)) ylim[2] = max(c(ylim, ymax))

  by_ordered = FALSE
  by_continuous = !is.null(by) && inherits(by, c("numeric", "integer"))
  # manual overrides with warning
  if (isTRUE(by_continuous) && type %in% c("l", "b", "o", "ribbon", "polygon")) {
    warning("\nContinuous legends not supported for this plot type. Reverting to discrete legend.")
    by_continuous = FALSE
  } else if (!is.null(by)){
    by_ordered = is.ordered(by)
  }
  
  if (!is.null(by) && !by_continuous) {
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
  
  lwd = by_lwd(ngrps = ngrps, type = type, lwd = lwd)

  # palette = substitute(palette)
  col = by_col(
    ngrps = ngrps,
    col = col,
    palette = substitute(palette),
    gradient = by_continuous,
    ordered = by_ordered,
    alpha = alpha
  )
  if (is.null(bg) && !is.null(fill)) bg = fill
  if (!is.null(bg) && length(bg)==1 && is.numeric(bg) && bg>=0 && bg <=1) {
    alpha = bg
    bg = "by"
  }
  if (!is.null(bg) && length(bg)==1 && bg == "by") {
    bg = by_col(
      ngrps = ngrps,
      col = NULL,
      palette = substitute(palette),
      gradient = by_continuous,
      ordered = by_ordered,
      alpha = alpha
    )
  } else if (length(bg) != ngrps) {
    bg = rep(bg, ngrps)
  }
  if (type == "ribbon") {
    if (is.null(ribbon.alpha)) ribbon.alpha = .tpar[["ribbon.alpha"]]
    if (!is.null(bg)) {
      bg = adjustcolor(bg, ribbon.alpha)
    } else if (!is.null(col)) {
      bg = adjustcolor(col, ribbon.alpha)
    }
  }
  
  if (isTRUE(by_continuous)) {
    ## Identify the pretty break points for our labels
    nlabs = 5
    ncolors = length(col)
    ubyvar = unique(by)
    byvar_range = range(ubyvar)
    pbyvar = pretty(byvar_range, n = nlabs)
    pbyvar = pbyvar[pbyvar >= byvar_range[1] & pbyvar <= byvar_range[2]]
    # optional thinning
    if (length(ubyvar)==2 && all(ubyvar %in% pbyvar)) {
      pbyvar = ubyvar
    } else if (length(pbyvar)>nlabs) {
      pbyvar = pbyvar[seq_along(pbyvar) %% 2 == 0]
    }
    ## Find the (approximate) location of our pretty labels
    pidx = rescale_num(c(byvar_range, pbyvar), to = c(1, ncolors))[-c(1:2)]
    pidx = round(pidx)
    lgnd_labs = rep(NA, times = ncolors)
    lgnd_labs[pidx] = pbyvar
  }
  
  # Determine the number and arrangement of facets.
  # Note: We're do this up front, so we can make some adjustments to legend cex
  #   next (if there are facets). But the actual drawing of the facets will only
  #   come later.
  if (!is.null(facet)) {
    
    facets = sort(unique(facet))
    ifacet = seq_along(facets)
    nfacets = length(facets)
    
    if (isTRUE(add)) {
      
      omfrow = par("mfrow")
      nfacet_rows = omfrow[1]
      nfacet_cols = omfrow[2]
      
    } else {
      
      if (!is.null(facet.args[["nrow"]])) {
        nfacet_rows = facet.args[["nrow"]]
        nfacet_cols = ceiling(nfacets/nfacet_rows)
      } else if (!is.null(facet.args[["ncol"]])) {
        nfacet_cols = facet.args[["ncol"]]
        nfacet_rows = ceiling(nfacets/nfacet_cols)
      } else {
        # default is a square arrangement for nfacets > 3
        if (nfacets > 3) {
          nfacet_cols = ceiling(sqrt(nfacets))
          nfacet_rows = ceiling(nfacets/nfacet_cols)
        } else {
          nfacet_rows = 1L
          nfacet_cols = nfacets
        }
      }
      
    }
    
    # determine "outside" facets for selected axis printing if frame = FALSE
    oxaxis = tail(ifacet, nfacet_cols)
    oyaxis = seq(1, nfacets, by = nfacet_cols)
    
    # legend cex adjustment for facet plots
    # see: https://stat.ethz.ch/pipermail/r-help/2017-August/448431.html
    if (nfacet_rows >= 3 || nfacet_cols >= 3) {
      cex_fct_adj = 0.66
    } else if (nfacet_rows == 2 && nfacet_cols == 2) {
      cex_fct_adj = 0.83
    } else {
      cex_fct_adj = 1
    }
    
  } else {
    # no facet case
    facets = ifacet = nfacets = oxaxis = oyaxis = cex_fct_adj = 1
    
  }
  
  #
  ## Global plot elements (legend and titles)
  #
  
  # place and draw the legend
  has_legend = FALSE # simple indicator variable for later use
  
  if (!exists("legend_args")) {
    legend_args = dots[["legend_args"]]
  }
  if (is.null(legend_args)) legend_args = list(x = NULL)
  legend = substitute(legend)
  
  if (isFALSE(legend)) {
    legend = "none"
  } else if (isTRUE(legend)) {
    legend = NULL
  }
  if (!is.null(legend) && legend == "none") {
    legend_args[["x"]] = "none"
  }
  
  if (is.null(by)) {
    if (is.null(legend)) {
      legend = "none"
      legend_args[["x"]] = "none"
    }
  }
  
  if ((is.null(legend) || legend != "none") && isFALSE(add)) {
    
    if (isFALSE(by_continuous)) {
      if (ngrps>1) {
        lgnd_labs = names(split_data)
      } else {
        lgnd_labs = ylab
      }
    }
    
    has_sub = !is.null(sub)
    
    if (isTRUE(was_area_type)) {
      legend_args[["pt.lwd"]] = par("lwd")
      legend_args[["lty"]] = 0
    }
    
    draw_legend(
      legend = legend,
      legend_args = legend_args,
      by_dep = by_dep,
      lgnd_labs = lgnd_labs,
      type = type,
      pch = pch,
      lty = lty,
      lwd = lwd,
      col = col,
      bg = bg,
      gradient = by_continuous,
      cex = cex * cex_fct_adj,
      has_sub = has_sub
    )
    
    has_legend = TRUE
    
  } else if (legend_args[["x"]]=="none" && isFALSE(add)) {
    
    omar = par("mar")
    ooma = par("oma")
    topmar_epsilon = 0.1
    
    # Catch to avoid recursive offsets, e.g. repeated tinyplot calls with
    # "bottom!" legend position.
    
    ## restore inner margin defaults
    ## (in case the plot region/margins were affected by the preceding tinyplot call)
    if (any(ooma != 0)) {
      if ( ooma[1] != 0 & omar[1] == par("mgp")[1] + 1*par("cex.lab") ) omar[1] = 5.1
      if ( ooma[2] != 0 & omar[2] == par("mgp")[1] + 1*par("cex.lab") ) omar[2] = 4.1
      if ( ooma[3] == topmar_epsilon & omar[3] != 4.1 ) omar[3] = 4.1
      if ( ooma[4] != 0 & omar[4] == 0 ) omar[4] = 2.1
      par(mar = omar)
    }
    ## restore outer margin defaults (with a catch for custom mfrow plots)
    if (all(par("mfrow") == c(1, 1))) {
      par(omd = c(0,1,0,1))
    }
     
    # clean up for now
    rm(omar, ooma, topmar_epsilon)
    
    # Draw new plot 
    plot.new()
    
  }
  
  # Titles. Only draw these if add = FALSE
  if (isFALSE(add)) {
    # main title
    # Note that we include a special catch for the main title if legend is
    # "top!" (and main is specified in the first place).
    legend_eval = tryCatch(eval(legend), error = function(e) NULL)
    # Extra bit of footwork if user passed legend = legend(...) instead of
    # legend = list(...), since the call environment is tricky
    if (is.null(legend_eval)) {
      legend_eval = tryCatch(paste0(legend)[[2]], error = function(e) NULL)
    }
    adj_title = !is.null(legend) && (legend == "top!" || (!is.null(legend_args[["x"]]) && legend_args[["x"]]=="top!") || (is.list(legend_eval) && legend_eval[[1]]=="top!") )
    if (is.null(main) || isFALSE(adj_title)) {
      title(
        main = main,
        sub = sub
      )
    } else {
      # For the "top!" legend case, bump main title up to make space for the
      # legend beneath it: Take the normal main title line gap (i.e., 1.7 lines)
      # and add the difference between original top margin and new one (i.e.,
      # which should equal the height of the new legend). Note that we also
      # include a 0.1 epsilon bump, which we're using to reset the tinyplot
      # window in case of recursive "top!" calls. (See draw_legend code.)
      title(main = main, line = par("mar")[3] - opar[["mar"]][3] + 1.7 + 0.1)
      title(sub = sub)
    }
    # Axis titles
    title(xlab = xlab, ylab = ylab)
  }
  
  #
  ## Facet windows
  #
  
  omar = NULL # Placeholder variable for now, which we re-assign as part of facet margins
  
  if (!is.null(facet) && isFALSE(add)) {
    
    if (is.null(omar)) omar = par("mar")
    
    # Grab some of the customizable facet args that we'll be using later
    facet_rect = FALSE
    facet_text = .tpar[["facet.cex"]]
    facet_font = .tpar[["facet.font"]]
    facet_col = .tpar[["facet.col"]]
    facet_bg = .tpar[["facet.bg"]]
    facet_border = .tpar[["facet.border"]]
    if (!is.null(facet.args)) {
      if (!is.null(facet.args[["cex"]])) facet_text = facet.args[["cex"]]
      if (!is.null(facet.args[["col"]])) facet_col = facet.args[["col"]]
      if (!is.null(facet.args[["font"]])) facet_font = facet.args[["font"]]
      if (!is.null(facet.args[["bg"]])) facet_bg = facet.args[["bg"]]
      if (!is.null(facet.args[["border"]])) facet_border = facet.args[["border"]]
    }
    if (!is.null(facet_bg) || !is.null(facet_border)) facet_rect = TRUE
    
    # Need extra adjustment to top margin if facet titles have "\n" newline
    # separator. (Note that we'll also need to take account for this in the
    # individual facet margins / gaps further below.)
    facet_newlines = lengths(gregexpr("\n", grep("\\n", facets, value = TRUE)))
    # if (length(facet_newlines)==0) facet_newlines = 0
    # omar[3] = omar[3] + max(facet_newlines)
    facet_newlines = ifelse(length(facet_newlines)==0, 0, max(facet_newlines))
    omar[3] = omar[3] + facet_newlines * facet_text / cex_fct_adj
    # apply the changes
    par(mar = omar)

  }
  
  # Now draw the individual facet windows (incl. axes, grid lines, and facet titles)
  # Skip if adding to an existing plot
  
  if (isFALSE(add)) {
    
    if (nfacets > 1) {
      # Set facet margins (i.e., gaps between facets)
      if (is.null(facet.args[["fmar"]])) {
        fmar = tpar("fmar")
      } else {
        if (length(facet.args[["fmar"]]) != 4) {
          warning(
            "`fmar` has to be a vector of length four, e.g.",
            "`facet.args = list(fmar = c(b,l,t,r))`.",
            "\n",
            "Resetting to fmar = c(1,1,1,1) default.",
            "\n"
          )
          fmar = tpar("fmar")
        } else {
          fmar = facet.args[["fmar"]]
        }
      }
      # We need to adjust for n>=3 facet cases for correct spacing...
      if (nfacets >= 3) {
        ## ... exception for 2x2 cases
        if (!(nfacet_rows==2 && nfacet_cols==2)) fmar = fmar*.75
      }
      # Extra reduction if no plot frame to reduce whitespace
      if (isFALSE(frame.plot)) {
        fmar = fmar - 0.5
      }
      
      ooma = par("oma")
      
      # Bump top margin down for facet titles
      fmar[3] = fmar[3] + 1
      if (isTRUE(attr(facet, "facet_grid"))) {
        fmar[3] = max(0, fmar[3] - 1)
        # Indent for RHS facet_grid title strip if "right!" legend
        if (has_legend && ooma[4]>0) ooma[4] = ooma[4] + 1
      }
      fmar[3] = fmar[3] + facet_newlines * facet_text/cex_fct_adj
      
      omar = par("mar")
      
      # Now we set the margins. The trick here is that we simultaneously adjust
      # inner (mar) and outer (oma) margins by the same amount, but in opposite
      # directions, to preserve the overall facet and plot centroids. 
      nmar = (fmar+.1)/cex_fct_adj
      noma = (ooma+omar-fmar-.1)/cex_fct_adj
      # Catch in case of negative oma values. (Probably only occurs with some
      # user-supplied tpar(lmar) values and a "left!" positioned legend.)
      if (any(noma<0)) {
        noma_orig = noma
        noma[noma<0] = 0
        # noma_diff = noma-noma_orig
        # nmar = nmar + noma_diff
      }
      # apply changes
      par(oma = noma)
      par(mar = nmar)
      
      # Now that the margins have been set, arrange facet rows and columns based
      # on our earlier calculations.
      par(mfrow = c(nfacet_rows, nfacet_cols))
    }
    
    ## Loop over the individual facet windows and draw the plot region
    ## components (axes, titles, box, grid, etc.)
    for (ii in ifacet) {
    
      # See: https://github.com/grantmcdermott/tinyplot/issues/65
      if (nfacets > 1) {
        mfgi = ceiling(ii/nfacet_cols)
        mfgj = ii %% nfacet_cols
        if (mfgj==0) mfgj = nfacet_cols
        par(mfg = c(mfgi, mfgj))
      }
      
      ## Set the plot window
      ## Problem: Passing extra args through ... (e.g., legend_args) to plot.window
      ## triggers an annoying warning about unrecognized graphical params.
      # plot.window(
      #   xlim = xlim, ylim = ylim, 
      #   asp = asp, log = log,
      #   # ...
      # )
      ## Solution: Only pass on relevant args using name checking and do.call.
      ## Idea borrowed from here: https://stackoverflow.com/a/4128401/4115816
      pdots = dots[names(dots) %in% names(formals(plot.default))]
      do.call(
        "plot.window",
        c(list(xlim = xlim, ylim = ylim, asp = asp, log = log), pdots)
      )
      
      # axes, plot.frame and grid
      if (isTRUE(axes)) {
        if (isTRUE(frame.plot)) {
          # if plot frame is true then print axes per normal...
          if (type %in% c("pointrange", "errorbar", "ribbon") && !is.null(xlabs)) {
            Axis(x, side = 1, at = xlabs, labels = names(xlabs))
          } else {
            Axis(x, side = 1)
          }
          Axis(y, side = 2)
        } else {
          # ... else only print the "outside" axes.
          if (ii %in% oxaxis) {
            if (type %in% c("pointrange", "errorbar", "ribbon") && !is.null(xlabs)) {
              Axis(x, side = 1, at = xlabs, labels = names(xlabs))
            } else {
              Axis(x, side = 1)
            }
          }
          if (ii %in% oyaxis) {
            Axis(y, side = 2)
          }
        }
      }
      
      # facet titles
      ## Note: facet titles could be done more simply with mtext... but then we
      ## couldn't adjust background features (e.g., fill), or rotate the rhs
      ## facet grid text. So we're rolling our own "manual" versions with text
      ## and rect.
      if (!is.null(facet)) {
        # Get the four corners of plot area (x1, x2, y1, y2)
        corners = par("usr")
        # special logic for facet grids
        if (is.null(facet_newlines) || facet_newlines==0) {
          facet_title_lines = 1 
        } else {
          facet_title_lines = 1 + facet_newlines
        }
        # different logic for facet grids versus regular facets
        if (isTRUE(attr(facet, "facet_grid"))) {
          ## top facet strips
          if (ii %in% 1:nfacet_cols) {
            if (isTRUE(facet_rect)) {
              line_height = grconvertY(facet_title_lines + .1, from="lines", to="user") - grconvertY(0, from="lines", to="user")
              line_height = line_height * facet_text / cex_fct_adj
              rect(
                corners[1], corners[4], corners[2], corners[4] + line_height,
                col = facet_bg, border = facet_border,
                xpd = NA
              )
            }
            text(
              x = mean(corners[1:2]),
              y = corners[4] + grconvertY(0.4, from="lines", to="user") - grconvertY(0, from="lines", to="user"),
              labels = sub("^(.*?)~.*", "\\1", facets[[ii]]),
              adj = c(0.5, 0),
              cex = facet_text/cex_fct_adj,
              col = facet_col,
              font = facet_font,
              xpd = NA, 
            )
          }
          ## right facet strips
          if (ii %% nfacet_cols == 0 || ii == nfacets) {
            if (isTRUE(facet_rect)) {
              line_height = grconvertX(facet_title_lines + .1, from="lines", to="user") - grconvertX(0, from="lines", to="user")
              line_height = line_height * facet_text / cex_fct_adj
              rect(
                corners[2], corners[3], corners[2] + line_height, corners[4],
                col = facet_bg, border = facet_border,
                xpd = NA
              )
            }
            text(
              x = corners[2] + grconvertX(0.4, from="lines", to="user") - grconvertX(0, from="lines", to="user"),
              y = mean(corners[3:4]),
              labels = sub("^.*?~(.*)", "\\1", facets[[ii]]),
              srt = 270,
              adj = c(0.5, 0),
              cex = facet_text/cex_fct_adj,
              col = facet_col,
              font = facet_font,
              xpd = NA
            )
          }
        } else {
          if (isTRUE(facet_rect)) {
            line_height = grconvertY(facet_title_lines + .1, from="lines", to="user") - grconvertY(0, from="lines", to="user")
            line_height = line_height * facet_text / cex_fct_adj
            rect(
              corners[1], corners[4], corners[2], corners[4] + line_height,
              col = facet_bg, border = facet_border,
              xpd = NA
            )
          }
          text(
            x = mean(corners[1:2]),
            y = corners[4] + grconvertY(0.4, from="lines", to="user") - grconvertY(0, from="lines", to="user"),
            labels = paste(facets[[ii]]),
            adj = c(0.5, 0),
            cex = facet_text/cex_fct_adj,
            col = facet_col,
            font = facet_font,
            xpd = NA
          )
        }
      }
      
      # plot frame
      if (frame.plot) box()
      
      # panel grid lines
      if (is.null(grid)) grid = .tpar[["grid"]]
      if (!is.null(grid)) {
        if (is.logical(grid)) {
          ## If grid is TRUE create a default grid. Rather than just calling the default grid()
          ## abline(... = pretty(extendrange(...)), ...) is used. Reason: pretty() is generic
          ## and works better for axes based on date/time classes. Exception: For axes in logs,
          ## resort to using grid() which is likely better handled there.
          if (isTRUE(grid)) {
            gnx = gny = NULL
            if (!par("xlog")) {
              abline(v = pretty(extendrange(x)), col = "lightgray", lty = "dotted", lwd = par("lwd"))
              gnx = NA
            }
            if (!par("ylog")) {
              abline(h = pretty(extendrange(c(y, ymin, ymax))), col = "lightgray", lty = "dotted", lwd = par("lwd"))
              gny = NA
            }
            grid(nx = gnx, ny = gny)
          }
        } else {
          grid
        }
      }
      
    } # end of ii facet loop
    
  } # end of add check

  
  #
  ## Interior plot elements
  #
  
  # Finally, we can draw all of the plot elements (points, lines, etc.)
  # We'll do this via a nested loops:
  #  1) Outer loop over groups 
  #  2) Inner loop over facets

  ## Outer loop over the "by" groups
  for (i in seq_along(split_data)) {
    
    # Split group-level data again to grab any facets
    idata = split_data[[i]]
    ifacet = idata[["facet"]]
    if (!is.null(ifacet)) {
      idata[["facet"]] = NULL ## Don't need this anymore since we'll be splitting by ifacet
      ## Need extra catch for non-groupby data that also doesn't have ymin or
      ## ymax vars
      if (is.null(by) || isTRUE(by_continuous)) {
        if (is.null(idata[["ymin"]])) idata[["ymin"]] = NULL
        if (is.null(idata[["ymax"]])) idata[["ymax"]] = NULL
      }
      if (isTRUE(by_continuous)) {
        idata[["col"]] = col[round(rescale_num(by, to = c(1,100)))]
        idata[["bg"]] = bg[round(rescale_num(by, to = c(1,100)))]
      }
      idata = lapply(idata, split, ifacet)
      idata = do.call(function(...) Map("list", ...), idata)
    } else {
      idata = list(idata)
      if (isTRUE(by_continuous)) {
        if (length(col)!=1) {
          idata[[1]][["col"]] = col[round(rescale_num(by, to = c(1,100)))]
        } else {
          idata[[1]][["col"]] = col
        }
        if (length(bg)!=1) {
          idata[[1]][["bg"]] = bg[round(rescale_num(by, to = c(1,100)))]
        } else {
          idata[[1]][["bg"]] = bg
        }
      }
    }
    
    icol = col[i]
    ibg = bg[i]
    ipch = pch[i]
    ilty = lty[i]
    ilwd = lwd[i]
    
    ## Inner loop over the "facet" variables
    for (ii in seq_along(idata)) {
      xx = idata[[ii]]$x
      yy = idata[[ii]]$y
      yymin = idata[[ii]]$ymin
      yymax = idata[[ii]]$ymax
      
      if (isTRUE(by_continuous)) {
        icol = idata[[ii]]$col
        ibg = idata[[ii]]$bg
      }
      
      # Set the facet "window" manually
      # See: https://github.com/grantmcdermott/tinyplot/issues/65
      # if (nfacets > 1) par(mfg = c(1, ii))
      if (nfacets > 1) {
        mfgi = ceiling(ii/nfacet_cols)
        mfgj = ii %% nfacet_cols
        if (mfgj==0) mfgj = nfacet_cols
        par(mfg = c(mfgi, mfgj)) 
      }
      
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
          col = icol,
          # lty = ilty,
          lwd = ilwd
        )
      }
      if (type == "errorbar") {
        arrows(
          x0 = xx,
          y0 = yymin,
          x1 = xx,
          y1 = yymax,
          col = icol,
          # lty = ilty,
          lwd = ilwd,
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
          col = icol,
          bg = ibg,
          ## rather hardcode "p" to avoid warning message about "pointrange"
          type = "p",
          pch = ipch,
          # lty = ilty,
          lwd = ilwd,
          cex = cex
        )
      } else if (type %in% c("l", "o", "b", "c", "h", "s", "S", "ribbon")) {
        rtype = type == "ribbon"
        if (rtype) type = "l"
        lines(
          x = xx,
          y = yy,
          col = icol,
          type = type,
          pch = ipch,
          lty = ilty,
          lwd = ilwd
        )
        if (rtype) type = "ribbon"
      } else if (type == "polygon") {
        polygon(
          x = xx,
          y = yy,
          border = icol,
          col = ibg,
          lty = ilty,
          lwd = ilwd
        )
      } else {
        stop("`type` argument not supported.", call. = FALSE)
      }
      
    }
    
  }
  
  # save end pars for possible recall later
  apar = par(no.readonly = TRUE)
  set_saved_par(when = "after", apar)

  
}




#' @rdname tinyplot
#' @importFrom stats as.formula model.frame terms
#' @export
tinyplot.formula = function(
    x = NULL,
    data = parent.frame(),
    facet = NULL,
    facet.args = NULL,
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
    lwd = NULL,
    restore.par = FALSE,
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
  legend_args = list(x = NULL)

  ## set up model frame
  m = match.call(expand.dots = FALSE)
  m = m[c(1L, match(c("formula", "data", "subset", "na.action", "drop.unused.levels"), names(m), 0L))]
  m$formula = formula
  ## need stats:: for non-standard evaluation
  m[[1L]] = quote(stats::model.frame)
  # catch for facets (need to ensure that na.omit, na.action, etc. are at done
  # at the same level to avoid mismatches if there any missing variables)
  has_facet_fml = !is.null(facet) && inherits(facet, "formula")
  if (has_facet_fml) {
    facet_fml = facet
    facet_fml[[1]] = as.name("+")
    if (no_y) {
      m$formula = eval(substitute(
        update(formula,  ~  . + facet_fml), list(facet_fml = facet_fml)
      ))
    } else {
      m$formula = eval(substitute(
        update(formula, .  ~ . + facet_fml), list(facet_fml = facet_fml)
      ))
    }
  }
  mf = eval.parent(m)
  
  ## We need to do some extra work if we included facet variables in the model
  ## frame above
  if (has_facet_fml) {
    # separate the facet columns from the rest of the model frame
    facet_n_cols = length(all.vars(facet_fml))
    fmf = mf[, tail(seq_along(mf), facet_n_cols), drop = FALSE]
    mf = mf[, head(seq_along(mf), -facet_n_cols), drop = FALSE]
    # now do some prep work on the facets themselves for nicer plotting (e.g,
    # grid arrangement for two-sided facet formula)
    no_yfacet = length(facet) == 2L
    facet_fml_rhs = if (no_yfacet) 2L else 3L
    if (no_yfacet) {
      yfacet_loc = NULL
      xfacet_loc = 1L
    } else {
      yfacet_loc = 1L
      xfacet_loc = 2L
    }
    if (NCOL(fmf) < xfacet_loc) stop("formula should specify at least one variable on the right-hand side")
    yfacet = if (no_yfacet) NULL else fmf[, yfacet_loc]
    xfacet = fmf[, xfacet_loc:NCOL(fmf)]
    
    ## return object
    xfacet = interaction(xfacet, sep = ":")
    if (no_yfacet) {
      facet = xfacet
    } else {
      # yfacet = interaction(yfacet, sep = ":")
      ## NOTE: We "swap" the formula LHS and RHS since mfrow plots rowwise
      facet = interaction(xfacet, yfacet, sep = "~")
      attr(facet, "facet_grid") = TRUE
      attr(facet, "facet_nrow") = length(unique(yfacet))
    }
    
  }
  
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
    # special catch if by is the same as x or y (normally for continuous legend)
    by_same_y = by_same_x = FALSE
    fml_all_vars = all.vars(m$formula, unique = FALSE)
    if (any(duplicated(fml_all_vars))) {
      if (isTRUE(no_y)) {
        by_same_x = TRUE ## i.e., if there is duplication and no y var, assume by must be the same as x
      } else {
        fml_lhs_vars = paste(attr(terms(m$formula), "variables")[[2]])
        fml_rhs_vars = fml_all_vars[!(fml_all_vars %in% fml_lhs_vars)]
        if (any(duplicated(fml_rhs_vars))) {
          by_same_x = TRUE
        } else {
          by_same_y = TRUE
        }
      }
    }
    if (isTRUE(by_same_y)) {
      by = y
      bylab = names(mf)[y_loc]
      legend_args[["title"]] = bylab
    } else if (isTRUE(by_same_x)) {
      by = x
      bylab = names(mf)[x_loc]
      legend_args[["title"]] = bylab
    } else {
      by = bylab = NULL
    }
  } else if (NCOL(mf) == by_loc) {
    by = mf[, by_loc]
    bylab = names(mf)[by_loc]
    legend_args[["title"]] = bylab
    # if (!inherits(by, "factor")) by = as.factor(by)
  } else if (NCOL(mf) > by_loc) {
    by = do.call("interaction", mf[, -c(y_loc, x_loc)])
    bylab = sprintf("interaction(%s)", paste(names(mf)[-c(y_loc, x_loc)], collapse = ", "))
    legend_args[["title"]] = bylab
  }

  ## nice axis and legend labels
  if (no_y) {
    if (is.null(ylab)) ylab = names(mf)[x_loc]
    if (is.null(xlab)) xlab = "Index"
  } else {
    if (is.null(ylab)) ylab = names(mf)[y_loc]
    if (is.null(xlab)) xlab = names(mf)[x_loc]
  }
  
  tinyplot.default(
    x = x, y = y, by = by,
    facet = facet, facet.args = facet.args,
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
    legend_args = legend_args,
    pch = pch,
    col = col,
    lty = lty,
    lwd = lwd,
    restore.par = restore.par,
    ...
    )

}

#' @importFrom methods as
#' @importFrom stats update
#' @rdname tinyplot
#' @export
tinyplot.density = function(
    x = NULL,
    by = NULL,
    facet = NULL,
    facet.args = NULL,
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
    lwd = NULL,
    bg = NULL,
    fill = NULL,
    restore.par = FALSE,
    ...
  ) {

  type = match.arg(type)
  ## override if bg = "by"
  if (!is.null(bg) || !is.null(fill)) type = "area"
  
  # catch for facet_grid
  if (!is.null(facet)) {
    facet_attributes = attributes(facet)
    # facet_grid = attr(facet, "facet_grid")
  }

  if (inherits(x, "density")) {
    object = x
    legend_args = list(x = NULL)
    # Grab by label to pass on legend title to tinyplot.default
    legend_args[["title"]] = deparse(substitute(by))
  } else {
    ## An internal catch for non-density objects that were forcibly
    ## passed to tinyplot.density (e.g., via a one-side formula)
    if (anyNA(x)) {
      x = na.omit(x)
      if (!is.null(by)) by = by[-attr(x, "na.action")]
      if (!is.null(facet)) facet = facet[-attr(x, "na.action")]
      x = as.numeric(x)
    }
    object = stats::density(x)
    legend_args = list(...)[["legend_args"]]
  }

  by_names = facet_names = NULL
  if (is.null(by) && is.null(facet)) {
    x = object$x
    y = object$y
  } else {
    x = eval(str2lang(object$data.name), envir = parent.frame())
    if (anyNA(x)) {
      x = na.omit(x)
      if (!is.null(by) && length(by) != length(x)) by = by[-attr(x, "na.action")]
      if (!is.null(facet) && length(facet) != length(x)) facet = facet[-attr(x, "na.action")]
      x = as.numeric(x)
    }
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
    by_names = tryCatch(as(by_names, class(by)), error = function(e) if (inherits(by, "factor")) as.factor(by_names) else by_names)
    # need to coerce facet variables to factors for faceting to work properly later on
    # if we originally passed a factor, try to preserve this order for grid arrangement
    if (inherits(facet, "factor")) {
      orig_len = length(levels(facet))
      new_len = length(facet_names)
      if (orig_len == new_len) {
        facet_names = levels(facet)
      } else {
        ## need to recycle names if nested in multiple by splits
        facet_names = rep(levels(facet), each = new_len/orig_len)
      }
    } else {
      facet_names = tryCatch(as(facet_names, class(facet)), error = function(e) facet_names)
      facet_names = tryCatch(as.factor(facet_names), error = function(e) facet_names)
    }
    
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
      if (length(by_names) > 3 || length(facet_names) > 3) {
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
    legend_args[["x.intersp"]] = 1.25
    legend_args[["lty"]] = 0
    legend_args[["pt.lwd"]] = 1
  }

  ## axes range
  if (is.null(xlim)) xlim = range(x)
  if (is.null(ylim)) ylim = range(y)

  ## nice labels and titles
  if (is.null(ylab)) ylab = "Density"
  if (is.null(xlab)) xlab = paste0("N = ", object$n, "   Bandwidth = ", sprintf("%.4g", object$bw))
  if (is.null(main)) main = paste0(paste(object$call, collapse = "(x = "), ")")

  # if (!is.null(facet)) attr(facet, "facet_grid") = facet_grid
  if (!is.null(facet)) {
    if (!is.null(facet_attributes[["levels"]])) {
      facet = factor(facet, levels = facet_attributes[["levels"]])
    } else {
      facet = factor(facet)
    }
    attr(facet, "facet_grid") = facet_attributes[["facet_grid"]]
  }
  
  tinyplot.default(
    x = x, y = y, by = by, facet = facet, facet.args = facet.args,
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
    legend_args = legend_args,
    pch = pch,
    col = col,
    bg = bg,
    fill = fill,
    lty = lty,
    lwd = lwd,
    restore.par = restore.par,
    ...
    )

}

#' @export
#' @name plt
#' @rdname tinyplot
plt = tinyplot


# utility function for converting facet formulas into variables

get_facet_fml = function(formula, data = NULL) {
  
  xfacet = yfacet = NULL
  
  ## catch one-sided formula ~ x or ~ x | z with no "y" variable
  if (!inherits(formula, "formula")) formula = as.formula(formula)
  no_yfacet = length(formula) == 2L
  fml_rhs = if (no_yfacet) 2L else 3L
  
  ## set up model frame
  m = match.call(expand.dots = FALSE)
  
  if (!is.null(data)) {
    m = m[c(1L, match(c("formula", "data", "subset", "na.action", "drop.unused.levels"), names(m), 0L))]
  }
  
  m$formula = formula
  ## need stats:: for non-standard evaluation
  m[[1L]] = quote(stats::model.frame)
  mf = eval.parent(m)
  
  ## extract variables: x, y (if any)
  if (no_yfacet) {
    yfacet_loc = NULL
    xfacet_loc = 1L
  } else {
    yfacet_loc = 1L
    xfacet_loc = 2L
  }
  if (NCOL(mf) < xfacet_loc) stop("formula should specify at least one variable on the right-hand side")
  yfacet = if (no_yfacet) NULL else mf[, yfacet_loc]
  xfacet = mf[, xfacet_loc:NCOL(mf)]
  
  ## return object
  xfacet = interaction(xfacet, sep = ":")
  if (no_yfacet) {
    ret = xfacet
  } else {
    # yfacet = interaction(yfacet, sep = ":")
    ## NOTE: We "swap" the formula LHS and RHS since mfrow plots rowwise
    ret = interaction(xfacet, yfacet, sep = "~")
    attr(ret, "facet_grid") = TRUE
    attr(ret, "facet_nrow") = length(unique(yfacet))
  }
  
  return(ret)
}
