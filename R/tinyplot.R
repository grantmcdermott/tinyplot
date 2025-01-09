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
#'   passes a numeric or integer to `by`. To group by multiple variables, wrap
#'   them with \code{\link[base]{interaction}}.
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
#'   - `free` a logical value indicating whether the axis limits (scales) for
#'   each individual facet should adjust independently to match the range of
#'   the data within that facet. Default is `FALSE`. Separate free scaling of
#'   the x- or y-axis (i.e., whilst holding the other axis fixed) is not
#'   currently supported.
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
#' @param formula a \code{\link[stats]{formula}} that optionally includes
#'   grouping variable(s) after a vertical bar, e.g. `y ~ x | z`. One-sided
#'   formulae are also permitted, e.g. `~ y | z`. Multiple grouping variables
#'   can be specified in different ways, e.g. `y ~ x | z1:z2` or
#'   `y ~ x | z1 + z2`. (These two representations are treated as equivalent;
#'   both are parsed as `interaction(z1, z2)` internally.) Note that the
#'   `formula` and `x` arguments should not be specified in the same call.
#' @param data a data.frame (or list) from which the variables in formula
#'   should be taken. A matrix is converted to a data frame.
#' @param type character string or call to a `type_*()` function giving the
#'   type of plot desired.
#'   - NULL (default): Choose a sensible type for the type of `x` and `y` inputs
#'     (i.e., usually `"p"`).
#'   - 1-character values supported by \code{\link[graphics]{plot}}:
#'     - `"p"` Points
#'     - `"l"` Lines
#'     - `"b"` Both points and lines
#'     - `"c"` Empty points joined by lines
#'     - `"o"` Overplotted points and lines
#'     - `"s"` Stair steps
#'     - `"S"` Stair steps
#'     - `"h"` Histogram-like vertical lines
#'     - `"n"` Empty plot over the extent of the data
#'   - `tinyplot`-specific types. These fall into several categories:
#'     - Shapes:
#'       - `"area"` / [`type_area()`]: Plots the area under the curve from `y` = 0 to `y` = f(`x`).
#'       - `"errorbar"` / [`type_errorbar()`]: Adds error bars to points; requires `ymin` and `ymax`.
#'       - `"pointrange"` / [`type_pointrange()`]: Combines points with error bars.
#'       - `"polygon"` / [`type_polygon()`]: Draws polygons.
#'       - `"polypath"` / [`type_polypath()`]: Draws a path whose vertices are given in `x` and `y`.
#'       - `"rect"` / [`type_rect()`]: Draws rectangles; requires `xmin`, `xmax`, `ymin`, and `ymax`.
#'       - `"ribbon"` / [`type_ribbon()`]: Creates a filled area between `ymin` and `ymax`.
#'       - `"segments"` / [`type_segments()`]: Draws line segments between pairs of points.
#'       - `"text"` / [`type_text()`]: Add text annotations.
#'     - Visualizations:
#'       - `"boxplot"` / [`type_boxplot()`]: Creates a box-and-whisker plot.
#'       - `"density"` / [`type_density()`]: Plots the density estimate of a variable.
#'       - `"histogram"` / [`type_histogram()`]: Creates a histogram of a single variable.
#'       - `"jitter"` / [`type_jitter()`]: Jittered points.
#'       - `"qq"` / [`type_qq()`]: Creates a quantile-quantile plot.
#'       - `"ridge"` / [`type_ridge()`]: Creates a ridgeline (aka joy) plot.
#'       - `"rug"` / [`type_rug()`]: Adds a rug to an existing plot.
#'       - `"spineplot"` / [`type_spineplot()`]: Creates a spineplot or spinogram.
#'     - Models:
#'       - `"loess"` / [`type_loess()`]: Local regression curve.
#'       - `"lm"` / [`type_lm()`]: Linear regression line.
#'       - `"glm"` / [`type_glm()`]: Generalized linear model fit.
#'       - `"spline"` / [`type_spline()`]: Cubic (or Hermite) spline interpolation.
#'     - Functions:
#'       - [`type_abline()`]: line(s) with intercept and slope.
#'       - [`type_hline()`]: horizontal line(s).
#'       - [`type_vline()`]: vertical line(s).
#'       - [`type_function()`]: arbitrary function.
#'       - [`type_summary()`]: summarize `y` by unique values of `x`.
#' @param xmin,xmax,ymin,ymax minimum and maximum coordinates of relevant area
#'   or interval plot types. Only used when the `type` argument is one of
#'   `"rect"` or `"segments"` (where all four min-max coordinates are required),
#'   or `"pointrange"`, `"errorbar"`, or `"ribbon"` (where only `ymin` and
#'   `ymax` required alongside `x`).
#' @param xlim the x limits (x1, x2) of the plot. Note that x1 > x2 is allowed
#'   and leads to a ‘reversed axis’. The default value, NULL, indicates that
#'   the range of the `finite` values to be plotted should be used.
#' @param ylim the y limits of the plot.
#' @param log a character string which contains "x" if the x axis is to be
#'   logarithmic, "y" if the y axis is to be logarithmic and "xy" or "yx" if
#'   both axes are to be logarithmic.
#' @param empty logical indicating whether the interior plot region should be
#'  left empty. The default is `FALSE`. Setting to `TRUE` has a similar effect
#'  to invoking `type = "n"` above, except that any legend artifacts owing to a
#'  particular plot type (e.g., lines for `type = "l"` or squares for
#'  `type = "area"`) will still be drawn correctly alongside the empty plot. In
#'  contrast,`type = "n"` implicitly assumes a scatterplot and so any legend
#'  will only depict points.
#' @param main a main title for the plot, see also `title`.
#' @param sub a subtitle for the plot.
#' @param xlab a label for the x axis, defaults to a description of x.
#' @param ylab a label for the y axis, defaults to a description of y.
#' @param ann a logical value indicating whether the default annotation (title
#'   and x and y axis labels) should appear on the plot.
#' @param axes logical or character. Should axes be drawn (`TRUE` or `FALSE`)?
#'   Or alternatively what type of axes should be drawn: `"standard"` (with
#'   axis, ticks, and labels; equivalent to `TRUE`), `"none"` (no axes;
#'   equivalent to `FALSE`), `"ticks"` (only ticks and labels without axis line),
#'   `"labels"` (only labels without ticks and axis line), `"axis"` (only axis
#'   line and labels but no ticks). To control this separately for the two
#'   axes, use the character specifications for `xaxt` and/or `yaxt`.
#' @param frame.plot a logical indicating whether a box should be drawn around
#'   the plot. Can also use `frame` as an acceptable argument alias.
#'   The default is to draw a frame if both axis types (set via `axes`, `xaxt`,
#'   or `yaxt`) include axis lines.
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
# `   par("lwd")`) and pad on either side of that.
#' @param bg background fill color for the open plot symbols 21:25 (see
#'   `points.default`), as well as ribbon and area plot types.
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
#' @param add logical. If TRUE, then elements are added to the current plot rather
#'   than drawing a new plot window. Note that the automatic legend for the
#'   added elements will be turned off. See also [tinyplot_add], which provides
#'   a convenient wrapper around this functionality for layering on top of an
#'   existing plot without having to repeat arguments.
#' @param draw a function that draws directly on the plot canvas (before `x` and
#'   `y` are plotted). The `draw` argument is primarily useful for adding common
#'   elements to each facet of a faceted plot, e.g.
#'   \code{\link[graphics]{abline}} or \code{\link[graphics]{text}}. Note that
#'   this argument is somewhat experimental and that _no_ internal checking is
#'   done for correctness; the provided argument is simply captured and
#'   evaluated as-is. See Examples.
#' @param flip logical. Should the plot orientation be flipped, so that the
#'   y-axis is on the horizontal plane and the x-axis is on the vertical plane?
#'   Default is FALSE.
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
#' @param xaxt,yaxt character specifying the type of x-axis and y-axis, respectively.
#'   See `axes` for the possible values.
#' @param xaxs,yaxs character specifying the style of the interval calculation used
#'   for the x-axis and y-axis, respectively. See \code{\link[graphics]{par}}
#'   for the possible values.
#' @param ... other graphical parameters. If `type` is a character specification
#'   (such as `"hist"`) then any argument names that match those from the corresponding
#'   `type_*()` function (such as \code{\link{type_hist}}) are passed on to that.
#'   All remaining arguments from `...` can be further graphical parameters, see
#'   \code{\link[graphics]{par}}).
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
#' @importFrom grDevices axisTicks adjustcolor colorRampPalette extendrange palette palette.colors palette.pals hcl.colors hcl.pals xy.coords png jpeg pdf svg dev.off dev.new dev.list
#' @importFrom graphics abline arrows axis Axis box boxplot grconvertX grconvertY hist lines mtext par plot.default plot.new plot.window points polygon polypath segments rect text title
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
#' tinyplot(Temp ~ Day | Month, data = aq) ## formula method
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
#'   pch = 21, # use filled circles
#'   cex = 2,
#'   bg = 0.3, # numeric in [0,1] adds a grouped background fill with transparency
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
#'   facet = ~Month,
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
#'   facet = ~Month,
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
#'   facet = ~Month, facet.args = list(nrow = 1),
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
#' aq$hot = ifelse(aq$Temp >= 75, "hot", "cold")
#' aq$windy = ifelse(aq$Wind >= 15, "windy", "calm")
#' tinyplot(
#'   Temp ~ Day,
#'   facet = windy ~ hot,
#'   data = aq
#' )
#'
#' # To add common elements to each facet, use the `draw` argument
#'
#' tinyplot(
#'   Temp ~ Day,
#'   facet = windy ~ hot,
#'   data = aq,
#'   draw = abline(h = 75, lty = 2, col = "hotpink")
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
#' # It's possible to customize the look of your plots by setting graphical
#' # parameters (e.g., via `(t)par`)... But a more convenient way is to just use
#' # built-in themes (see `?tinytheme`).
#'
#' tinytheme("clean2")
#' tinyplot(
#'   Temp ~ Day | Month,
#'   data = aq,
#'   type = "b",
#'   alpha = 0.5,
#'   main = "Daily temperatures by month",
#'   sub = "Brought to you by tinyplot"
#' )
#' # reset the theme
#' tinytheme()
#'
#' # For more examples and a detailed walkthrough, please see the introductory
#' # tinyplot tutorial available online:
#' # https://grantmcdermott.com/tinyplot/vignettes/introduction.html
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
    x = NULL,
    y = NULL,
    by = NULL,
    facet = NULL,
    facet.args = NULL,
    data = NULL,
    type = NULL,
    xlim = NULL,
    ylim = NULL,
    log = "",
    main = NULL,
    sub = NULL,
    xlab = NULL,
    ylab = NULL,
    ann = par("ann"),
    axes = TRUE,
    frame.plot = NULL,
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
    xmin = NULL,
    xmax = NULL,
    ymin = NULL,
    ymax = NULL,
    add = FALSE,
    draw = NULL,
    file = NULL,
    width = NULL,
    height = NULL,
    empty = FALSE,
    xaxt = NULL,
    yaxt = NULL,
    flip = FALSE,
    xaxs = NULL,
    yaxs = NULL,
    ...) {
  par_first = get_saved_par("first")
  if (is.null(par_first)) set_saved_par("first", par())

  # save for tinyplot_add()
  if (!isTRUE(add)) {
    calls = sys.calls()
    idx = grep("^tinyplot", sapply(calls, function(k) k[[1]]))
    if (length(idx) > 0) {
      options(tinyplot_last_call = calls[[idx[1]]])
    }
  }

  ## TODO: remove the global option above and move to this when density is refactored
  # cal = match.call(call = sys.call(sys.parent()), expand.dots = TRUE)
  # assign(".last_call", cal, envir = get(".tinyplot_env", envir = parent.env(environment())))

  dots = list(...)

  if (isTRUE(add)) legend = FALSE
  draw = substitute(draw)


  # sanitize arguments

  # type factories vs. strings
  type = sanitize_type(type, x, y, dots)
  if ("dots" %in% names(type)) dots = type$dots
  
  # retrieve type-specific data and drawing functions
  type_data = type$data
  type_draw = type$draw
  type = type$name
  
  # area flag (mostly for legend)
  was_area_type = identical(type, "area")
  # check flip flag is logical 
  assert_flag(flip)

  palette = substitute(palette)

  # themes
  if (is.null(palette)) palette = get_tpar("palette", default = NULL)
  if (is.null(pch)) pch = get_tpar("pch", default = NULL)

  xlabs = ylabs = NULL

  # type_ridge()
  ygroup = NULL

  # will be overwritten by some type_data() functions and ignored by others
  ribbon.alpha = sanitize_ribbon.alpha(NULL)

  ## handle defaults of axes, xaxt, yaxt, frame.plot
  ## - convert axes to character if necessary
  ## - set defaults of xaxt/yaxt (if these are NULL) based on axes
  ## - set logical axes based on xaxt/yaxt
  ## - set frame.plot default based on xaxt/yaxt
  if (isFALSE(axes)) {
    axes = xaxt = yaxt = "none"
  } else if (isTRUE(axes)) {
    axes = "standard"
    if (is.null(xaxt)) xaxt = get_tpar("xaxt", default = "standard")
    if (is.null(yaxt)) yaxt = get_tpar("yaxt", default = "standard")
  } else {
    xaxt = yaxt = axes
  }
  axis_types = c("standard", "none", "labels", "ticks", "axis")
  axes = match.arg(axes, axis_types)
  xaxt = match.arg(xaxt, axis_types)
  yaxt = match.arg(yaxt, axis_types)
  xaxt = substr(match.arg(xaxt, axis_types), 1L, 1L)
  yaxt = substr(match.arg(yaxt, axis_types), 1L, 1L)
  axes = any(c(xaxt, yaxt) != "n")
  if (is.null(frame.plot) || !is.logical(frame.plot)) frame.plot = all(c(xaxt, yaxt) %in% c("s", "a"))

  # Write plot to output file or window with fixed dimensions
  setup_device(file = file, width = width, height = height)
  if (!is.null(file)) on.exit(dev.off(), add = TRUE)

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
  if (!is.null(facet) && isTRUE(add)) {
    par(get_saved_par(when = "after"))
  }

  # Capture deparsed expressions early, before x, y and by are evaluated
  x_dep = if (!is.null(x)) {
    deparse1(substitute(x))
  } else if (type %in% c("rect", "segments")) {
    x = NULL
    NULL
  }
  y_dep = if (is.null(y)) {
    deparse1(substitute(x))
  } else {
    deparse1(substitute(y))
  }
  by_dep = deparse1(substitute(by))

  ## coerce character variables to factors
  if (!is.null(x) && is.character(x)) x = factor(x)
  if (!is.null(y) && is.character(y)) y = factor(y)
  if (!is.null(by) && is.character(by)) by = factor(by)

  # flag if x==by (currently only used for "boxplot", "spineplot" and "ridges" types)
  x_by = identical(x, by)

  facet_dep = deparse1(substitute(facet))
  # flag if facet==by
  facet_by = FALSE
  if (!is.null(facet) && length(facet) == 1 && facet == "by") {
    by = as.factor(by) ## if by==facet, then both need to be factors
    facet = by
    facet_by = TRUE
  } else if (!is.null(facet) && inherits(facet, "formula")) {
    facet = get_facet_fml(facet, data = data)
    if (isTRUE(attr(facet, "facet_grid"))) {
      facet.args[["nrow"]] = attr(facet, "facet_nrow")
    }
  }
  facet_attr = attributes(facet) ## TODO: better solution for restoring facet attributes?

  if (is.null(x)) {
    ## Special catch for rect and segment plots without a specified y-var
    if (type %in% c("rect", "segments")) {
      xmin_dep = deparse(substitute(xmin))
      xmax_dep = deparse(substitute(xmax))
      x_dep = paste0("[", xmin_dep, ", ", xmax_dep, "]")
      x = rep(NA, length(x))
    }
  }
  if (is.null(y)) {
    ## Special catch for area and interval plots without a specified y-var
    if (type %in% c("rect", "segments", "pointrange", "errorbar", "ribbon")) {
      ymin_dep = deparse(substitute(ymin))
      ymax_dep = deparse(substitute(ymax))
      y_dep = paste0("[", ymin_dep, ", ", ymax_dep, "]")
      y = rep(NA, length(x))
    } else if (type == "density") {
      if (is.null(ylab)) ylab = "Density"
    } else if (type %in% c("histogram", "function")) {
      if (is.null(ylab)) ylab = "Frequency"
    } else {
      y = x
      x = seq_along(x)
      if (is.null(xlab)) xlab = "Index"
    }
  }

  if (is.null(xlab)) xlab = x_dep
  if (is.null(ylab)) ylab = y_dep

  # alias
  if (is.null(bg) && !is.null(fill)) bg = fill

  datapoints = list(x = x, y = y, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, ygroup = ygroup)
  datapoints = Filter(function(z) length(z) > 0, datapoints)
  datapoints = data.frame(datapoints)
  if (nrow(datapoints) > 0) {
    datapoints[["rowid"]] = seq_len(nrow(datapoints))
    datapoints[["facet"]] = if (!is.null(facet)) facet else ""
    datapoints[["by"]] = if (!is.null(by)) by else ""
  }

  ## initialize empty list with information that type_data
  ## can overwrite in order to pass on to type_draw
  type_info = list()

  if (!is.null(type_data)) {
    fargs = list(
      datapoints = datapoints,
      bg = bg,
      by = by,
      col = col,
      lty = lty,
      facet = facet,
      facet.args = facet.args,
      palette = palette,
      ribbon.alpha = ribbon.alpha,
      xaxt = xaxt,
      xlabs = xlabs,
      xlim = xlim,
      yaxt = yaxt,
      ylab = ylab,
      ylim = ylim)
    fargs = c(fargs, dots)
    list2env(do.call(type_data, fargs), environment())
  }


  # swap x and y values if flip is TRUE
  assert_flag(flip)
  # extra catch for boxplots
  # now swap the values
  if (isTRUE(flip)) {
    if (type != "boxplot") {
      # limits, labs, etc.
      xlim_cp = xlim
      xlim = ylim
      ylim = xlim_cp
      xlab_cp = xlab
      xlab = ylab
      ylab = xlab_cp
      xlabs_cp = xlabs
      xlabs = ylabs
      ylabs = xlabs_cp
      xaxt_cp = xaxt
      xaxt = yaxt
      yaxt = xaxt_cp
      if (!is.null(log)) {
        log = if (log == "x") "y" else if (log == "y") "x" else log
      }
      # x/y vars
      x_cp = datapoints[["x"]]
      datapoints[["x"]] = datapoints[["y"]]
      datapoints[["y"]] = x_cp
      # x/y min and max vars
      xmin_cp = if (!is.null(datapoints[["xmin"]])) datapoints[["xmin"]] else NULL
      datapoints[["xmin"]] = if (!is.null(datapoints[["ymin"]])) datapoints[["ymin"]] else NULL
      datapoints[["ymin"]] = if (!is.null(xmin_cp)) xmin_cp else NULL
      xmax_cp = if (!is.null(datapoints[["xmax"]])) datapoints[["xmax"]] else NULL
      datapoints[["xmax"]] = if (!is.null(datapoints[["ymax"]])) datapoints[["ymax"]] else NULL
      datapoints[["ymax"]] = if (!is.null(xmax_cp)) xmax_cp else NULL
      # clean up
      rm(xlim_cp, xlab_cp, xlabs_cp, xaxt_cp, x_cp, xmin_cp, xmax_cp)
    } else {
      # We'll let boxplot(..., horizontal = TRUE) handle most of the adjustments
      # and just catch a few elements that we draw beforehand.
      xlab_cp = xlab
      xlab = ylab
      ylab = xlab_cp
      rm(xlab_cp)
    }
  }


  # plot limits
  fargs = lim_args(datapoints = datapoints, xlim = xlim, ylim = ylim, type = type)
  list2env(fargs, environment())


  # split data
  by_ordered = FALSE
  by_continuous = !is.null(by) && inherits(datapoints$by, c("numeric", "integer"))
  if (isTRUE(by_continuous) && type %in% c("l", "b", "o", "ribbon", "polygon", "polypath", "boxplot")) {
    warning("\nContinuous legends not supported for this plot type. Reverting to discrete legend.")
    by_continuous = FALSE
  } else if (!is.null(by)) {
    by_ordered = is.ordered(by)
  }

  if (length(unique(datapoints$facet)) == 1) {
    datapoints[["facet"]] = NULL
  }
  if (!by_continuous) {
    split_data = split(datapoints, datapoints$by)
    split_data = lapply(split_data, as.list)
  } else {
    split_data = list(as.list(datapoints))
  }

  # aesthetics by group: col, bg, etc.
  ngrps = length(split_data)
  pch = by_pch(ngrps = ngrps, type = type, pch = pch)
  lty = by_lty(ngrps = ngrps, type = type, lty = lty)
  lwd = by_lwd(ngrps = ngrps, type = type, lwd = lwd)
  col = by_col(
    ngrps = ngrps, col = col, palette = palette,
    gradient = by_continuous, ordered = by_ordered, alpha = alpha)
  bg = by_bg(
    adjustcolor = adjustcolor, alpha = alpha, bg = bg, by = by, by_continuous = by_continuous,
    by_ordered = by_ordered, col = col, fill = fill, palette = substitute(palette),
    ribbon.alpha = ribbon.alpha, ngrps = ngrps, type = type)

  ncolors = length(col)
  lgnd_labs = rep(NA, times = ncolors)
  if (isTRUE(by_continuous)) {
    ## Identify the pretty break points for our labels
    nlabs = 5
    ncolors = length(col)
    ubyvar = unique(by)
    byvar_range = range(ubyvar)
    pbyvar = pretty(byvar_range, n = nlabs)
    pbyvar = pbyvar[pbyvar >= byvar_range[1] & pbyvar <= byvar_range[2]]
    # optional thinning
    if (length(ubyvar) == 2 && all(ubyvar %in% pbyvar)) {
      pbyvar = ubyvar
    } else if (length(pbyvar) > nlabs) {
      pbyvar = pbyvar[seq_along(pbyvar) %% 2 == 0]
    }
    ## Find the (approximate) location of our pretty labels
    pidx = rescale_num(c(byvar_range, pbyvar), to = c(1, ncolors))[-c(1:2)]
    pidx = round(pidx)
    lgnd_labs[pidx] = pbyvar
  }

  # Determine the number and arrangement of facets.
  # Note: We're do this up front, so we can make some adjustments to legend cex
  #   next (if there are facets). But the actual drawing of the facets will only
  #   come later.
  attributes(facet) = facet_attr ## TODO: better solution for restoring facet attributes?
  fargs = facet_layout(facet = facet, facet.args = facet.args, add = add)
  list2env(fargs, environment())

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
      if (ngrps > 1) {
        lgnd_labs = names(split_data)
      } else {
        lgnd_labs = ylab
      }
    }

    has_sub = !is.null(sub)

    if (isTRUE(was_area_type) || isTRUE(type %in% c("area", "rect", "hist", "histogram"))) {
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
  } else if (legend_args[["x"]] == "none" && isFALSE(add)) {
    omar = par("mar")
    ooma = par("oma")
    topmar_epsilon = 0.1

    # Catch to avoid recursive offsets, e.g. repeated tinyplot calls with
    # "bottom!" legend position.

    ## restore inner margin defaults
    ## (in case the plot region/margins were affected by the preceding tinyplot call)
    if (any(ooma != 0)) {
      if (ooma[1] != 0 && omar[1] == par("mgp")[1] + 1 * par("cex.lab")) omar[1] = 5.1
      if (ooma[2] != 0 && omar[2] == par("mgp")[1] + 1 * par("cex.lab")) omar[2] = 4.1
      if (ooma[3] == topmar_epsilon && omar[3] != 4.1) omar[3] = 4.1
      if (ooma[4] != 0 && omar[4] == 0) omar[4] = 2.1
      par(mar = omar)
    }
    ## restore outer margin defaults (with a catch for custom mfrow plots)
    if (all(par("mfrow") == c(1, 1))) {
      par(omd = c(0, 1, 0, 1))
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

    adj_title = !is.null(legend) && (legend == "top!" || (!is.null(legend_args[["x"]]) && legend_args[["x"]] == "top!") || (is.list(legend_eval) && legend_eval[[1]] == "top!"))

    # For the "top!" legend case, bump main title up to make space for the
    # legend beneath it: Take the normal main title line gap (i.e., 1.7 lines)
    # and add the difference between original top margin and new one (i.e.,
    # which should equal the height of the new legend). Note that we also
    # include a 0.1 epsilon bump, which we're using to reset the tinyplot
    # window in case of recursive "top!" calls. (See draw_legend code.)

    if (isTRUE(adj_title)) {
      line_main = par("mar")[3] - opar[["mar"]][3] + 1.7 + 0.1
    } else {
      line_main = NULL
    }

    if (!is.null(sub)) {
      if (isTRUE(get_tpar("side.sub", 1) == 3)) {
        if (is.null(line_main)) line_main = par("mgp")[3] + 1.7 - .1
        line_main = line_main + 1.2
      }
      if (isTRUE(get_tpar("side.sub", 1) == 3)) {
        line_sub = get_tpar("line.sub", 1.7)
      } else {
        line_sub = get_tpar("line.sub", 4)
      }
      args = list(
        text = sub,
        line = line_sub,
        cex = get_tpar("cex.sub", 1.2),
        col = get_tpar("col.sub", "black"),
        adj = get_tpar(c("adj.sub", "adj")),
        font = get_tpar("font.sub", 1),
        side = get_tpar("side.sub", 1),
        las = 1
      )
      args = Filter(function(x) !is.null(x), args)
      do.call(mtext, args)
    }

    if (!is.null(main)) {
      args = list(
        main = main,
        line = line_main,
        cex.main = get_tpar("cex.main", 1.4),
        col.main = get_tpar("col.main", "black"),
        font.main = get_tpar("font.main", 2),
        adj = get_tpar(c("adj.main", "adj"), 3))
      args = Filter(function(x) !is.null(x), args)
      do.call(title, args)
    }


    # Axis titles
    args = list(xlab = xlab)
    args[["adj"]] = get_tpar(c("adj.xlab", "adj"))
    do.call(title, args)
    args = list(ylab = ylab)
    args[["adj"]] = get_tpar(c("adj.ylab", "adj"))
    do.call(title, args)
  }

  #
  ## Facet windows
  #

  omar = NULL # Placeholder variable for now, which we re-assign as part of facet margins

  # placeholders for facet_window_args() call
  facet_newlines = facet_text = facet_rect = facet_font = facet_col = facet_bg = facet_border = NULL

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
    facet_newlines = ifelse(length(facet_newlines) == 0, 0, max(facet_newlines))
    omar[3] = omar[3] + facet_newlines * facet_text / cex_fct_adj
    # apply the changes
    par(mar = omar)
  }

  # Now draw the individual facet windows (incl. axes, grid lines, and facet titles)
  # Skip if adding to an existing plot

  facet_window_args = draw_facet_window(
    add = add, asp = asp, axes = axes, cex_fct_adj = cex_fct_adj, dots = dots,
    facet = facet, facet.args = facet.args, facet_newlines = facet_newlines,
    facet_rect = facet_rect, facet_text = facet_text, facet_font = facet_font,
    facet_col = facet_col, facet_bg = facet_bg, facet_border = facet_border,
    facets = facets, ifacet = ifacet,
    nfacet_cols = nfacet_cols, nfacet_rows = nfacet_rows, nfacets = nfacets,
    frame.plot = frame.plot, grid = grid,
    has_legend = has_legend, log = log,
    oxaxis = oxaxis, oyaxis = oyaxis, type = type,
    x = datapoints$x,
    y = datapoints$y,
    xmax = datapoints$xmax, xmin = datapoints$xmin,
    ymax = datapoints$ymax, ymin = datapoints$ymin,
    xaxt = xaxt, xlabs = xlabs, xlim = xlim,
    yaxt = yaxt, ylabs = ylabs, ylim = ylim,
    flip = flip,
    draw = draw,
    xaxs = xaxs, yaxs = yaxs
  )
  list2env(facet_window_args, environment())


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
      if (isTRUE(by_continuous)) {
        idata[["col"]] = col[round(rescale_num(by, to = c(1, 100)))]
        idata[["bg"]] = bg[round(rescale_num(by, to = c(1, 100)))]
      }
      idata = lapply(idata, split, ifacet)
      idata = do.call(function(...) Map("list", ...), idata)
    } else {
      idata = list(idata)
      if (isTRUE(by_continuous)) {
        if (length(col) != 1) {
          idata[[1]][["col"]] = col[round(rescale_num(by, to = c(1, 100)))]
        } else {
          idata[[1]][["col"]] = col
        }
        if (length(bg) != 1) {
          idata[[1]][["bg"]] = bg[round(rescale_num(by, to = c(1, 100)))]
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
      ix = idata[[ii]][["x"]]
      iy = idata[[ii]][["y"]]
      iz = idata[[ii]][["z"]]
      ixmin = idata[[ii]]$xmin
      ixmax = idata[[ii]]$xmax
      iymin = idata[[ii]]$ymin
      iymax = idata[[ii]]$ymax
      ilabels = idata[[ii]][["labels"]]

      if (isTRUE(by_continuous)) {
        icol = idata[[ii]]$col
        ibg = idata[[ii]]$bg
      }

      # Set the facet "window" manually
      # See: https://github.com/grantmcdermott/tinyplot/issues/65
      # if (nfacets > 1) par(mfg = c(1, ii))
      if (nfacets > 1) {
        mfgi = ceiling(ii / nfacet_cols)
        mfgj = ii %% nfacet_cols
        if (mfgj == 0) mfgj = nfacet_cols
        par(mfg = c(mfgi, mfgj))

        # For free facets, we need to reset par(usr) based extent of that
        # particular facet... which we calculated and saved to the .fusr env var
        # (list) back in draw_facet_window()
        if (isTRUE(facet.args[["free"]])) {
          fusr = get(".fusr", envir = get(".tinyplot_env", envir = parent.env(environment())))
          par(usr = fusr[[ii]])
        }
      }

      # empty plot flag
      empty_plot = FALSE
      if (isTRUE(empty) || isTRUE(type == "n") || ((length(ix) == 0) && !(type %in% c("histogram", "hist", "rect", "segments", "spineplot")))) {
        empty_plot = TRUE
      }

      # Draw the individual plot elements...
      if (!isTRUE(empty_plot)) {
        if (is.null(type_draw)) {
          type_draw = switch(type,
            "ribbon" = type_ribbon()$draw,
            "polygon" = type_polygon()$draw,
            "rect" = type_rect()$draw,
            "p" = ,
            "points" = type_points()$draw,
            "l" = ,
            "o" = ,
            "b" = ,
            "c" = ,
            "h" = ,
            "s" = ,
            "S" = type_lines(type = type)$draw
          )
        }
        type_draw(
          ibg = ibg,
          icol = icol,
          ilty = ilty,
          ilwd = ilwd,
          ipch = ipch,
          ix = ix,
          ixmax = ixmax,
          ixmin = ixmin,
          iy = iy,
          iymax = iymax,
          iymin = iymin,
          ilabels = ilabels,
          iz = iz,
          cex = cex,
          dots = dots,
          type = type,
          x_by = x_by,
          iby = i,
          ifacet = ii,
          facet_by = facet_by,
          data_facet = idata,
          data_by = split_data,
          flip = flip,
          type_info = type_info,
          facet_window_args = facet_window_args)
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
    type = NULL,
    xlim = NULL,
    ylim = NULL,
    # log = "",
    main = NULL,
    sub = NULL,
    xlab = NULL,
    ylab = NULL,
    ann = par("ann"),
    axes = TRUE,
    frame.plot = NULL,
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
    ...) {
  ## formula for variables must be specified through 'x' or 'formula' but not both
  if (is.null(x)) {
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

  ## placeholder for legend title
  legend_args = list(x = NULL)

  ## process all formulas
  tf = tinyformula(formula, facet)

  ## set up model frame
  m = match.call(expand.dots = FALSE)
  m = m[c(1L, match(c("formula", "data", "subset", "na.action", "drop.unused.levels"), names(m), 0L))]
  m$formula = tf$full
  ## need stats:: for non-standard evaluation
  m[[1L]] = quote(stats::model.frame)
  mf = eval.parent(m)

  ## extract x
  x = tinyframe(tf$x, mf)
  xnam = names(x)[[1L]]
  if (length(names(x)) != 1L) warning(paste("formula should specify exactly one x-variable, using:", xnam))
  x = x[[xnam]]

  ## extract y (if any)
  y = tinyframe(tf$y, mf)
  if (!is.null(y)) {
    ynam = names(y)[[1L]]
    if (length(names(y)) > 1L) warning(paste("formula should specify at most one y-variable, using:", ynam))
    y = y[[ynam]]
  }

  ## extract by (if any)
  by = tinyframe(tf$by, mf)
  if (!is.null(by)) {
    bynam = names(by)
    by = if (length(bynam) == 1L) by[[bynam]] else interaction(by, sep = ":")
  }

  ## extract x/y facet (if formula)
  if (!is.null(tf$xfacet) || !is.null(tf$yfacet)) {
    xfacet = tinyframe(tf$xfacet, mf)
    yfacet = tinyframe(tf$yfacet, mf)
    if (!is.null(xfacet)) xfacet = if (ncol(xfacet) == 1L) xfacet[[1L]] else interaction(xfacet, sep = ":")
    if (!is.null(yfacet)) yfacet = if (ncol(yfacet) == 1L) yfacet[[1L]] else interaction(yfacet, sep = ":")
    if (is.null(yfacet)) {
      facet = xfacet
    } else {
      facet = interaction(xfacet, yfacet, sep = "~")
      attr(facet, "facet_grid") = TRUE
      attr(facet, "facet_nrow") = length(unique(yfacet))
    }
  }

  ## nice axis and legend labels
  dens_type = (is.atomic(type) && identical(type, "density")) || (!is.atomic(type) && identical(type$name, "density"))
  hist_type = (is.atomic(type) && type %in% c("hist", "histogram")) || (!is.atomic(type) && identical(type$name, "histogram"))
  if (!is.null(type) && dens_type) {
    if (is.null(ylab)) ylab = "Density"
    if (is.null(xlab)) xlab = xnam
  } else if (!is.null(type) && hist_type) {
    if (is.null(ylab)) ylab = "Frequency"
    if (is.null(xlab)) xlab = xnam
  } else if (is.null(y)) {
    if (is.null(ylab)) ylab = xnam
    if (is.null(xlab)) xlab = "Index"
  } else {
    if (is.null(ylab)) ylab = ynam
    if (is.null(xlab)) xlab = xnam
  }
  if (!is.null(by)) {
    legend_args[["title"]] = if (length(bynam) == 1L) bynam else sprintf("interaction(%s)", paste(bynam, collapse = ", "))
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

#' @rdname tinyplot
#' @export
tinyplot.density = function(
    x = NULL,
    type = c("l", "area"),
    ...) {
  
  dots = list(...)
  
  if (!is.null(dots[["by"]]) || !is.null(dots[["facet"]])) {
    stop(
      '\nGrouped and/or faceted plots are no longer supported with the tinyplot.density() method. ',
      '\nPlease use the dedicated type argument instead, e.g. `tinyplot(..., type = "density")`. See `?type_density` for details.',
      '\n\nThis breaking change was introduced in tinyplot v0.3.0.'
    )
  }
  
  type = match.arg(type)
  
  ## override if bg = "by"
  if (!is.null(dots[["bg"]]) || !is.null(dots[["fill"]])) type = "area"
  
  if (inherits(x, "density")) {
    object = x
    # legend_args = list(x = NULL)
    # # Grab by label to pass on legend title to tinyplot.default
    # legend_args[["title"]] = deparse(substitute(by))
  } else {
    ## An internal catch for non-density objects that were forcibly
    ## passed to tinyplot.density (e.g., via a one-side formula)
    if (anyNA(x)) {
      x = na.omit(x)
      x = as.numeric(x)
    }
    object = density(x)
  }
  
  x = object$x
  y = object$y
  
  if (type == "area") {
    ymin = rep(0, length(y))
    ymax = y
    # # set extra legend params to get bordered boxes with fill
    # legend_args[["x.intersp"]] = 1.25
    # legend_args[["lty"]] = 0
    # legend_args[["pt.lwd"]] = 1
  }
  
  # splice in change arguments
  dots[["x"]] = x
  dots[["y"]] = y
  dots[["type"]] = type
  
  ## axes range
  if (is.null(dots[["xlim"]])) dots[["xlim"]] = range(x)
  if (is.null(dots[["ylim"]])) dots[["ylim"]] = range(y)
  
  ## nice labels and titles
  if (is.null(dots[["ylab"]])) dots[["ylab"]] = "Density"
  if (is.null(dots[["xlab"]])) dots[["xlab"]] = paste0("N = ", object$n, "   Bandwidth = ", sprintf("%.4g", object$bw))
  if (is.null(dots[["main"]])) dots[["main"]] = paste0(paste(object$call, collapse = "(x = "), ")")
  
  do.call(tinyplot.default, args = dots)
  
}


#' @export
#' @name plt
#' @rdname tinyplot
plt = tinyplot
