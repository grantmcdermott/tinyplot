#' Ribbon and area plot types
#'
#' @param alpha numeric value between 0 and 1 specifying the opacity of ribbon shading
#'   If no `alpha` value is provided, then will default to `tpar("ribbon.alpha")`
#'   (i.e., probably `0.2` unless this has been overridden by the user in their global
#'   settings.)
#' @param stack logical. Should the `by` groups be stacked on top of one
#'   another, rather than overplotted from a common zero baseline? Only
#'   relevant for grouped area plots. Default is `FALSE`. See the "Stacked
#'   area plots" section below.
#' @param byord controls the order of the `by` groups and, thus, the order in
#'   which they stack. Three keywords rank the groups according to their `y`
#'   values along the `x` axis: `"start"` (value at the smallest `x`), `"end"`
#'   (value at the largest `x`), and `"total"` (summed `y` values over the full
#'   `x` axis range). Each ranks the largest group first, i.e. into the bottom
#'   band. A fourth keyword, `"asis"`, takes the groups in the order that they
#'   appear in the data. Users can also pass their own custom function to
#'   determine both the ranking statistic and its direction, e.g.
#'   `function(y) -median(y)` would layer by median `y` value, from the biggest
#'   to the smallest. Default is `NULL`, in which case the existing factor level
#'   order is retained; to set that order explicitly, call
#'   `factor(levels = )` on the grouping variable beforehand. See Examples, as
#'   well as the "Stacked area plots" section below.
#' @param FUN a function for collapsing repeated `y` values within a group and
#'   `x` position, used only when `stack = TRUE`. Defaults to `mean`, matching
#'   [`type_barplot()`], so that the same data stacks to the same heights
#'   whether it is drawn as bars or as an area.
#' @inheritParams type_errorbar
#'
#' @description Type constructor functions for producing polygon ribbons, which
#' define a `y` interval (usually spanning from `ymin` to `ymax`) for each
#' `x` value. Area plots are a special case of ribbon plot where `ymin` is
#' set to 0 and `ymax` is set to `y`.
#' 
#' @section Dodging ribbon plots:
#' 
#' We support dodging for grouped ribbon plots, enabling similar functionality
#' to dodged errorbar and pointrange plots. However, it is strongly recommended
#' that dodging is only implemented for cases where the x-axis comprises a
#' limited number of discrete cases (e.g., coefficient or event-study plots).
#' See Examples.
#'
#' @section Stacked area plots:
#'
#' Passing `type_area(stack = TRUE)` stacks the `by` groups cumulatively,
#' rather than drawing each one from a zero baseline. Groups are accumulated in
#' the order of their (factor) levels, so the first level forms the bottom band
#' and the top of the final band traces the group total. Stacking is computed
#' separately within each facet.
#'
#' The `byord` argument is a helpful companion to stacked area plots, since
#' it controls which group ends up where. Most useful are the three positional
#' keywords: `"start"`, `"end"`, and `"total"`. These rank the stacked `by`
#' groups according to their `y` values at the designated position along the
#' `x` axis. Following convention, the ranking runs in descending order, so that
#' the biggest group is drawn on the bottom layer to provide a stable visual
#' baseline.
#'
#' Stacking needs exactly one `y` value per group per `x` value. Repeated cells
#' ---typically caused by a variable that is present in the data but absent from
#' the plot---are collapsed with `FUN` (default `mean`) rather than being
#' stacked against each other. Conversely, groups that are *missing* an `x`
#' value (or have an `NA` there) count as contributing zero at that point, so
#' that a gap in one group does not shift the groups stacked above it. Note that
#' stacking negative values is not meaningful and will produce overlapping
#' bands.

#' Note that the legend key for stacked area plots is deliberately inverted
#' compared to other plot types (including non-stacked area plots) to ensure a
#' consistent ordering with the "bottoms-up" layering of the stacked regions.
#' Similarly, reordering of the `by` group levels will reassign the palette,
#' since group colours are allocated by level position. This matches what
#' releveling a factor does elsewhere, but it does mean that reordering the
#' bands repaints them.
#'
#' Finally, note that unlike non-stacked area plots, the stacked bands are
#' drawn with opaque fill by default, since they do not overlap. Pass an
#' explicit `alpha` or `fill` value to override.
#'
#' @examples
#' x = 1:100 / 10
#' y = sin(x)
#'
#' #
#' ## Ribbon plots
#'
#' # "ribbon" convenience string
#' tinyplot(x = x, ymin = y - 1, ymax = y + 1, type = "ribbon")

#' # Same result with type_ribbon()
#' tinyplot(x = x, ymin = y-1, ymax = y+1, type = type_ribbon())
#'
#' # y will be added as a line if it is specified
#' tinyplot(x = x, y = y, ymin = y-1, ymax = y+1, type = "ribbon")
#'
#' #
#' ## Area plots
#'
#' # "area" type convenience string
#' tinyplot(x, y, type = "area")
#'
#' # Same result with type_area()
#' tinyplot(x, y, type = type_area())
#'
#' # Area plots are often used for time series charts
#' tinyplot(AirPassengers, type = "area")
#'
#' #
#' ## Dodged ribbon/area plots
#' 
#' # Dodged ribbon or area plots can be useful in cases where there is strong
#' # overlap across groups (and a limited number of discrete x-axis values).
#' 
#' dat = data.frame(
#'   x = rep(c("Before", "After"), each = 2),
#'   grp = rep(c("A", "B"), 2),
#'   y = c(10, 10.5, 15, 15.3),
#'   lwr = c(8, 8.5, 13, 13.3),
#'   upr = c(12, 12.5, 17, 17.3)
#' )
#' 
#' tinyplot(
#'   y ~ x | grp,
#'   data = dat,
#'   ymin = lwr, ymax = upr,
#'   type = type_ribbon(),
#'   main = "Overlappling ribbons"
#' )
#' 
#' tinyplot(
#'   y ~ x | grp,
#'   data = dat,
#'   ymin = lwr, ymax = upr,
#'   type = type_ribbon(dodge = 0.1),
#'   main = "Dodged ribbons"
#' )
#'
#' #
#' ## Stacked area plots
#'
#' # Grouped area plots can be stacked cumulatively, rather than being drawn
#' # from a common zero baseline.
#'
#' dat = expand.grid(year = 2000:2020, grp = factor(c("A", "B", "C")))
#' dat$val = abs(sin(dat$year / 3) + as.integer(dat$grp)) + 1
#'
#' tinyplot(val ~ year | grp, data = dat, type = type_area(stack = TRUE))
#'
#' # Use `byord` to control which group stacks where. Here we stack by their
#' # largest end value.
#'
#' tinyplot(
#'   val ~ year | grp, data = dat,
#'   type = type_area(stack = TRUE, byord = "end")
#' )
#'
#' # Stacking expects a single `y` value per group per `x` value. Any repeats
#' # are collapsed for us first, using `FUN` (`mean` by default). Here, for
#' # instance, ChickWeight records many chicks per diet at each timepoint.
#'
#' tinyplot(
#'   weight ~ Time | Diet, data = ChickWeight,
#'   type = type_area(stack = TRUE, FUN = median)
#' )
#'
#' @export
type_ribbon = function(alpha = NULL, dodge = 0, fixed.dodge = FALSE) {
    out = list(
        draw = draw_ribbon(),
        data = data_ribbon(ribbon.alpha = alpha, dodge = dodge, fixed.dodge = fixed.dodge),
        name = "ribbon"
    )
    class(out) = "tinyplot_type"
    return(out)
}


draw_ribbon = function() {
    fun = function(ix, iy, ixmin, ixmax, iymin, iymax, ibg, ilty, ilwd, icol, ipch, i, flip = FALSE, ...) {
        polyg = type_polygon()$draw
        lin = type_lines()$draw
        if (isFALSE(flip)) {
            polyg(ix = c(ix, rev(ix)), iy = c(iymin, rev(iymax)), icol = NA, ibg = ibg)
        } else {
            polyg(c(ixmin, rev(ixmax)), iy = c(iy, rev(iy)), icol = NA, ibg = ibg)
        }
        lin(ix = ix, iy = iy, icol = icol, ipch = ipch, ibg = ibg, ilty = ilty, ilwd = ilwd, type = "l")
    }
    return(fun)
}


data_ribbon = function(ribbon.alpha = NULL, dodge = 0, fixed.dodge = FALSE) {
    ribbon.alpha = sanitize_ribbon_alpha(ribbon.alpha)
    fun = function(settings, ...) {
        env2env(settings, environment(), c("datapoints", "xlabs", "null_by", "null_facet"))
        # Convert x to factor if it's not already
        if (is.character(datapoints$x)) {
            datapoints$x = as.factor(datapoints$x)
        }

        if (is.factor(datapoints$x)) {
            xlvls = levels(datapoints$x)
            xlabs = seq_along(xlvls)
            names(xlabs) = xlvls
            datapoints$x = as.integer(datapoints$x)
        } else {
            xlabs = NULL
        }
        
        # dodge (auto-detects x, xmin, xmax columns)
        if (dodge != 0) {
            datapoints = dodge_positions(datapoints, dodge, fixed.dodge)
        }

        if (null_by && null_facet) {
            xord = order(datapoints$x)
        } else if (null_facet) {
            xord = order(datapoints$by, datapoints$x)
        } else if (null_by) {
            xord = order(datapoints$facet, datapoints$x)
        } else {
            xord = order(datapoints$by, datapoints$facet, datapoints$x)
        }

        # Reorder x, y, ymin, and ymax based on the order determined
        datapoints = datapoints[xord, ]

        # Catch for missing ymin and ymax
        if (is.null(datapoints$ymin)) datapoints$ymin = datapoints$y
        if (is.null(datapoints$ymax)) datapoints$ymax = datapoints$y

        x = datapoints$x
        y = datapoints$y
        ymin = datapoints$ymin
        ymax = datapoints$ymax
        by = if (length(unique(datapoints$by)) > 1) datapoints$by else NULL
        facet = if (length(unique(datapoints$facet)) > 1) datapoints$facet else NULL

        # ribbon.alpha comes from parent scope, so assign it locally
        ribbon.alpha = ribbon.alpha

        # legend customizations
        settings$legend_args[["pch"]] = settings$legend_args[["pch"]] %||% 22
        settings$legend_args[["pt.cex"]] = settings$legend_args[["pt.cex"]] %||% 3.5
        settings$legend_args[["pt.lwd"]] = settings$legend_args[["pt.lwd"]] %||% 0
        settings$legend_args[["y.intersp"]] = settings$legend_args[["y.intersp"]] %||% 1.25
        settings$legend_args[["seg.len"]] = settings$legend_args[["seg.len"]] %||% 1.25

        vars_to_copy = c("x", "y", "ymin", "ymax", "xlabs", "datapoints", "ribbon.alpha")
        if (!is.null(by)) vars_to_copy = c(vars_to_copy, "by")
        if (!is.null(facet)) vars_to_copy = c(vars_to_copy, "facet")

        env2env(environment(), settings, vars_to_copy)
    }
    return(fun)
}
