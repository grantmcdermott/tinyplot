#' @title Generic function for adding an axis to a (tiny)plot
#'   
#' @description Internal function used for adding an axis to a [`tinyplot`]
#'   call.
#' @details `tinyAxis` provides a thin(ish) wrapper around
#'   \code{\link[graphics]{Axis}}, but with enhanced flexibility to (i) match
#'   parameter combinations based on the axis type and plotting theme, (ii)
#'   provide better support for date-time variables, and (iii) enable convenient
#'   formatting of axis tick labels.
#' @inheritParams graphics::Axis
#' @param type the type of axis to be drawn; inherited from the `xaxt` or `yaxt`
#'   arguments of the parent [`tinyplot()`] call. One of either: `"standard"`
#'   (default that draws the axis, ticks, and labels), `"none"` (no axes),
#'   `"ticks"` (only ticks and labels without axis line), `"labels"` (only
#'   labels without ticks and axis line), or `"axis"` (only axis line and labels
#'   but no ticks). Partial matching is allowed, e.g. `type = "s"`.
#' @inheritParams tinylabel
#' @examples
#' \dontrun{
#' 
#' # plot without axes
#' tinyplot(0:10, axes = "n")
#' # add x-axis (labels only)
#' tinyplot:::tinyAxis(x = 0:10, side = 1, type = "l")
#' # add y-axis (with custom label formatting)
#' tinyplot:::tinyAxis(x = 0:10, side = 2, type = "s", labeller = "$")
#' }
#' @keywords internal
tinyAxis = function(x = NULL, ..., type = "standard", labeller = NULL) {
  type = match.arg(type, c("standard", "none", "labels", "ticks", "axis"))
  if (type == "none") {
    invisible(numeric(0L))
  } else {
    args = list(x = x, ...)
    if (type == "labels") {
      args$tick = FALSE
    } else if (type == "ticks") {
      args$lwd = 0
      if (!("lwd.ticks" %in% names(args))) args$lwd.ticks = 1
    } else if (type == "axis") {
      args$lwd.ticks = 0
    } else {
      args$tick = TRUE
    }
    if (!is.null(labeller)) {
      if (!is.null(args$at)) {
        args$labels = if (!is.null(args$labels)) tinylabel(args$labels, labeller) else tinylabel(args$at, labeller)
      } else {
        args$at = if (!inherits(x, c("POSIXt", "Date"))) axTicks(args$side) else axTicksDateTime(args$side, x = x)  
        args$labels = tinylabel(args$at, labeller)
      }
    }
    do.call("Axis", args)
  }
}

# Special case for Date-Time, adapted/simplified from axis.date()
axTicksDateTime = function(side, x, ...) {
  if (inherits(x, "POSIXt")) {
    tz = attr(x, "tz")
    range = extendrange(x)
    rangeDateTime = .POSIXct(range, tz = tz)
  } else {
    range = sort(par("usr")[if (side%%2)  1L:2L else 3:4L])
    range[1L] = ceiling(range[1L])
    range[2L] = floor(range[2L])
    rangeDateTime = range
    class(rangeDateTime) = "Date"
  }
  z = pretty(rangeDateTime, n = par("lab")[2 - side%%2])
  keep = z >= range[1L] & z <= range[2L]
  z = z[keep]
  return(z)
}


## Categorical y-axis tick labels, for margin measurement.
##
## Feeds axis_tick_labels() below, which resolves the label set that
## tick_label_extent() then measures.
##
## Returns a one-element list wrapping the label set when a type puts categories
## on the y axis, or NULL when it does not and the caller should fall back to its
## own axisTicks() computation. The wrapper matters: `levels(y)` is itself NULL
## for a ridge plot over a *numeric* y, and that empty result must stay
## distinguishable from "this isn't a categorical axis" -- otherwise the caller
## would substitute numeric ticks and bump the margin that the label-less axis
## does not need.
##
## `ylabs` covers the general case of a type that has placed named categories on
## the y axis. The ridge and flipped-boxplot cases are special: ridge takes its
## categories from the y factor's levels, while a flipped boxplot has had its
## categories swapped onto `xlabs` by flip_datapoints().
y_axis_labels = function(type, y, ylabs, xlabs, flip) {
  if (identical(type, "ridge")) {
    return(list(levels(y)))
  }
  if (!is.null(ylabs)) {
    return(list(if (!is.null(names(ylabs))) names(ylabs) else ylabs))
  }
  if (identical(type, "boxplot") && isTRUE(flip) && !is.null(xlabs)) {
    return(list(if (!is.null(names(xlabs))) names(xlabs) else xlabs))
  }
  NULL
}


## Categorical x-axis tick labels, for margin measurement. The x-side counterpart
## to y_axis_labels(), and trivial by comparison: no type takes its x categories
## from anywhere but `xlabs`. It exists so both sides feed axis_tick_labels() the
## same one-element-list shape.
x_axis_labels = function(xlabs) {
  if (is.null(xlabs)) return(NULL)
  list(if (!is.null(names(xlabs))) names(xlabs) else xlabs)
}


## Tick labels that a side's axis will draw, for margin measurement.
##
## Three places need this: the dynmar precompute in tinyplot.default(), and the
## faceted and non-faceted branches of draw_facet_window(). Each used to rebuild
## the label set inline, and they had drifted -- only the faceted branch lacked
## the degenerate-range guard below, so a faceted plot over a single distinct
## value handed axisTicks() a zero-width usr.
##
## `labelset` is the tri-state result of x_axis_labels()/y_axis_labels(): NULL
## when no type has put categories on this axis (fall back to axisTicks), or a
## one-element list wrapping the categories. The wrapper matters because that
## element may legitimately be empty -- see y_axis_labels() for why.
##
## `free_lims` carries the per-facet limits under free scales. The margin has to
## clear every panel's ticks, so all of them are measured and the widest wins.
axis_tick_labels = function(labelset, lim, axb = NULL, axl = NULL, log = FALSE,
                            free_lims = NULL, cex = 1) {
  if (!is.null(labelset)) {
    out = labelset[[1L]]
  } else if (!is.null(free_lims)) {
    sets = lapply(free_lims, function(l) {
      axisTicks(usr = extendrange(l, f = 0.04), log = log)
    })
    widths = vapply(
      sets,
      function(s) max(strwidth(s, "inches", cex = cex)),
      numeric(1L)
    )
    out = sets[[which.max(widths)]]
  } else {
    # A single distinct value gives a zero-width range that extendrange() can't
    # pad and axisTicks() can't tick, so widen it the way plot.window() does.
    # An explicit `at` (xaxb/yaxb) supplies its own ticks, hence the guard.
    usr = if (diff(lim) == 0 && is.null(axb)) {
      lim + c(-0.5, 0.5)
    } else {
      extendrange(lim, f = 0.04)
    }
    out = axisTicks(usr = usr, log = log)
  }
  if (!is.null(axl)) out = tinylabel(out, axl)
  out
}


## Margin lines a side must reserve to clear its tick labels ("whtsbp").
##
## Under las 1:2 the y labels are horizontal and their *width* eats into mar[2];
## under las 2:3 the x labels are vertical and that same width eats into mar[1].
## One measurement serves both.
##
## strwidth("inches") / csi is exactly what the previous spelling,
##   grconvertX(strwidth(labels, "figure"), from = "nfc", to = "lines"),
## computed -- the figure width cancels between the two conversions. The 0.5
## subtracted is the allowance the tick row already provides. Both forms work
## before plot.new(), which matters for the dynmar precompute.
##
## An axis with no labels at all measures zero width and so returns -0.5, not 0:
## there is nothing to clear, and the tick-row allowance still has to come back
## off. Callers guard the sign themselves, because they do not agree on what a
## non-positive result means -- see draw_facet_window().
tick_label_extent = function(labels, cex = 1) {
  # An empty (as opposed to zero-width) set maxes to -Inf; treat it as no ink.
  w = suppressWarnings(max(strwidth(labels, "inches", cex = cex)))
  if (!is.finite(w)) w = 0
  w / par("csi") - 0.5
}
