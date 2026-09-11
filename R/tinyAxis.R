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
#' @param srt numeric giving the tick label rotation in degrees
#'   counter-clockwise, or `NULL` (default) to leave the labels to base
#'   `axis()` and its `las` setting. Any non-zero value is drawn by hand, since
#'   `axis()` only understands the four right angles that `las` selects.
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
tinyAxis = function(x = NULL, ..., type = "standard", labeller = NULL,
                    srt = NULL) {
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
    if (!is.null(srt) && is.finite(srt) && srt %% 360 != 0) {
      # Draw the line and ticks, but no labels -- Axis() hands back the tick
      # positions it settled on, which is also what the rotated text needs.
      lab = args[["labels"]]
      args[["labels"]] = FALSE
      # Take a date-time axis's ticks here rather than letting Axis() pick them,
      # so that pretty()'s chosen label format comes back with them.
      if (is.null(args[["at"]]) && inherits(x, c("Date", "POSIXt"))) {
        args[["at"]] = axTicksDateTime(args[["side"]], x = x)
      }
      at = do.call("Axis", args)
      if (is.null(lab) || isTRUE(lab)) {
        fmt = attr(args[["at"]], "format")
        lab = if (!is.null(fmt)) format(at, format = fmt) else format(at, trim = TRUE)
      }
      draw_rotated_labels(
        side = args[["side"]], at = at, labels = lab, srt = srt,
        cex = args[["cex.axis"]] %||% par("cex.axis"),
        col = args[["col.axis"]], font = args[["font.axis"]]
      )
      return(invisible(at))
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
  # pretty() picks a compact label format for the range and returns it as an
  # attribute, which is what axis.Date()/axis.POSIXct() label with. Subsetting
  # drops it, so carry it over for callers that format the ticks themselves.
  fmt = attr(z, "format")
  keep = z >= range[1L] & z <= range[2L]
  z = z[keep]
  if (!is.null(fmt)) attr(z, "format") = fmt
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
  ticks = function(l) {
    u = axis_usr(l, log = log, axb = axb)
    axisTicks(usr = u[["usr"]], log = u[["log"]])
  }
  if (!is.null(labelset)) {
    out = labelset[[1L]]
  } else if (!is.null(free_lims)) {
    sets = lapply(free_lims, ticks)
    widths = vapply(
      sets,
      function(s) max(strwidth(s, "inches", cex = cex)),
      numeric(1L)
    )
    out = sets[[which.max(widths)]]
  } else {
    out = ticks(lim)
  }
  if (!is.null(axl)) out = tinylabel(out, axl)
  out
}


## Axis limits in the coordinate space axisTicks() and par("usr") speak.
##
## Callers hold limits in *data* units, but on a log axis axisTicks() reads
## its `usr` in log10 units. Keeping that conversion here -- rather than
## leaving each call site to remember it -- is the point of this helper: the
## units contract and the padding rules stay in one place. Returns the padded
## limits together with the log flag they were built under, so the caller
## hands axisTicks() a matching pair.
##
## Margin measurement runs *before* plot.window(), so `log` must come from the
## caller's own `log=` argument. par("xlog")/par("ylog") still describe the
## previous plot on the device at that point (#725).
##
## A log axis can't represent a zero or negative limit, so those fall back to a
## linear measurement: plot.window() raises its own, clearer complaint moments
## later, and log10() here would only put "NaNs produced" in front of it.
axis_usr = function(lim, log = FALSE, axb = NULL) {
  log = isTRUE(log) && all(is.finite(lim)) && all(lim > 0)
  if (log) lim = log10(lim)
  # A single distinct value gives a zero-width range that extendrange() can't
  # pad and axisTicks() can't tick, so widen it the way plot.window() does.
  # An explicit `at` (xaxb/yaxb) supplies its own ticks, hence the guard.
  usr = if (diff(lim) == 0 && is.null(axb)) {
    lim + c(-0.5, 0.5)
  } else {
    extendrange(lim, f = 0.04)
  }
  list(usr = usr, log = log)
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
## `srt` rotates the labels off the axis, in degrees counter-clockwise. The
## margin then has to clear the label's extent *perpendicular* to its axis,
## which mixes the string's width and its height as it turns:
##
##   sides 1/3:  w*|sin(srt)| + h*|cos(srt)|
##   sides 2/4:  w*|cos(srt)| + h*|sin(srt)|
##
## h is half the ink height of a text line, because the label is anchored on its
## centre line and so only half of it projects onto the perpendicular. The full
## line-height allowance dynmar_side() uses is wrong here: it over-reserves by
## ~0.7*|cos(srt)| lines, which is visible as a gap between the tilted labels
## and the axis title, since this one number places both.
##
## Both existing callers are recovered exactly -- side 1 at srt = 90 (las 2:3)
## and side 2 at srt = 0 (las 1:2) both zero the h term and reduce to plain w --
## so `srt = NULL` and those two angles stay interchangeable.
tick_label_extent = function(labels, cex = 1, srt = NULL, side = 1L) {
  # An empty (as opposed to zero-width) set maxes to -Inf; treat it as no ink.
  w = suppressWarnings(max(strwidth(labels, "inches", cex = cex)))
  if (!is.finite(w)) w = 0
  w = w / par("csi")
  if (is.null(srt)) return(w - 0.5)
  rad = srt * pi / 180
  h = 0.3 * cex
  perp = if (side %in% c(1L, 3L)) {
    w * abs(sin(rad)) + h * abs(cos(rad))
  } else {
    w * abs(cos(rad)) + h * abs(sin(rad))
  }
  perp - 0.5
}


## How far the end labels of a rotated axis reach *along* it, in margin lines.
##
## A vertical (las = 2) label sits in its own tick's column, so this never came
## up before. Tilt it and the string leans sideways: on side 1 a positive `srt`
## trails the first label off the left of the plot region, a negative one trails
## the last off the right, by w*|cos(srt)| either way. Returns c(low, high) --
## (left, right) for sides 1/3, (bottom, top) for 2/4 -- so the caller can widen
## the margins that the lean would otherwise overrun.
##
## Only the part of the lean that clears the plot region needs margin. The end
## ticks are usually inset from the edge -- half a category on a discrete axis,
## the extendrange() padding on a continuous one -- and the label leans across
## that inset first, over the panel, before it reaches the edge. `inset` is that
## gap at each end, in margin lines; ignoring it over-reserves by exactly the
## inset, which on a three-category axis is a sixth of the panel.
tick_label_overhang = function(labels, cex = 1, srt = 0, side = 1L,
                               inset = c(0, 0)) {
  n = length(labels)
  if (!n || !is.finite(srt) || srt %% 180 == 0) return(c(0, 0))
  rad = srt * pi / 180
  ends = suppressWarnings(
    strwidth(labels[c(1L, n)], "inches", cex = cex) / par("csi")
  )
  ends[!is.finite(ends)] = 0
  # The anchored end is the one nearest the plot, so the string leans away from
  # it: towards the low end of the axis for a positive srt, the high end for a
  # negative one. Only the label at that end of the axis can overrun.
  lean = if (side %in% c(1L, 3L)) abs(cos(rad)) else abs(sin(rad))
  reach = if (srt > 0) c(ends[1L] * lean, 0) else c(0, ends[2L] * lean)
  inset[!is.finite(inset)] = 0
  pmax(0, reach - inset)
}


## The gap between each end of an axis's plot region and its outermost tick, in
## margin lines -- what a leaning label crosses before it overruns the panel.
##
## `at` are the tick positions and `usr` the region's extent, both in user
## coordinates; `span_lines` is the region's width (or height) in lines. The
## caller has to estimate that span from the margins it has computed so far,
## which is one iteration short of exact: the lean it is about to add will widen
## the margin slightly, shrinking the panel and so the inset. The error is a
## fraction of a line and always in the safe direction (a slightly larger
## reservation than needed).
axis_tick_inset = function(at, usr, span_lines) {
  if (!length(at) || length(usr) != 2L || !is.finite(span_lines)) return(c(0, 0))
  at = at[is.finite(at)]
  w = diff(range(usr))
  if (!length(at) || !is.finite(w) || w <= 0) return(c(0, 0))
  lo = (min(at) - min(usr)) / w
  hi = (max(usr) - max(at)) / w
  pmax(0, c(lo, hi) * span_lines)
}


## Where to anchor a rotated tick label, as a text() `adj` pair.
##
## The anchor goes on the end of the string nearest the plot, so the rest of it
## leans away into the margin rather than back over the data. Which end that is
## depends on how the string is pointing: on side 1 a string tilting up to the
## right (sin(srt) > 0) is nearest the plot at its right end, so anchor there.
##
## When the string runs parallel to the axis there is no nearest end, so it is
## centred along the axis and anchored on the edge facing the plot instead --
## adj[2] = 1 puts the anchor at the top of the ink, 0 at the baseline.
rotated_label_adj = function(side, srt) {
  rad = srt * pi / 180
  along = if (side %in% c(1L, 3L)) sin(rad) else cos(rad)
  # Guard the parallel case with a tolerance: sin(pi) is 1.2e-16, not 0.
  if (abs(along) < 1e-8) {
    centred = switch(as.character(side), "1" = 1, "3" = 0, "2" = 0, "4" = 1)
    return(c(0.5, centred))
  }
  near_high_end = if (side %in% c(1L, 2L)) along > 0 else along < 0
  c(if (near_high_end) 1 else 0, 0.5)
}


## Draw an axis's tick labels at an arbitrary rotation.
##
## base axis() only understands las 0:3, so anything else has to be drawn by
## hand: the caller suppresses axis()'s own labels and calls this for the line
## of text. Labels sit mgp[2] lines off the axis, where axis() puts its own, and
## are drawn with xpd = NA so a long string can lean out into the margin.
##
## The offset is a *line* count, so it converts to user coordinates differently
## at every device size. Computing it once at draw time would bake in the
## coordinates of whatever device happened to be current, and the labels would
## drift off the axis on the next resize -- hence recordGraphics(), which
## re-runs the whole placement against the replayed device.
draw_rotated_labels = function(side, at, labels, srt, cex = 1, col = NULL,
                               font = NULL) {
  if (!length(at) || !length(labels)) return(invisible(NULL))
  recordGraphics(
    rotated_labels_draw(side, at, labels, srt, cex, col, font),
    list = list(
      side = side, at = at, labels = labels, srt = srt,
      cex = cex, col = col, font = font
    ),
    env = getNamespace("tinyplot")
  )
}

rotated_labels_draw = function(side, at, labels, srt, cex, col, font) {
  usr = par("usr")
  off = par("mgp")[2L] * par("csi")
  adj = rotated_label_adj(side, srt)
  args = list(labels = labels, srt = srt, adj = adj, cex = cex, xpd = NA)
  if (!is.null(col)) args[["col"]] = col
  if (!is.null(font)) args[["font"]] = font
  if (side %in% c(1L, 3L)) {
    edge = if (side == 1L) usr[3L] else usr[4L]
    sgn = if (side == 1L) -1 else 1
    args[["x"]] = at
    args[["y"]] = grconvertY(
      grconvertY(edge, "user", "inches") + sgn * off, "inches", "user"
    )
  } else {
    edge = if (side == 2L) usr[1L] else usr[2L]
    sgn = if (side == 2L) -1 else 1
    args[["y"]] = at
    args[["x"]] = grconvertX(
      grconvertX(edge, "user", "inches") + sgn * off, "inches", "user"
    )
  }
  do.call(text, args)
  invisible(NULL)
}
