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
