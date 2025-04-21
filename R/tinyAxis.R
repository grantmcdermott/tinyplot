#' auxiliary Axis() interface with different parameter combinations based on type
#'
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
        args$at = if (!inherits(x, c("POSIXt", "Date"))) axTicks(args$side) else axTicksDateTime(args$side)  
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
