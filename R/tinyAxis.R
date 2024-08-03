## auxiliary Axis() interface with different parameter combinations based on type
tinyAxis = function(x = NULL, ..., type = "standard") {
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
    do.call("Axis", args)
  }
}
