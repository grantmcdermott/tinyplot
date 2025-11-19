sanitize_axes = function(settings) {
  list2env(settings[c("axes", "xaxt", "yaxt", "frame.plot")], environment())
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


  settings = update_settings(settings, axes = axes, xaxt = xaxt, yaxt = yaxt, frame.plot = frame.plot)
  return(settings)
}
