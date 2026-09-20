sanitize_axes = function(settings) {
  env2env(settings, environment(),
          c("axes", "xaxt", "yaxt", "frame.plot", "xaxr", "yaxr", "xpad", "ypad",
            "las"))
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


  ## tick label rotation: an explicit x/yaxr wins over the theme's tpar setting.
  ## A rotation of exactly 0 is the same as none, and saying so here keeps the
  ## "is it rotated?" test downstream a plain is.null().
  if (is.null(xaxr)) xaxr = get_tpar("xaxr")
  if (is.null(yaxr)) yaxr = get_tpar("yaxr")
  assert_numeric(xaxr, len = 1, null.ok = TRUE, name = "xaxr")
  assert_numeric(yaxr, len = 1, null.ok = TRUE, name = "yaxr")
  if (!is.null(xaxr) && (!is.finite(xaxr) || xaxr %% 360 == 0)) xaxr = NULL
  if (!is.null(yaxr) && (!is.finite(yaxr) || yaxr %% 360 == 0)) yaxr = NULL

  ## tick label orientation. Unlike x/yaxr, not resolved against get_tpar():
  ## that reads the global .tpar, which lacks the active theme's own las (queued
  ## in a hook), so folding it in would overwrite a theme's las with 0.
  assert_numeric(las, len = 1, lower = 0, upper = 3, null.ok = TRUE, name = "las")
  if (!is.null(las) && las %% 1 != 0) {
    stop("`las` must be a whole number in 0:3", call. = FALSE)
  }

  ## axis padding: an explicit x/ypad wins over the theme's tpar setting. This
  ## has to resolve here, before flip_datapoints() swaps the pair -- resolving
  ## it later would leave a tpar default attached to the axis rather than to
  ## the variable, so the two paths would disagree under `flip`.
  if (is.null(xpad)) xpad = get_tpar("xpad")
  if (is.null(ypad)) ypad = get_tpar("ypad")
  assert_numeric(xpad, len = 1, lower = 0, null.ok = TRUE, name = "xpad")
  assert_numeric(ypad, len = 1, lower = 0, null.ok = TRUE, name = "ypad")
  ## Keep the user's pad distinct from the one lim_args() may synthesize for a
  ## categorical axis: that one is a fraction of the whole plot's range, so a
  ## free panel spanning fewer categories has to work its own out instead.
  ## Captured here rather than in lim_args() because that never runs on the
  ## `add = TRUE` path; flip_datapoints() swaps the pair alongside x/ypad.
  xpad_user = xpad
  ypad_user = ypad

  env2env(
    environment(),
    settings,
    c("axes", "xaxt", "yaxt", "frame.plot", "xaxr", "yaxr", "xpad", "ypad",
      "xpad_user", "ypad_user", "las")
  )
}
