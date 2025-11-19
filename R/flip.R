swap_elements = function(env, a, b) {
  # Swap two elements in an environment
  val_a = if (exists(a, envir = env, inherits = FALSE)) env[[a]] else NULL
  val_b = if (exists(b, envir = env, inherits = FALSE)) env[[b]] else NULL

  assign(a, val_b, envir = env)
  assign(b, val_a, envir = env)
}


swap_columns = function(dp, a, b) {
  va = dp[[a]]
  vb = dp[[b]]
  dp[[a]] = if (!is.null(vb)) vb else NULL
  dp[[b]] = if (!is.null(va)) va else NULL
  dp
}


flip_datapoints = function(settings) {
  env2env(settings, environment(), c("flip", "type", "datapoints", "log"))

  assert_flag(flip)
  if (isTRUE(flip)) {
    if (type == "boxplot") {
      # boxplot: let horizontal=TRUE do most work; only swap labels
      swap_elements(settings, "xlab", "ylab")
    } else {
      datapoints = swap_columns(datapoints, "xmin", "ymin")
      datapoints = swap_columns(datapoints, "xmax", "ymax")
      datapoints = swap_columns(datapoints, "x", "y")

      # Swap all the x/y settings in the environment
      swap_elements(settings, "x", "y")
      swap_elements(settings, "xaxb", "yaxb")
      swap_elements(settings, "xaxl", "yaxl")
      swap_elements(settings, "xaxs", "yaxs")
      swap_elements(settings, "xaxt", "yaxt")
      swap_elements(settings, "xlab", "ylab")
      swap_elements(settings, "xlabs", "ylabs")
      swap_elements(settings, "xlim", "ylim")
      swap_elements(settings, "xmax", "ymax")
      swap_elements(settings, "xmin", "ymin")

      if (!is.null(log)) {
        log = chartr("xy", "yx", log)
        assign("log", log, envir = settings)
      }

      # Copy modified datapoints back
      assign("datapoints", datapoints, envir = settings)
    }
  }
}
