swap_elements = function(lst, a, b) {
  if (any(!c(a, b) %in% names(lst))) {
    out = lst
  } else if (all(c(a, b) %in% names(lst))) {
    out = do.call(update_settings, c(list(lst), stats::setNames(lst[c(b, a)], c(a, b))))
  } else if (a %in% names(lst)) {
    out = do.call(update_settings, c(list(lst), stats::setNames(list(NULL, lst[[a]]), c(a, b))))
  } else if (b %in% names(lst)) {
    out = do.call(update_settings, c(list(lst), stats::setNames(list(NULL, lst[[b]]), c(b, a))))
  }
  out
}


swap_columns = function(dp, a, b) {
  va = dp[[a]]
  vb = dp[[b]]
  dp[[a]] = if (!is.null(vb)) vb else NULL
  dp[[b]] = if (!is.null(va)) va else NULL
  dp
}


flip_datapoints = function(settings) {
  flip = settings$flip
  assert_flag(flip)
  if (isTRUE(flip)) {
    if (settings$type == "boxplot") {
      # boxplot: let horizontal=TRUE do most work; only swap labels
      settings = swap_elements(settings, "xlab", "ylab")
    } else {
      datapoints = swap_columns(settings$datapoints, "xmin", "ymin")
      datapoints = swap_columns(datapoints, "xmax", "ymax")
      settings$datapoints = swap_columns(datapoints, "x", "y")
      settings = swap_elements(settings, "x", "y")
      settings = swap_elements(settings, "xaxb", "yaxb")
      settings = swap_elements(settings, "xaxl", "yaxl")
      settings = swap_elements(settings, "xaxs", "yaxs")
      settings = swap_elements(settings, "xaxt", "yaxt")
      settings = swap_elements(settings, "xlab", "ylab")
      settings = swap_elements(settings, "xlabs", "ylabs")
      settings = swap_elements(settings, "xlim", "ylim")
      settings = swap_elements(settings, "xmax", "ymax")
      settings = swap_elements(settings, "xmin", "ymin")
      if (!is.null(settings$log)) settings$log = chartr("xy", "yx", settings$log)
    }
  }
  return(settings)
}

