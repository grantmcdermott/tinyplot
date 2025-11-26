# Ensure added layers respect the x-axis order of the original plot layer
# (e.g., when adding lines or ribbons on top of errorbars)
align_layer = function(settings) {
  # Retrieve xlabs and plot/device metadata from original layer
  tinyplot_env = get(".tinyplot_env", envir = parent.env(environment()))
  xlabs_orig = tryCatch(get("xlabs_orig", envir = tinyplot_env), error = function(e) NULL)
  usr_orig = tryCatch(get("usr_orig", envir = tinyplot_env), error = function(e) NULL)
  dev_orig = tryCatch(get("dev_orig", envir = tinyplot_env), error = function(e) NULL)
  
  # Validate that we're adding to the same plot (not a stale xlabs from previous plot)
  if (is.null(usr_orig) || is.null(dev_orig) || dev_orig != dev.cur()) {
    return(invisible())
  }
  # Normalize current usr for comparison (accounting for flipped plots)
  usr_layer = if (isTRUE(settings$flip)) par("usr")[c(3,4,1,2)] else par("usr")
  if (!identical(usr_orig, usr_layer)) {
    return(invisible())
  }
  
  # xlabs of current layer
  xlabs_layer = settings[["xlabs"]]
  
  # Only adjust if original layer has named xlabs
  if (!is.null(names(xlabs_orig))) {
    if (is.factor(settings$datapoints[["x"]])) {
      # Case 1: relevel a factor (e.g., ribbon added to errorbars)
      settings$datapoints[["x"]] = tryCatch(
        factor(settings$datapoints[["x"]], levels = names(xlabs_orig)),
        error = function(e) {
          settings$datapoints[["x"]]
        }
      )
      settings$datapoints = settings$datapoints[order(settings$datapoints[["x"]]), ]
    } else if (!is.null(names(xlabs_layer))) {
      # Case 2: match implicit integer -> label mapping (e.g., lines added to errorbars)
      if (setequal(names(xlabs_layer), names(xlabs_orig))) {
        orig_order = xlabs_orig[names(xlabs_layer)[settings$datapoints[["x"]]]]
        x_layer = settings$datapoints[["x"]]
        if (is.null(settings$dodge)) {
          x_new = x_layer[orig_order] 
        } else {
          names(x_layer) = names(xlabs_layer)[round(x_layer)]
          x_new = x_layer + (xlabs_orig[names(round(x_layer))] - round(x_layer))
        }
        settings$datapoints[["x"]] = x_new
        # Adjust ancillary variables
        for (v in c("xmin", "xmax")) {
          if (identical(settings$datapoints[[v]], unname(x_layer))) {
            settings$datapoints[[v]] = x_new
          }
        }
        settings$datapoints = settings$datapoints[order(settings$datapoints[["x"]]), ]
        settings$datapoints[["rowid"]] = seq_len(nrow(settings$datapoints))
      }
    }
  }
}

