# Ensure added layers respect the x-axis order of the original plot layer
# (e.g., when adding lines or ribbons on top of errorbars)
align_layer = function(settings) {
  # Retrieve xlabs from current and original layers
  xlabs_layer = settings[["xlabs"]]
  xlabs_orig = tryCatch(
    get("xlabs", envir = get(".tinyplot_env", envir = parent.env(environment()))),
    error = function(e) NULL
  )
  
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

