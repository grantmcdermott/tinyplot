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
    # The atomic branch of this condition covers a base layer that coerced a
    # numeric/character x to a factor itself (bars, ridges): its categories are
    # the *labels*, while the added layer still carries the raw values. Those
    # values are releveled below just like a factor would be, and the resulting
    # integer codes are the positions the base layer drew at.
    #
    # Both extra tests are load-bearing. Requiring the layer to have no named
    # xlabs of its own leaves Case 2 owning layers that already converted --
    # otherwise a base whose categories are literally "1", "2", "3" would have
    # the layer's integer *positions* misread as labels. Requiring every value
    # to match leaves a partial overlap alone, rather than silently turning the
    # unmatched rows into NA and dropping them from the plot.
    if (is.factor(settings$datapoints[["x"]]) ||
        (is.null(names(xlabs_layer)) &&
         all(as.character(settings$datapoints[["x"]]) %in% names(xlabs_orig)))) {
      # Case 1: relevel a factor (e.g., ribbon added to errorbars), or an
      # atomic x whose values name the original layer's categories
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
        # If mappings already agree and no dodge, no realignment needed
        if (identical(xlabs_layer, xlabs_orig) && is.null(settings$dodge)) return(invisible())
        x_layer = settings$datapoints[["x"]]
        if (is.null(settings$dodge)) {
          # Per-row lookup, not a permutation: the position each row's category
          # occupies in the original layer. Indexing `x_layer` by it instead
          # only coincided with the right answer when the layer's rows happened
          # to arrive in ascending order. (#679)
          x_new = unname(xlabs_orig[names(xlabs_layer)[x_layer]])
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
