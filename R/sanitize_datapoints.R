sanitize_datapoints = function(settings) {
  # potentially useful variables
  env2env(settings, environment(), c("x", "xmin", "xmax", "xaxt", "y", "ymin", "ymax", "ygroup", "facet", "null_by", "by", "type"))

  ## coerce character variables to factors
  if (!is.null(x) && is.character(x)) x = factor(x)
  if (!is.null(y) && is.character(y)) y = factor(y)

  if (is.null(x)) {
    ## Special catch for rect and segment plots without a specified y-var
    if (type %in% c("rect", "segments")) {
      x = rep(NA, length(x))
    }
  }

  if (is.null(y)) {
    ## Special catch for area and interval plots without a specified y-var
    if (type %in% c("rect", "segments", "pointrange", "errorbar", "ribbon")) {
      y = rep(NA, length(x))
    } else if (type == "boxplot") {
      y = x
      x = rep.int("", length(y))
      xaxt = "a"
    } else if (!(type %in% c("histogram", "barplot", "density", "function"))) {
      y = x
      x = seq_along(x)
    }
  }

  datapoints = list(
    x = x, xmin = xmin, xmax = xmax,
    y = y, ymin = ymin, ymax = ymax, ygroup = ygroup
  )
  datapoints = Filter(function(z) length(z) > 0, datapoints)
  datapoints = data.frame(datapoints)
  if (nrow(datapoints) > 0) {
    datapoints[["rowid"]] = seq_len(nrow(datapoints))
    datapoints[["facet"]] = if (!is.null(facet)) facet else ""
    datapoints[["by"]] = if (!null_by) by else ""
  }

  # potentially modified variables
  env2env(environment(), settings, c("x", "y", "xaxt", "datapoints"))
}
