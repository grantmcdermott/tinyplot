sanitize_datapoints = function(settings) {
  # potentially useful variables
  env2env(
    settings,
    environment(),
    c(
      "x", "xmin", "xmax", "xaxt",
      "y", "ymin", "ymax", "ygroup",
      "facet", "null_by", "by", "type",
      "weights", "labels"
    )
  )

  ## drop degenerate dimensions from array inputs, e.g. a 1-row or 1-column
  ## matrix is just a vector (#548)
  drop_dims = function(z) {
    if (length(dim(z)) > 1L && sum(dim(z) > 1L) <= 1L) drop(z) else z
  }
  x = drop_dims(x); xmin = drop_dims(xmin); xmax = drop_dims(xmax)
  y = drop_dims(y); ymin = drop_dims(ymin); ymax = drop_dims(ymax)

  ## coerce character and logical variables to factors
  ## (aside: we won't risk converting x and y logicals to factors b/c it can
  ##  mess up types that rely on predict underneath the hood, e.g type_lm)
  if (!is.null(x) && is.character(x)) x = factor(x)
  if (!is.null(y) && is.character(y)) y = factor(y)
  if (!null_by && is.logical(by)) by = factor(by)

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
    y = y, ymin = ymin, ymax = ymax, ygroup = ygroup,
    weights = weights, labels = labels
  )
  datapoints = Filter(function(z) length(z) > 0, datapoints)
  datapoints = data.frame(datapoints)
  if (nrow(datapoints) > 0) {
    datapoints[["rowid"]] = seq_len(nrow(datapoints))
    datapoints[["facet"]] = if (!is.null(facet)) facet else ""
    datapoints[["by"]] = if (!null_by) by else ""
  }

  # potentially modified variables
  env2env(
    environment(),
    settings,
    c("x", "xmin", "xmax", "y", "ymin", "ymax", "xaxt", "datapoints")
  )
}
