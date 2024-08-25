type_jitter = function(datapoints) {
  x = datapoints$x
  y = datapoints$y
  if (is.character(x)) x = as.factor(x)
  if (is.character(y)) y = as.factor(y)
  if (is.factor(x)) {
    xlvls = levels(x)
    xlabs = seq_along(xlvls)
    names(xlabs) = xlvls
    x = as.integer(x)
  } else {
    xlabs = NULL
  }
  if (is.factor(y)) {
    ylvls = levels(y)
    ylabs = seq_along(ylvls)
    names(ylabs) = ylvls
    y = as.integer(y)
  } else {
    ylabs = NULL
  }
  x = jitter(x)
  y = jitter(y)

  datapoints$x = x
  datapoints$y = y

  out = list(
    datapoints = datapoints,
    x = x,
    y = y,
    xlabs = xlabs,
    ylabs = ylabs,
    type = "p"
  )
  return(out)
}
