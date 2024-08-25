type_pointrange = function(datapoints, xlabs) {
 if (is.character(datapoints$x)) datapoints$x = as.factor(datapoints$x)
 if (is.factor(datapoints$x)) {
     ## original data (i.e., no new sorting by factor)
     xlvls = unique(datapoints$x)
     datapoints$x = factor(datapoints$x, levels = xlvls)
     xlabs = seq_along(xlvls)
     names(xlabs) = xlvls
     datapoints$x = as.integer(datapoints$x)
 }
  out = list(
    x = datapoints$x,
    xlabs = xlabs,
    datapoints = datapoints)

  return(out)
}