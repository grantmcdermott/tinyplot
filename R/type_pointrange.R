type_pointrange = function(datapoints, xlabs) {
 dp = datapoints
 if (is.character(dp$x)) dp$x = as.factor(dp$x)
 if (is.factor(dp$x)) {
     ## original data (i.e., no new sorting by factor)
     xlvls = unique(dp$x)
     dp$x = factor(dp$x, levels = xlvls)
     xlabs = seq_along(xlvls)
     names(xlabs) = xlvls
     dp$x = as.integer(dp$x)
 }
  out = list(
    x = dp$x,
    xlabs = xlabs,
    datapoints = dp)

  return(out)
}