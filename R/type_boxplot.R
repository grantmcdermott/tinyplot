type_boxplot = function(datapoints) {
  # Convert x to factor if it's not already
  datapoints$x = as.factor(datapoints$x)

  # Handle factor levels and maintain order
  xlvls = levels(datapoints$x)
  xlabs = seq_along(xlvls)
  names(xlabs) = xlvls
  datapoints$x = as.integer(datapoints$x)
  
  # Handle ordering based on by and facet variables
  null_by = length(unique(datapoints$by)) == 1
  null_facet = length(unique(datapoints$facet)) == 1

  if (null_by && null_facet) {
    xord = order(datapoints$x)
  } else if (null_facet) {
    xord = order(datapoints$by, datapoints$x)
  } else if (null_by) {
    xord = order(datapoints$facet, datapoints$x)
  } else {
    xord = order(datapoints$by, datapoints$facet, datapoints$x)
  }
  
  # Reorder x, y, ymin, and ymax based on the order determined
  datapoints = datapoints[xord,]
  
  # Return the result as a list called 'out'
  out = list(
    x = datapoints$x,
    y = datapoints$y,
    ymin = datapoints$ymin,
    ymax = datapoints$ymax,
    xlabs = xlabs,
    datapoints = datapoints)

  if (length(unique(datapoints$by)) > 1) out[["by"]] = datapoints$by
  if (length(unique(datapoints$facet)) > 1) out[["facet"]] = datapoints$facet

  return(out)
}
