boxplot_args <- function(datapoints) {
  dp = datapoints

  # Convert x to factor if it's not already
  dp$x = as.factor(dp$x)

  # Handle factor levels and maintain order
  xlvls = levels(dp$x)
  xlabs = seq_along(xlvls)
  names(xlabs) = xlvls
  dp$x = as.integer(dp$x)
  
  # Handle ordering based on by and facet variables
  null_by = length(unique(dp$by)) == 1
  null_facet = length(unique(dp$facet)) == 1

  if (null_by && null_facet) {
    xord = order(dp$x)
  } else if (null_facet) {
    xord = order(dp$by, dp$x)
  } else if (null_by) {
    xord = order(dp$facet, dp$x)
  } else {
    xord = order(dp$by, dp$facet, dp$x)
  }
  
  # Reorder x, y, ymin, and ymax based on the order determined
  dp = dp[xord,]
  
  # Return the result as a list called 'out'
  out <- list(
    x = dp$x,
    y = dp$y,
    ymin = dp$ymin,
    ymax = dp$ymax,
    xlabs = xlabs,
    datapoints = dp)

  if (length(unique(dp$by)) > 1) out[["by"]] = dp$by
  if (length(unique(dp$facet)) > 1) out[["facet"]] = dp$facet

  return(out)
}
