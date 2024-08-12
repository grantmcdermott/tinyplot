ribbon_args <- function(datapoints, by, facet) {
  dp = datapoints

  # Convert x to factor if it's not already
  if (is.character(dp$x)) {
    dp$x = as.factor(dp$x)
  }

  if (is.factor(dp$x)) {
    xlvls = levels(dp$x)
    xlabs = seq_along(xlvls)
    names(xlabs) = xlvls
    dp$x = as.integer(dp$x)
  } else {
    xlabs = NULL
  }

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
    by = if (length(unique(dp$by)) == 1) by else dp$by, 
    facet = if (length(unique(dp$facet)) == 1) facet else dp$facet,
    xlabs = xlabs,
    datapoints = datapoints)
  return(out)
}
