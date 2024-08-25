type_histogram = function(x, by, facet, dots, ylab, col, bg, fill, ribbon.alpha, datapoints) {
  hbreaks = ifelse(!is.null(dots[["breaks"]]), dots[["breaks"]], "Sturges")
  hist_list = hist(x, breaks = hbreaks, plot = FALSE)
  
  if (is.null(ylab)) ylab = "Frequency"
  if (is.null(by) && is.null(palette)) {
    if (is.null(col)) col = par("fg")
    if (is.null(bg) && is.null(fill)) bg = "lightgray"
  } else {
    if (is.null(bg) && !is.null(fill)) bg = fill
    if (is.null(bg)) {
      bg = ribbon.alpha
    }
  }
  
  type = "rect"

  datapoints_breaks = hist(datapoints$x, breaks = hbreaks, plot = FALSE)
  datapoints = split(datapoints, list(datapoints$by, datapoints$facet))
  datapoints = Filter(function(k) nrow(k) > 0, datapoints)

  datapoints = lapply(datapoints, function(k) {
    h = hist(k$x, breaks = datapoints_breaks$breaks, plot = FALSE)
    out = data.frame(
      by = k$by[1], # already split
      facet = k$facet[1], # already split
      ymin = 0,
      ymax = h$counts,
      xmin = h$breaks[-1],
      xmax = h$mids + (h$mids - h$breaks[-1])
    )
    return(out)
  })
  datapoints = do.call(rbind, datapoints)

  out = list(
    x = c(datapoints$xmin, datapoints$xmax), 
    y = c(datapoints$ymin, datapoints$ymax),
    ymin = datapoints$ymin, 
    ymax = datapoints$ymax, 
    xmin = datapoints$xmin, 
    xmax = datapoints$xmax, 
    ylab = ylab, 
    col = col, 
    bg = bg, 
    type = type, 
    datapoints = datapoints,
    by = if (length(unique(datapoints$by)) == 1) by else datapoints$by, 
    facet = if (length(unique(datapoints$facet)) == 1) facet else datapoints$facet
  )
  return(out)
}
