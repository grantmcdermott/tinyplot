histogram_args = function(x, by, facet, dots, ylab, col, bg, fill, ribbon.alpha, datapoints) {
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

  dp = datapoints
  dp_breaks = hist(dp$x, breaks = hbreaks, plot = FALSE)
  dp = split(dp, list(datapoints$by, datapoints$facet))
  dp = Filter(function(k) nrow(k) > 0, dp)

  dp = lapply(dp, function(k) {
    h = hist(k$x, breaks = dp_breaks$breaks, plot = FALSE)
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
  dp = do.call(rbind, dp)

  out = list(
    x = c(dp$xmin, dp$xmax), 
    y = c(dp$ymin, dp$ymax),
    ymin = dp$ymin, 
    ymax = dp$ymax, 
    xmin = dp$xmin, 
    xmax = dp$xmax, 
    ylab = ylab, 
    col = col, 
    bg = bg, 
    type = type, 
    by = if (length(unique(dp$by)) == 1) by else dp$by, 
    facet = if (length(unique(dp$facet)) == 1) facet else dp$facet
  )
  return(out)
}
