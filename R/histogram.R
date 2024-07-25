histogram_args = function(x, by, facet, facet_by, dots, ylab, col, bg, fill, ribbon.alpha) {
  hbreaks = ifelse(!is.null(dots[["breaks"]]), dots[["breaks"]], "Sturges")
  hist_list = hist(x, breaks = hbreaks, plot = FALSE)
  
  if (!is.null(by) || !is.null(facet)) {
    split_type = ifelse(is.null(facet) || isTRUE(facet_by), "byonly", ifelse(is.null(by), "facetonly", "byandfacet"))
    
    if (split_type == "byonly") {
      split_x = split(x, by)
    } else if (split_type == "facetonly") {
      split_x = split(x, facet)
    } else {
      split_x = split(x, interaction(by, facet, sep = "___"))
    }
    
    hist_split = lapply(seq_along(split_x), function(s) {
      h = hist(split_x[[s]], breaks = hist_list$breaks, plot = FALSE)
      h$breaks = h$breaks[-1]
      
      if (split_type %in% c("byonly", "byandfacet")) {
        h$by = rep(names(split_x)[[s]], length(h$breaks))
      } else {
        h$by = NULL
      }
      
      if (split_type %in% c("facetonly", "byandfacet")) {
        h$facet = rep(names(split_x)[[s]], length(h$breaks))
      } else if (isTRUE(facet_by)) {
        h$facet = h$by
      } else {
        h$facet = NULL
      }
      return(h)
    })
    
    hist_list = do.call(Map, c(c, hist_split))
    xmin = hist_list$breaks
    xmax = hist_list$mids + (hist_list$mids - hist_list$breaks)
    by = hist_list$by
    
    if (split_type == "byandfacet") by = sub("___.*$", "", by)
    facet = hist_list$facet
    
    if (split_type == "byandfacet") facet = sub(".*___", "", facet)
    if (!is.null(facet)) facet = as.factor(facet)
  } else {
    xmin = hist_list$breaks[-1]
    xmax = hist_list$mids + (hist_list$mids - hist_list$breaks[-1])
  }
  
  ymin = hist_list$counts
  
  # Optional: remove zero count cases
  if (!is.null(by) || !is.null(facet)) {
    hidx = which(ymin != 0)
    xmin = xmin[hidx]
    xmax = xmax[hidx]
    ymin = ymin[hidx]
    by = by[hidx]
    facet = facet[hidx]
  }
  
  ymax = rep(0, length(ymin))
  x = c(xmin, xmax)
  y = c(ymin, ymax)
  
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

  out = list(
    x = x, 
    y = y, 
    ymin = ymin, 
    ymax = ymax, 
    xmin = xmin, 
    xmax = xmax, 
    ylab = ylab, 
    col = col, 
    bg = bg, 
    type = type, 
    by = by, 
    facet = facet,
    hbreaks = hbreaks,
    hist_list = hist_list
  )
  return(out)
}
