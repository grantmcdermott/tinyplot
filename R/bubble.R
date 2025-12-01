bubble = function(settings) {
  # Only process for points and text types
  if (!(settings$type %in% c("p", "text"))) return(invisible())
  
  cex = settings$cex
  
  # Only process if cex is a vector matching data length
  if (is.null(cex) || length(cex) != nrow(settings$datapoints)) return(invisible())
  
  clim = settings$clim %||% c(0.5, 2.5)
  
  bubble = TRUE
  
  ## Identify the pretty break points for our bubble labels
  bubble_labs = pretty(cex, n = 5)
  len_labs = length(bubble_labs)
  cex = rescale_num(sqrt(c(bubble_labs, cex)) / pi, to = clim)
  bubble_cex = cex[1:len_labs]
  cex = cex[(len_labs+1):length(cex)]
  
  # catch for cases where pretty breaks leads to smallest category of 0
  if (bubble_labs[1] == 0) {
    bubble_labs = bubble_labs[-1]
    bubble_cex = bubble_cex[-1]
  }
  names(bubble_cex) = format(bubble_labs)
  
  if (max(clim) > 2.5) {
    settings$legend_args[["x.intersp"]] = max(clim) / 2.5
    settings$legend_args[["y.intersp"]] = sapply(bubble_cex / 2.5, max, 1)
  }

  ## fixme: can't assign pt.cex here b/c of dual legend gotcha (don't want to
  ##   override the "normal" pt.cex too) 
  # legend_args[["pt.cex"]] = legend_args[["pt.cex"]] %||% (settings[["cex"]] %||% par("cex"))
  
  # Must update settings with bubble/bubble_cex/cex before calling sanitize_bubble
  env2env(environment(), settings, c("bubble", "bubble_cex", "cex"))
  
  sanitize_bubble(settings)
}

sanitize_bubble = function(settings) {
  env2env(settings, environment(), c("datapoints", "pch", "alpha", "bg", "cex", "bubble"))
  
  if (!bubble) return(invisible())
  
  datapoints[["cex"]] = cex
  bubble_pch = if (!is.null(pch) && length(pch)==1) pch else par("pch")
  bubble_alpha = if (!is.null(alpha)) alpha else 1
  bubble_bg_alpha = if (!is.null(bg) && length(bg)==1 && is.numeric(bg) && bg > 0 && bg <=1) bg else 1

  env2env(environment(), settings, c("datapoints", "bubble_pch", "bubble_alpha", "bubble_bg_alpha"))
}

