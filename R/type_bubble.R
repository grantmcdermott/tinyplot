sanitize_bubble = function(settings) {
  list2env(settings[c("datapoints", "pch", "alpha", "bg", "cex", "bubble")], environment())
  if (bubble) {
    datapoints[["cex"]] = cex
    bubble_pch = if (!is.null(pch) && length(pch)==1) pch else par("pch")
    bubble_alpha = if (!is.null(alpha)) alpha else 1
    bubble_bg_alpha = if (!is.null(bg) && length(bg)==1 && is.numeric(bg) && bg > 0 && bg <=1) bg else 1
  }
  settings[c("datapoints", "bubble_pch", "bubble_alpha", "bubble_bg_alpha")] =
    list(datapoints = datapoints,
         bubble_pch = if (bubble) bubble_pch else NULL,
         bubble_alpha = if (bubble) bubble_alpha else NULL,
         bubble_bg_alpha = if (bubble) bubble_bg_alpha else NULL)
  settings
}
