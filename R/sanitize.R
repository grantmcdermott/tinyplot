sanitize_ribbon.alpha = function(ribbon.alpha) {
  assert_numeric(ribbon.alpha, len = 1, lower = 0, upper = 1, null.ok = TRUE)
  if (is.null(ribbon.alpha)) ribbon.alpha = .tpar[["ribbon.alpha"]]
  return(ribbon.alpha)
}



sanitize_type = function(type, x, y, dots) {
  if (inherits(type, "tinyplot_type")) {
    return(type)
  }

  types = c(
    "area", "boxplot", "density", "jitter", "ribbon", "pointrange", "hist",
    "histogram", "errorbar", "polygon", "polypath", "rect", "qq", "segments", "points",
    "p", "l", "o", "b", "c", "h", "j", "s", "S", "n", "loess", "spline", "lm", "glm",
    "spineplot", "function"
  )
  assert_choice(type, types, null.ok = TRUE)

  if (is.null(type)) {
    if (!is.null(x) && is.factor(x) && !is.factor(y)) {
      # enforce boxplot type for y ~ factor(x)
      type = type_boxplot
    } else if (is.factor(y)) {
      # enforce spineplot type for factor(y) ~ x
      type = type_spineplot
    } else {
      type = "p"
    }
  } else if (type %in% c("hist", "histogram")) {
    type = "histogram"
  } else if (type %in% c("j", "jitter")) {
    type = type_jitter
  }

  if (is.character(type)) type = switch(type,
    "points" = type_points,
    "segments" = type_segments,
    "area" = type_area,
    "rect" = type_rect,
    "polypath" = type_polypath,
    "polygon" = type_polygon,
    "pointrange" = type_pointrange,
    "errorbar" = type_errorbar,
    "boxplot" = type_boxplot,
    "ribbon" = type_ribbon,
    "histogram" = type_histogram,
    "spineplot" = type_spineplot,
    "qq" = type_qq,
    "j" = type_jitter,
    "jitter" = type_jitter,
    "loess" = type_loess,
    "spline" = type_spline,
    "glm" = type_glm,
    "lm" = type_lm,
    "function" = type_function,
    type # Default case
  )
  
  if (is.function(type)) {
    args = intersect(names(formals(type)), names(dots))
    args = if (length(args) >= 1L) dots[args] else list()
    type = do.call(type, args)
    type$dots = dots[setdiff(names(dots), names(args))]
  }
  
  if (inherits(type, "tinyplot_type")) return(type)

  out = list(draw = NULL, data = NULL, name = type)
  return(out)
}
