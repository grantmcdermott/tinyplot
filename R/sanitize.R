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
    "p", "l", "o", "b", "c", "h", "j", "s", "S", "n", 
    "density",
    "abline", "area", "boxplot", "errorbar", "function", "glm", "hist",
    "histogram", "hline", "j", "jitter", "lines", "lm", "loess", "pointrange",
    "points", "polygon", "polypath", "qq", "rect", "ribbon", "ridge", "rug",
    "segments", "spineplot", "spline", "text", "vline"
  )
  assert_choice(type, types, null.ok = TRUE)

  if (is.null(type)) {
    if (!is.null(x) && (is.factor(x) || is.character(x)) && !(is.factor(y) || is.character(y))) {
      # enforce boxplot type for y ~ factor(x)
      type = type_boxplot
    } else if (is.factor(y) || is.character(y)) {
      # enforce spineplot type for factor(y) ~ x
      type = type_spineplot
    } else {
      type = "p"
    }
  }

  if (is.character(type)) type = switch(type,
    "abline"     = type_abline,
    "area"       = type_area,
    "boxplot"    = type_boxplot,
    "density"    = type_density,
    "errorbar"   = type_errorbar,
    "function"   = type_function,
    "glm"        = type_glm,
    "hist"       = type_histogram,
    "histogram"  = type_histogram,
    "hline"      = type_hline,
    "j"          = type_jitter,
    "jitter"     = type_jitter,
    "lines"      = type_lines,
    "lm"         = type_lm,
    "loess"      = type_loess,
    "p"          = type_points,
    "pointrange" = type_pointrange,
    "points"     = type_points,
    "polygon"    = type_polygon,
    "polypath"   = type_polypath,
    "qq"         = type_qq,
    "rect"       = type_rect,
    "ribbon"     = type_ribbon,
    "ridge"      = type_ridge,
    "rug"        = type_rug,
    "segments"   = type_segments,
    "spineplot"  = type_spineplot,
    "spline"     = type_spline,
    "text"       = type_text,
    "vline"      = type_vline,
    type           # default case
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
