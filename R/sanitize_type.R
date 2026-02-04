sanitize_type = function(settings) {
  env2env(settings, environment(), c("type", "dots", "x", "y"))

  if (inherits(type, "tinyplot_type")) {
    settings$type = type$name
    settings$type_draw = type$draw
    settings$type_data = type$data
    return(invisible(NULL))
  }

  known_types = c(
    "p", "l", "o", "b", "c", "h", "j", "s", "S", "n",
    "abline",
    "area",
    "bar", "barplot",
    "box", "boxplot",
    "density",
    "errorbar",
    "function",
    "glm",
    "hist", "histogram",
    "hline",
    "j", "jitter",
    "lines",
    "lm",
    "loess",
    "pointrange",
    "points",
    "polygon", "polypath",
    "qq",
    "rect",
    "ribbon",
    "ridge",
    "rug",
    "segments",
    "spine", "spineplot",
    "spline",
    "summary",
    "text",
    "violin",
    "vline"
  )
  assert_choice(type, known_types, null.ok = TRUE)

  if (is.null(type)) {
    if (is.null(x) && !(is.factor(y) || is.character(y))) {
      # enforce histogram type for y ~ 1
      settings$x = y
      settings$y = NULL
      type = type_hist
    } else if (is.null(x) && (is.factor(y) || is.character(y))) {
      # enforce barplot type for factor(y) ~ 1
      settings$x = y
      settings$y = NULL
      type = type_barplot
    } else if ((is.factor(x) || is.character(x)) && is.null(y)) {
      # enforce barplot type for ~ factor(y)
      type = type_barplot
    } else if (!is.null(x) && (is.factor(x) || is.character(x)) && !(is.factor(y) || is.character(y))) {
      # enforce boxplot type for y ~ factor(x)
      type = type_boxplot
    } else if (!is.null(x) && (is.factor(y) || is.character(y))) {
      # enforce spineplot type for factor(y) ~ x
      type = type_spineplot
    } else {
      type = "p"
    }
  }

  if (is.character(type)) {
    type = switch(type,
      "abline"     = type_abline,
      "area"       = type_area,
      "bar"        = type_barplot,
      "barplot"    = type_barplot,
      "box"        = type_boxplot,
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
      "l"          = type_lines,
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
      "spine"      = type_spineplot,
      "spineplot"  = type_spineplot,
      "spline"     = type_spline,
      "summary"    = type_summary,
      "text"       = type_text,
      "violin"     = type_violin,
      "vline"      = type_vline,
      type # default case
    )
  }
# browser()
  if (is.function(type)) {
    args = intersect(names(formals(type)), names(dots))
    args = if (length(args) >= 1L) dots[args] else list()
    type = do.call(type, args)
    type$dots = dots[setdiff(names(dots), names(args))]
  }

  if (inherits(type, "tinyplot_type")) {
    settings$type = type$name
    settings$type_draw = type$draw
    settings$type_data = type$data
  }
}
