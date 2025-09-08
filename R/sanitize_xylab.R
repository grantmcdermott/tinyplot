sanitize_xylab = function(
    x, xlab = NULL, x_dep = NULL, xmin_dep = NULL, xmax_dep = NULL,
    y, ylab = NULL, y_dep = NULL, ymin_dep = NULL, ymax_dep = NULL,
    type = NULL) {
  out_xlab = NULL
  out_ylab = NULL

  is_boxplot = type %in% c("boxplot")
  is_density = type %in% c("density")
  is_frequency = type %in% c("histogram", "barplot", "function")
  is_function = type %in% c("function")
  is_range = type %in% c("rect", "segments", "pointrange")
  is_ribbon = type %in% c("ribbon")
  is_index = !is_frequency && !is_ribbon && !is_density

  ##### xlab
  if (!is.null(xlab)) {
    out_xlab = xlab
  } else if (!is.null(xmin_dep) && !is.null(xmax_dep)) {
    out_xlab = sprintf("[%s, %s]", xmin_dep, xmax_dep)
  } else if (is_boxplot && is.null(y)) {
    out_xlab = ""
  } else if (is_index && is.null(y) && !is.null(x)) {
    out_xlab = "Index"
  } else {
    out_xlab = x_dep
  }

  ##### ylab
  if (!is.null(ylab)) {
    out_ylab = ylab
  } else if (is_frequency && is.null(y) && !is.null(x)) {
    out_ylab = "Frequency"
  } else if (is_density && is.null(y) && !is.null(x)) {
    out_ylab = "Density"
  } else if (is_ribbon) {
    if (!is.null(y_dep)) {
      out_ylab = y_dep
    } else if (!is.null(ymin_dep) && !is.null(ymax_dep)) {
      out_ylab = sprintf("[%s, %s]", ymin_dep, ymax_dep)
    }
  } else if ((is_range || is_ribbon) && !is.null(ymin_dep) && !is.null(ymax_dep)) {
    out_ylab = sprintf("[%s, %s]", ymin_dep, ymax_dep)
  } else if (!is.null(y_dep)) {
    out_ylab = y_dep
  } else if (is.null(y) && !is.null(x_dep)) {
    out_ylab = x_dep
  } else {
    out_ylab = NULL
  }

  out = list(xlab = out_xlab, ylab = out_ylab)
  return(out)
}
