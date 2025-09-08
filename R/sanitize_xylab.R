sanitize_xylab <- function(
    x, xlab = NULL, x_dep = NULL, xmin_dep = NULL, xmax_dep = NULL,
    y, ylab = NULL, y_dep = NULL, ymin_dep = NULL, ymax_dep = NULL,
    type = NULL) {
  out_xlab = NULL
  out_ylab = NULL

  ##### xlab
  if (!is.null(xlab)) {
    out_xlab = xlab
  } else {
    out_xlab = x_dep
  }

  if (is.null(out_xlab)) {
    if (!is.null(xmin_dep) && !is.null(xmax_dep)) {
      out_xlab = sprintf("[%s, %s]", xmin_dep, xmax_dep)
    } else if (is.null(y)) {
      if (identical(type, "boxplot")) {
        out_xlab = ""
      } else if (!type %in% c("histogram", "barplot")) {
        out_xlab = "Index"
      }
    }
  }

  ##### ylab
  is_density = type %in% c("density")
  is_frequency = type %in% c("function", "histogram", "barplot")
  is_range = type %in% c("rect", "segments", "pointrange", "ribbon")
  is_ribbon = type %in% c("ribbon")
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
  } else if (is_range && !is.null(ymin_dep) && !is.null(ymax_dep)) {
    out_ylab = sprintf("[%s, %s]", ymin_dep, ymax_dep)
  } else if (!is.null(y_dep)) {
    out_ylab = y_dep
  } else if (is.null(y) && !is.null(out_xlab)) {
    out_ylab = out_xlab
  } else {
    out_ylab = NULL
  }

  out <- list(xlab = out_xlab, ylab = out_ylab)
  return(out)
}
