sanitize_xlab = function(xlab = NULL, x_dep = NULL, xmin_dep = NULL, xmax_dep = NULL, type = NULL, y = NULL) {
  out = NULL

  if (!is.null(xlab)) {
    out = xlab
  } else {
    out = x_dep
  }

  if (is.null(out)) {
    if (!is.null(xmin_dep) && !is.null(xmax_dep)) {
      out = sprintf("[%s, %s]", xmin_dep, xmax_dep)
    } else if (is.null(y)) {
      if (identical(type, "boxplot")) {
        out = ""
      } else if (!type %in% c("histogram", "barplot")) {
        out = "Index"
      }
    }
  }

  return(out)
}
