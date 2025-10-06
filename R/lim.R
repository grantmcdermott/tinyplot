# calculate limits of each plot

lim_args = function(settings) {
  list2env(settings, environment())

  # For cases where x/yaxb is provided and corresponding x/ylabs is not null...
  # We can subset these here to provide breaks
  if (!is.null(xaxb) && !is.null(xlabs)) {
    xlabs = xlabs[names(xlabs) %in% xaxb]
    xaxb = NULL # don't need this any more
  }
  if (!is.null(yaxb) && !is.null(ylabs)) {
    ylabs = ylabs[names(ylabs) %in% yaxb]
    yaxb = NULL # don't need this any more
  }

  if (is.null(xlim)) {
    xlim = range(c(
      datapoints[["x"]], datapoints[["xmin"]],
      datapoints[["xmax"]]), finite = TRUE)
  }
  if (is.null(ylim)) {
    ylim = range(c(
      datapoints[["y"]], datapoints[["ymin"]],
      datapoints[["ymax"]]), finite = TRUE)
  }

  if (identical(type, "boxplot")) {
    xlim = xlim + c(-0.5, 0.5)
  }

  if (null_xlim && !is.null(xaxb) && type != "spineplot") xlim = range(c(xlim, xaxb))
  if (null_ylim && !is.null(yaxb) && type != "spineplot") ylim = range(c(ylim, yaxb))

  update_settings(settings, xlim = xlim, ylim = ylim, xlabs = xlabs, ylabs = ylabs, xaxb = xaxb, yaxb = yaxb)
}
