# calculate limits of each plot

lim_args = function(
  datapoints,
  xlim, ylim,
  xlabs, ylabs, 
  xaxb = NULL, yaxb = NULL,
  null_xlim = FALSE, null_ylim = FALSE,
  type) {

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

  out = list(xlim = xlim, ylim = ylim, xlabs = xlabs, ylabs = ylabs, xaxb = xaxb, yaxb = yaxb)
  return(out)
}
