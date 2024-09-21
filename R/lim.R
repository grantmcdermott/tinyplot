# calculate limits of each plot

lim_args = function(datapoints, xlim, ylim, type) {

  if (all(c("x", "y") %in% names(datapoints))) {
    xy = xy.coords(x = datapoints$x, y = datapoints$y)
    if (is.null(xlim)) xlim = range(xy$x[is.finite(xy$x)])
    if (is.null(ylim)) ylim = range(xy$y[is.finite(xy$y)])
  }

  if (is.null(xlim))
    xlim = range(c(xlim, datapoints[["xmin"]], datapoints[["xmax"]]), finite = TRUE)
  if (is.null(ylim))
    ylim = range(c(ylim, datapoints[["ymin"]], datapoints[["ymax"]]), finite = TRUE)

  if (type == "boxplot") {
    xlim = xlim + c(-0.5, 0.5)
  }

  out = list(xlim = xlim, ylim = ylim)
  return(out)
}

