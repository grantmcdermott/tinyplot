# calculate limits of each plot

lim_args = function(datapoints, xlim, ylim, type) {

  if (is.null(xlim))
    xlim = range(c(datapoints[["x"]], datapoints[["xmin"]],
                   datapoints[["xmax"]]), finite = TRUE)
  if (is.null(ylim))
    ylim = range(c(datapoints[["y"]], datapoints[["ymin"]],
                   datapoints[["ymax"]]), finite = TRUE)

  if (type == "boxplot") {
    xlim = xlim + c(-0.5, 0.5)
  }

  out = list(xlim = xlim, ylim = ylim)
  return(out)
}

