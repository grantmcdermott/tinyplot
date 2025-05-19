# calculate limits of each plot

lim_args = function(
    datapoints,
    xlim, ylim,
    xaxb = NULL, yaxb = NULL,
    xlim_user = FALSE, ylim_user = FALSE,
    type
) {
  
  if (is.null(xlim)) {
    xlim = range(c(datapoints[["x"]], datapoints[["xmin"]],
                   datapoints[["xmax"]]), finite = TRUE)
  }
  if (is.null(ylim)) {
    ylim = range(c(datapoints[["y"]], datapoints[["ymin"]],
                   datapoints[["ymax"]]), finite = TRUE)
  }

  if (identical(type, "boxplot")) {
    xlim = xlim + c(-0.5, 0.5)
  }
  
  if (!xlim_user && !is.null(xaxb)) xlim = range(c(xlim, xaxb))
  if (!ylim_user && !is.null(yaxb)) ylim = range(c(ylim, yaxb))

  out = list(xlim = xlim, ylim = ylim)
  return(out)
}

