# calculate limits of each plot

lim_args = function(datapoints, xlim, ylim, type) {
  
  xlim_user = ylim_user = TRUE
  if (is.null(xlim)) {
    xlim_user = FALSE
    xlim = range(c(datapoints[["x"]], datapoints[["xmin"]],
                   datapoints[["xmax"]]), finite = TRUE)
  }
  if (is.null(ylim)) {
    ylim_user = FALSE
    ylim = range(c(datapoints[["y"]], datapoints[["ymin"]],
                   datapoints[["ymax"]]), finite = TRUE)
  }

  if (identical(type, "boxplot")) {
    xlim = xlim + c(-0.5, 0.5)
  }

  out = list(xlim = xlim, ylim = ylim, xlim_user = xlim_user, ylim_user = ylim_user)
  return(out)
}

