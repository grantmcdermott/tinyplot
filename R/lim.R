# calculate limits of each plot

lim_args = function(settings) {
  env2env(
    settings,
    environment(),
    c(
      "xaxb", "xlabs", "xlim", "null_xlim", 
      "yaxb", "ylabs", "ylim", "null_ylim",
      "datapoints", "type"
    )
  )

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
    xlim = range(as.numeric(c(
      datapoints[["x"]], datapoints[["xmin"]],
      datapoints[["xmax"]])), finite = TRUE)
  }
  if (is.null(ylim)) {
    ylim = range(as.numeric(c(
      datapoints[["y"]], datapoints[["ymin"]],
      datapoints[["ymax"]])), finite = TRUE)
  }

  if (identical(type, "boxplot")) {
    xlim = xlim + c(-0.5, 0.5)
  }

  if (null_xlim && !is.null(xaxb) && type != "spineplot") xlim = range(c(xlim, xaxb))
  if (null_ylim && !is.null(yaxb) && type != "spineplot") ylim = range(c(ylim, yaxb))

  # update settings
  env2env(
    environment(),
    settings,
    c("xlim", "ylim", "xlabs", "ylabs", "xaxb", "yaxb")
  )
}

# translated from C_plot_window function in r-source/src/library/graphics/src/plot.c
calc_lim <- function(
  xlim,
  ylim,
  asp,
  xin = grconvertX(1, "npc", "inches"),
  yin = grconvertY(1, "npc", "inches")
) {
  if (!is.null(asp) && !is.na(asp) && asp > 0) {
    xmin = xlim[1]; xmax = xlim[2]; ymin = ylim[1]; ymax = ylim[2];
    xdelta = abs(xmax - xmin) / asp
    ydelta = abs(ymax - ymin)
    if(xdelta == 0.0 && ydelta == 0.0) {
      # Not from R source: actual zero used here
      return(list(xlim = xlim, ylim = ylim))
    }
    scale = min(c(xin / xdelta, yin / ydelta))
    xadd = .5 * (xin / scale - xdelta) * asp
    yadd = .5 * (yin / scale - ydelta)
    if (xmax < xmin) xadd = xadd * -1
    if (ymax < ymin) yadd = yadd * -1
    return(list(
      xlim = c(xmin - xadd, xmax + xadd),
      ylim = c(ymin - yadd, ymax + yadd)
    ))
  }
  list(xlim = xlim, ylim = ylim)
}
