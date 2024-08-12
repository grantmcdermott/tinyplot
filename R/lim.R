# calculate limits of each plot

lim_args = function(datapoints, xlim, ylim, palette, col, bg, fill, type) {

  if (all(c("x", "y") %in% names(datapoints))) {
    xy = xy.coords(x = datapoints$x, y = datapoints$y)
    if (is.null(xlim)) xlim = range(xy$x[is.finite(xy$x)])
    if (is.null(ylim)) ylim = range(xy$y[is.finite(xy$y)])
  }

  xlim = range(c(xlim, datapoints[["xmin"]], datapoints[["xmax"]]), finite = TRUE)
  ylim = range(c(ylim, datapoints[["ymin"]], datapoints[["ymax"]]), finite = TRUE)

  if (type == "boxplot") {
    xlim = xlim + c(-0.5, 0.5)
    if (length(unique(datapoints[["by"]])) == 1 && is.null(palette)) {
      if (is.null(col)) col = par("fg")
      if (is.null(bg) && is.null(fill)) bg = "lightgray"
    } else {
      fill = bg = "by"
    }
  }

  out = list(
    xlim = xlim,
    ylim = ylim,
    col = col,
    bg = bg,
    fill = fill)

  return(out)
}

