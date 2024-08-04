# calculate limits of each plot

lim_args = function(...) {
  list2env(list(...), environment())

  xy = xy.coords(x = x, y = y)
  if (is.null(xlim)) xlim = range(xy$x[is.finite(xy$x)])
  if (is.null(ylim)) ylim = range(xy$y[is.finite(xy$y)])

  if (!is.null(xmin)) xlim[1] = min(c(xlim, xmin))
  if (!is.null(xmax)) xlim[2] = max(c(xlim, xmax))
  if (!is.null(ymin)) ylim[1] = min(c(ylim, ymin))
  if (!is.null(ymax)) ylim[2] = max(c(ylim, ymax))

  if (type == "boxplot") {
    xlim = xlim + c(-0.5, 0.5)
    if (is.null(by) && is.null(palette)) {
      if (is.null(col)) col = par("fg")
      if (is.null(bg) && is.null(fill)) bg = "lightgray"
    } else {
      fill = bg = "by"
    }
  }

  return(as.list(environment()))
}

