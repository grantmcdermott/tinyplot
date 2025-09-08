#' @rdname type_errorbar
#' @export
type_pointrange = function(dodge = 0, fixed.pos = FALSE) {
  assert_numeric(dodge, len = 1, lower = 0)
  assert_logical(fixed.pos)

  out = list(
    draw = draw_pointrange(),
    data = data_pointrange(dodge = dodge, fixed.pos = fixed.pos),
    name = "p"
  )
  class(out) = "tinyplot_type"
  return(out)
}


draw_pointrange = function() {
  fun = function(
    ix,
    iy,
    ixmin,
    iymin,
    ixmax,
    iymax,
    icol,
    ibg,
    ipch,
    ilwd,
    icex,
    ...
  ) {
    segments(
      x0 = ixmin,
      y0 = iymin,
      x1 = ixmax,
      y1 = iymax,
      col = icol,
      lwd = ilwd
    )
    draw_points()(
      ix = ix,
      iy = iy,
      icol = icol,
      ibg = ibg,
      ipch = ipch,
      ilwd = ilwd,
      icex = icex
    )
  }
  return(fun)
}


data_pointrange = function(dodge, fixed.pos) {
  fun = function(datapoints, xlabs, ...) {
    if (is.character(datapoints$x)) {
      datapoints$x = as.factor(datapoints$x)
    }
    if (is.factor(datapoints$x)) {
      ## original data (i.e., no new sorting by factor)
      xlvls = unique(datapoints$x)
      datapoints$x = factor(datapoints$x, levels = xlvls)
      xlabs = seq_along(xlvls)
      names(xlabs) = xlvls
      datapoints$x = as.integer(datapoints$x)
    }
    datapoints$xmin = datapoints$x
    datapoints$xmax = datapoints$x

    # dodge
    if (dodge != 0) {
      if (fixed.pos) {
        n = nlevels(datapoints$by)
        d = cumsum(rep(dodge, n))
        d = d - mean(d)
        x_adj = d[as.integer(datapoints$by)]
        datapoints$x = datapoints$x + x_adj
        datapoints$xmin = datapoints$xmin + x_adj
        datapoints$xmax = datapoints$xmax + x_adj
      } else {
        xuniq = unique(datapoints$x)
        for (i in seq_along(xuniq)) {
          idx = which(datapoints$x == xuniq[i])
          n = length(idx)
          d = cumsum(rep(dodge, n))
          d = d - mean(d)
          datapoints$x[idx] = datapoints$x[idx] + d
          datapoints$xmin[idx] = datapoints$xmin[idx] + d
          datapoints$xmax[idx] = datapoints$xmax[idx] + d
        }
      }
    }

    out = list(
      x = datapoints$x,
      xlabs = xlabs,
      datapoints = datapoints
    )

    return(out)
  }
  return(fun)
}
