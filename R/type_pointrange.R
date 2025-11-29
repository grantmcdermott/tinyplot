#' @rdname type_errorbar
#' @export
type_pointrange = function(dodge = 0, fixed.dodge = FALSE) {
  out = list(
    draw = draw_pointrange(),
    data = data_pointrange(dodge = dodge, fixed.dodge = fixed.dodge),
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
      ...) {
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


data_pointrange = function(dodge, fixed.dodge) {
  fun = function(settings, ...) {
    env2env(settings, environment(), c("datapoints", "xlabs", "cex"))

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
      datapoints = dodge_positions(datapoints, dodge, fixed.dodge)
    }

    x = datapoints$x
    
    # legend customizations
    settings$legend_args[["pt.cex"]] = settings$legend_args[["pt.cex"]] %||% (cex %||% par("cex"))
    
    env2env(environment(), settings, c(
      "x",
      "xlabs",
      "datapoints"
    ))
  }
  return(fun)
}
