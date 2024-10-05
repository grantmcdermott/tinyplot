type_pointrange = function() {
  out <- list(
    draw = draw_pointrange(),
    data = data_pointrange(),
    name = "p"
  )
  class(out) <- "tinyplot_type"
  return(out)
}



draw_pointrange <- function() {
    fun = function(ix, iy, ixmin, iymin, ixmax, iymax, icol, ibg, ipch, ilwd, cex, ...) {
        segments(
            x0 = ixmin,
            y0 = iymin,
            x1 = ixmax,
            y1 = iymax,
            col = icol,
            lwd = ilwd
        )
        draw_points()(ix = ix, iy = iy, icol = icol, ibg = ibg, ipch = ipch, ilwd = ilwd, cex = cex)
    }
    return(fun)
}


data_pointrange = function() {
    fun = function(datapoints, xlabs, ...) {
        if (is.character(datapoints$x)) datapoints$x = as.factor(datapoints$x)
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
        out = list(
            x = datapoints$x,
            xlabs = xlabs,
            datapoints = datapoints)

        return(out)
    }
    return(fun)
}
