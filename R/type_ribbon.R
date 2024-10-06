type_ribbon = function() {
  out = list(
    draw = draw_ribbon(),
    data = data_ribbon(),
    name = "ribbon"
  )
  class(out) = "tinyplot_type"
  return(out)
}


draw_ribbon = function() {
    fun = function(ix, iy, ixmin, ixmax, iymin, iymax, ibg, ilty, ilwd, icol, ipch, i, flip = FALSE, ...) {
        polyg = type_polygon()$draw
        lin = type_lines()$draw
        if (isFALSE(flip)) {
            polyg(ix = c(ix, rev(ix)), iy = c(iymin, rev(iymax)), icol = NA, ibg = ibg)
        } else {
            polyg(c(ixmin, rev(ixmax)), iy = c(iy, rev(iy)), icol = NA, ibg = ibg)
        }
        lin(ix = ix, iy = iy, icol = icol, ipch = ipch, ilty = ilty, ilwd = ilwd, type = "l")
    }
    return(fun)
}


data_ribbon = function() {
    fun = function(datapoints, xlabs, ...) {
        # Convert x to factor if it's not already
        if (is.character(datapoints$x)) {
            datapoints$x = as.factor(datapoints$x)
        }

        if (is.factor(datapoints$x)) {
            xlvls = levels(datapoints$x)
            xlabs = seq_along(xlvls)
            names(xlabs) = xlvls
            datapoints$x = as.integer(datapoints$x)
        } else {
            xlabs = NULL
        }

        # Handle ordering based on by and facet variables
        null_by = length(unique(datapoints$by)) == 1
        null_facet = length(unique(datapoints$facet)) == 1

        if (null_by && null_facet) {
            xord = order(datapoints$x)
        } else if (null_facet) {
            xord = order(datapoints$by, datapoints$x)
        } else if (null_by) {
            xord = order(datapoints$facet, datapoints$x)
        } else {
            xord = order(datapoints$by, datapoints$facet, datapoints$x)
        }

        # Reorder x, y, ymin, and ymax based on the order determined
        datapoints = datapoints[xord,]

        out = list(
            x = datapoints$x,
            y = datapoints$y,
            ymin = datapoints$ymin,
            ymax = datapoints$ymax,
            xlabs = xlabs,
            datapoints = datapoints)

        if (length(unique(datapoints$by)) > 1) out[["by"]] = datapoints$by
        if (length(unique(datapoints$facet)) > 1) out[["facet"]] = datapoints$facet

        return(out)
    }
    return(fun)
}
