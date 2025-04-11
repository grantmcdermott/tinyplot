#' Ribbon and area plot types
#' 
#' @param alpha numeric value between 0 and 1 specifying the opacity of ribbon shading
#'   If no `alpha` value is provided, then will default to `tpar("ribbon.alpha")` 
#'   (i.e., probably `0.2` unless this has been overridden by the user in their global 
#'   settings.)
#'
#' @description Type constructor functions for producing polygon ribbons, which 
#' define a `y` interval (usually spanning from `ymin` to `ymax`) for each
#' `x` value. Area plots are a special case of ribbon plot where `ymin` is
#' set to 0 and `ymax` is set to `y`.
#' 
#' @examples
#' x = 1:100/10
#' y = sin(x)
#' 
#' #
#' ## Ribbon plots
#' 
#' # "ribbon" convenience string
#' tinyplot(x = x, ymin = y-1, ymax = y+1, type = "ribbon")

#' # Same result with type_ribbon()
#' tinyplot(x = x, ymin = y-1, ymax = y+1, type = type_ribbon())
#' 
#' # y will be added as a line if it is specified
#' tinyplot(x = x, y = y, ymin = y-1, ymax = y+1, type = "ribbon")
#'
#' #
#' ## Area plots
#'   
#' # "area" type convenience string
#' tinyplot(x, y, type = "area")
#' 
#' # Same result with type_area()
#' tinyplot(x, y, type = type_area())
#' 
#' # Area plots are often used for time series charts
#' tinyplot(AirPassengers, type = "area")
#' @export
type_ribbon = function(alpha = NULL) {
  out = list(
    draw = draw_ribbon(),
    data = data_ribbon(ribbon.alpha = alpha),
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
        lin(ix = ix, iy = iy, icol = icol, ipch = ipch, ibg = ibg, ilty = ilty, ilwd = ilwd, type = "l")
    }
    return(fun)
}


data_ribbon = function(ribbon.alpha = NULL) {
    ribbon.alpha = sanitize_ribbon.alpha(ribbon.alpha)
    fun = function(datapoints, xlabs, null_by, null_facet, ...) {
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

        # Catch for missing ymin and ymax 
        if (is.null(datapoints$ymin)) datapoints$ymin = datapoints$y 
        if (is.null(datapoints$ymax)) datapoints$ymax = datapoints$y

        out = list(
            x = datapoints$x,
            y = datapoints$y,
            ymin = datapoints$ymin,
            ymax = datapoints$ymax,
            xlabs = xlabs,
            datapoints = datapoints,
            ribbon.alpha = ribbon.alpha)

        if (length(unique(datapoints$by)) > 1) out[["by"]] = datapoints$by
        if (length(unique(datapoints$facet)) > 1) out[["facet"]] = datapoints$facet

        return(out)
    }
    return(fun)
}
