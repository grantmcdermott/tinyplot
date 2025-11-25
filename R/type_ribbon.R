#' Ribbon and area plot types
#'
#' @param alpha numeric value between 0 and 1 specifying the opacity of ribbon shading
#'   If no `alpha` value is provided, then will default to `tpar("ribbon.alpha")`
#'   (i.e., probably `0.2` unless this has been overridden by the user in their global
#'   settings.)
#' @inheritParams type_errorbar
#'
#' @description Type constructor functions for producing polygon ribbons, which
#' define a `y` interval (usually spanning from `ymin` to `ymax`) for each
#' `x` value. Area plots are a special case of ribbon plot where `ymin` is
#' set to 0 and `ymax` is set to `y`.
#' 
#' @section Dodging ribbon plots:
#' 
#' We support dodging for grouped ribbon plots, enabling similar functionality
#' to dodged errorbar and pointrange plots. However, it is strongly recommended
#' that dodging is only implemented for cases where the x-axis comprises a
#' limited number of discrete cases (e.g., coefficient or event-study plots).
#' See Examples.
#'
#' @examples
#' x = 1:100 / 10
#' y = sin(x)
#'
#' #
#' ## Ribbon plots
#'
#' # "ribbon" convenience string
#' tinyplot(x = x, ymin = y - 1, ymax = y + 1, type = "ribbon")

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
#' 
#' #
#' ## Dodged ribbon/area plots
#' 
#' # Dodged ribbon or area plots can be useful in cases where there is strong
#' # overlap across groups (and a limited number of discrete x-axis values).
#' 
#' dat = data.frame(
#'   x = rep(c("Before", "After"), each = 2),
#'   grp = rep(c("A", "B"), 2),
#'   y = c(10, 10.5, 15, 15.3),
#'   lwr = c(8, 8.5, 13, 13.3),
#'   upr = c(12, 12.5, 17, 17.3)
#' )
#' 
#' tinyplot(
#'   y ~ x | grp,
#'   data = dat,
#'   ymin = lwr, ymax = upr,
#'   type = type_ribbon(),
#'   main = "Overlappling ribbons"
#' )
#' 
#' tinyplot(
#'   y ~ x | grp,
#'   data = dat,
#'   ymin = lwr, ymax = upr,
#'   type = type_ribbon(dodge = 0.1),
#'   main = "Dodged ribbons"
#' )
#' 
#' @export
type_ribbon = function(alpha = NULL, dodge = 0, fixed.dodge = FALSE) {
    out = list(
        draw = draw_ribbon(),
        data = data_ribbon(ribbon.alpha = alpha, dodge = dodge, fixed.dodge = fixed.dodge),
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


data_ribbon = function(ribbon.alpha = NULL, dodge = 0, fixed.dodge = FALSE) {
    ribbon.alpha = sanitize_ribbon_alpha(ribbon.alpha)
    fun = function(settings, ...) {
        env2env(settings, environment(), c("datapoints", "xlabs", "null_by", "null_facet"))
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
        
        # dodge (auto-detects x, xmin, xmax columns)
        if (dodge != 0) {
            datapoints = dodge_positions(datapoints, dodge, fixed.dodge)
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
        datapoints = datapoints[xord, ]

        # Catch for missing ymin and ymax
        if (is.null(datapoints$ymin)) datapoints$ymin = datapoints$y
        if (is.null(datapoints$ymax)) datapoints$ymax = datapoints$y

        x = datapoints$x
        y = datapoints$y
        ymin = datapoints$ymin
        ymax = datapoints$ymax
        by = if (length(unique(datapoints$by)) > 1) datapoints$by else NULL
        facet = if (length(unique(datapoints$facet)) > 1) datapoints$facet else NULL

        # ribbon.alpha comes from parent scope, so assign it locally
        ribbon.alpha = ribbon.alpha

        vars_to_copy = c("x", "y", "ymin", "ymax", "xlabs", "datapoints", "ribbon.alpha")
        if (!is.null(by)) vars_to_copy = c(vars_to_copy, "by")
        if (!is.null(facet)) vars_to_copy = c(vars_to_copy, "facet")

        env2env(environment(), settings, vars_to_copy)
    }
    return(fun)
}
