#' Spline plot type
#' 
#' @description Type function for plotting a cubic (or Hermite) spline interpolation.
#' Arguments are passed to \code{\link[stats]{spline}}; see this latter function
#' for default argument values.
#' 
#' @inheritParams stats::spline
#' @inherit stats::spline details
#' @importFrom stats spline
#' @examples
#' # "spline" type convenience string
#' tinyplot(dist ~ speed, data = cars, type = "spline")
#' 
#' # Use `type_spline()` to pass extra arguments for customization
#' tinyplot(dist ~ speed, data = cars, type = type_spline(method = "natural", n = 25),
#'     add = TRUE, lty = 2)
#' @export
type_spline = function(
        n = NULL,
        method = "fmm",
        xmin = NULL,
        xmax = NULL,
        xout = NULL,
        ties = mean
    ) {
    out = list(
        draw = draw_lines(),
        data = data_spline(method = method, ties = ties, n = n, xmin = xmin, xmax = xmax, xout = xout),
        name = "l"
    )
    class(out) = "tinyplot_type"
    return(out)
}


data_spline = function(n, method, xmin, xmax, xout, ties, ...) {
    fun = function(datapoints, ...) {
        datapoints = split(datapoints, list(datapoints$facet, datapoints$by), drop = TRUE)
        datapoints = lapply(datapoints, function(dat) {
            if (is.null(n)) n = 3*length(dat$x)
            if (is.null(xmax)) xmax = max(dat$x)
            if (is.null(xmin)) xmin = min(dat$x)
            if (is.null(xout)) {
                fit = spline(x = dat$x, y = dat$y, n = n, method = method, xmin = xmin, xmax = xmax, ties = ties)
            } else {
                fit = spline(x = dat$x, y = dat$y, n = n, method = method, xmin = xmin, xmax = xmax, xout = xout, ties = ties)
            }
            fit = as.data.frame(fit)
            fit$facet = dat$facet[1]
            fit$by = dat$by[1]
            fit
            return(fit)
        })
        datapoints = do.call(rbind, datapoints)
        out = list(datapoints = datapoints)
        return(out)
    }
    return(fun)
}

