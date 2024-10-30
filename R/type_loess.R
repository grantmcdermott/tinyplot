#' Loess type
#' 
#' @description Type function for plotting a LOESS (LOcal regrESSion) fit.
#' Arguments are passed to \code{\link[stats]{loess}}.
#' 
#' @inheritParams stats::loess
#' @param se logical. If `TRUE` (the default), confidence intervals are drawn.
#' @param level the confidence level required if `se = TRUE`. Default is 0.95.
#' @importFrom stats loess loess.control predict
#' @examples
#' # "loess" type convenience string
#' tinyplot(dist ~ speed, data = cars, type = "loess")
#' 
#' # Use `type_loess()` to pass extra arguments for customization
#' tinyplot(dist ~ speed, data = cars, type = type_loess(span = 0.5, degree = 1))
#' @export
type_loess = function(
    span = 0.75,
    degree = 2,
    family = "gaussian",
    control = loess.control(),
    se = TRUE,
    level = 0.95
    ) {
    out = list(
        draw = draw_ribbon(),
        data = data_loess(span = span, degree = degree, family = family, control = control, se = se, level = level),
        name = if (isTRUE(se)) "ribbon" else "l"
    )
    class(out) = "tinyplot_type"
    return(out)
}


data_loess = function(span, degree, family, control, se, level, ...) {
    fun = function(datapoints, ...) {
        datapoints = split(datapoints, list(datapoints$facet, datapoints$by))
        datapoints = Filter(function(k) nrow(k) > 0, datapoints)
        datapoints = lapply(datapoints, function(dat) {
            fit = loess(y ~ x, data = dat, span = span, degree = degree, family = family, control = control)
            if (se == TRUE) {
                p = predict(fit, newdata = dat, se = TRUE)
                p = ci(p$fit, p$se.fit, conf.level = level, p$df)
                dat$y = p$estimate
                dat$ymax = p$conf.high
                dat$ymin = p$conf.low
            } else {
                dat$y = predict(fit, dat)
            }
            dat
        })
        datapoints = do.call(rbind, datapoints)
        datapoints = datapoints[order(datapoints$facet, datapoints$by, datapoints$x), ]
        out = list(datapoints = datapoints)
        return(out)
    }
    return(fun)
}

