#' Local polynomial regression plot type
#' 
#' @description Type function for plotting a LOESS (LOcal regrESSion) fit.
#' Arguments are passed to \code{\link[stats]{loess}}.
#' 
#' @inheritParams stats::loess
#' @param se logical. If `TRUE` (the default), confidence intervals are drawn.
#' @param level the confidence level required if `se = TRUE`. Default is 0.95.
#' @param locfit local. If `TRUE` the LOCFIT library is used to estimate the 
#' local polynomial regression which is much faster. Note that `span` is reused
#' for locfits `nn` option.
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
    level = 0.95,
    locfit = FALSE
    ) {
    if (locfit == TRUE) {
      pkg = "locfit"
      if (!requireNamespace(pkg, quietly = TRUE)) {
        msg = sprintf("Package '%s' is required by type_locfit(). Install with install.packages('%s').",
                      pkg, pkg)
        warning(msg, call. = FALSE)
        stop(msg, call. = FALSE)
      }
    }
    out = list(
        draw = draw_ribbon(),
        data = data_loess(span = span, degree = degree, family = family, control = control, se = se, level = level, locfit = locfit),
        name = if (isTRUE(se)) "ribbon" else "l"
    )
    class(out) = "tinyplot_type"
    return(out)
}


data_loess = function(span, degree, family, control, se, level, locfit, ...) {
    fun = function(datapoints, ...) {
        datapoints = split(datapoints, list(datapoints$facet, datapoints$by))
        datapoints = Filter(function(k) nrow(k) > 0, datapoints)
        datapoints = lapply(datapoints, function(dat) {
            if (locfit == FALSE) {
              fit = loess(y ~ x, data = dat, span = span, degree = degree, family = family, control = control)
            }
            else {
              fit = locfit::locfit(y ~ locfit::lp(x, nn = span, deg = degree), data = dat, family = family)  
            }
            if (se == TRUE) {
              if (locfit == FALSE) {
                p = predict(fit, newdata = dat, se = TRUE)
              }
              else {
                p = predict(fit, newdata = dat, se.fit = TRUE)
                p$df = Inf
              }
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

