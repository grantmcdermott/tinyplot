#' Local polynomial regression plot type
#'
#' @description Type function for plotting a LOESS (LOcal regrESSion) fit.
#' Arguments are passed to \code{\link[stats]{loess}}.
#'
#' @inheritParams type_glm
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
    level = 0.95,
    weights = NULL) {
    out = list(
        draw = draw_ribbon(),
        data = data_loess(span = span, degree = degree, family = family, control = control, se = se, level = level, weights = weights),
        name = if (isTRUE(se)) "ribbon" else "l"
    )
    class(out) = "tinyplot_type"
    return(out)
}


data_loess = function(span, degree, family, control, se, level, weights = NULL, ...) {
    fun = function(settings, ...) {
        env2env(settings, environment(), "datapoints")
        # top-level `weights` (carried on datapoints via NSE) take precedence
        # over the constructor-level `weights` argument
        if (is.null(datapoints[["weights"]]) && !is.null(weights)) {
            datapoints[["weights"]] = weights
        }
        has_weights = !is.null(datapoints[["weights"]])
        if (has_weights) settings$weights_used = TRUE
        datapoints = split(datapoints, list(datapoints$facet, datapoints$by))
        datapoints = Filter(function(k) nrow(k) > 0, datapoints)
        datapoints = lapply(datapoints, function(dat) {
            # .w is NULL when no weights column is present, which loess() treats
            # the same as omitting the argument
            .w = dat[["weights"]]
            fit = loess(y ~ x, data = dat, span = span, degree = degree, family = family, control = control, weights = .w)
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
        
        # legend customizations - same as ribbon but add line through square
        settings$legend_args[["pch"]] = settings$legend_args[["pch"]] %||% 22
        settings$legend_args[["pt.cex"]] = settings$legend_args[["pt.cex"]] %||% 3.5
        settings$legend_args[["pt.lwd"]] = settings$legend_args[["pt.lwd"]] %||% 0
        settings$legend_args[["lty"]] = settings$legend_args[["lty"]] %||% par("lty")
        settings$legend_args[["y.intersp"]] = settings$legend_args[["y.intersp"]] %||% 1.25
        settings$legend_args[["seg.len"]] = settings$legend_args[["seg.len"]] %||% 1.25
        
        env2env(environment(), settings, "datapoints")
    }
    return(fun)
}
