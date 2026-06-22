#' Generalized linear model plot type
#'
#' @description Type function for plotting a generalized model fit.
#' Arguments are passed to \code{\link[stats]{glm}}.
#'
#' @param se logical. If TRUE, confidence intervals are drawn.
#' @param weights an optional numeric vector of observation weights for the model
#'   fit, of the same length as the number of data points. Weights can also be
#'   supplied via the top-level `weights` argument of [`tinyplot`] (which is
#'   evaluated with non-standard evaluation in the formula method, and takes
#'   precedence if both are given).
#' @inheritParams stats::glm
#' @inheritParams stats::predict.glm
#' @inheritParams stats::confint
#' @importFrom stats glm predict
#' @examples
#' # "glm" type convenience string
#' tinyplot(am ~ mpg, data = mtcars, type = "glm")
#'
#' # Use `type_glm()` to pass extra arguments for customization
#' tinyplot(am ~ mpg, data = mtcars, type = type_glm(family = "binomial"))
#' @export
type_glm = function(family = "gaussian", se = TRUE, level = 0.95, type = "response", weights = NULL) {
    assert_flag(se)
    out = list(
        draw = draw_ribbon(),
        data = data_glm(family = family, se = se, level = level, type = type, weights = weights),
        name = if (isTRUE(se)) "ribbon" else "l"
    )
    class(out) = "tinyplot_type"
    return(out)
}


data_glm = function(family, se, level, type, weights = NULL, ...) {
    fun = function(settings, ...) {
        env2env(settings, environment(), "datapoints")
        # top-level `weights` (carried on datapoints via NSE) take precedence
        # over the constructor-level `weights` argument
        if (is.null(datapoints[["weights"]]) && !is.null(weights)) {
            datapoints[["weights"]] = weights
        }
        has_weights = !is.null(datapoints[["weights"]])
        if (has_weights) settings$weights_used = TRUE
        dat = split(datapoints, list(datapoints$facet, datapoints$by))
        dat = lapply(dat, function(x) {
            if (nrow(x) == 0) {
                return(x)
            }
            if (nrow(x) < 3) {
                x$y = NA
                return(x)
            }
            # .w is NULL when no weights column is present, which glm() treats
            # the same as omitting the argument
            .w = x[["weights"]]
            fit = glm(y ~ x, data = x, family = family, weights = .w)
            nd = data.frame(x = seq(min(x$x, na.rm = TRUE), max(x$x, na.rm = TRUE), length.out = 100))
            nd$by = x$by[1]
            nd$facet = x$facet[1]
            if (se == TRUE) {
                if (identical(type, "response")) {
                    p = predict(fit, newdata = nd, type = "link", se.fit = TRUE)
                    p = ci(p$fit, p$se.fit, conf.level = level, fit$df.residual, backtransform = stats::family(fit)$linkinv)
                    nd$y = p$estimate
                    nd$ymax = p$conf.high
                    nd$ymin = p$conf.low
                } else {
                    nd$y = predict(fit, newdata = nd, type = type)
                    nd = ci(nd$y, nd$se, level, fit$df.residual, backtransform = stats::family(fit)$linkinv)
                }
            } else {
                nd$y = predict(fit, nd, type = type)
            }
            nd
        })
        datapoints = do.call(rbind, dat)
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


#' Calculate confidence intervals
#' @importFrom stats qt
#' @keywords internal
ci = function(estimate, std.error, conf.level, df, backtransform = identity) {
    crit = qt(1 - (1 - conf.level) / 2, df)
    out = list(
        estimate = backtransform(estimate),
        conf.low = backtransform(estimate - crit * std.error),
        conf.high = backtransform(estimate + crit * std.error)
    )
    return(out)
}
