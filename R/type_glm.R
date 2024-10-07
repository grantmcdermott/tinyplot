#' GLM type
#' 
#' @param se logical. If TRUE, confidence intervals are drawn.
#' @inheritParams stats::glm
#' @inheritParams stats::predict.glm
#' @inheritParams stats::confint
#' @importFrom stats glm predict family
#' @export
type_glm = function(family = "gaussian", se = TRUE, level = 0.95, type = "response") {
    assert_flag(se)
    out = list(
        draw = draw_ribbon(),
        data = data_glm(family = family, se = se, level = level, type = type),
        name = if (isTRUE(se)) "ribbon" else "l"
    )
    class(out) = "tinyplot_type"
    return(out)
}


data_glm = function(family, se, level, type, ...) {
    fun = function(datapoints, ...) {
        dat = split(datapoints, list(datapoints$facet, datapoints$by))
        dat = lapply(dat, function(x) {
            fit = glm(y ~ x, data = x, family = family)
            nd = data.frame(x = seq(min(x$x), max(x$x), length.out = 100))
            nd$by = x$by[1]
            nd$facet = x$facet[1]
            if (se == TRUE) {
                if (identical(type, "response")) {
                    p = predict(fit, newdata = nd, type = "link", se.fit = TRUE)
                    p = ci(p$fit, p$se.fit, conf.level = level, fit$df.residual, backtransform = family(fit)$linkinv)
                    nd$y = p$estimate
                    nd$ymax = p$conf.high
                    nd$ymin = p$conf.low

                } else {
                    nd$y = predict(fit, newdata = nd, type = type)
                    nd = ci(nd$y, nd$se, level, fit$df.residual, backtransform = family(fit)$linkinv)
                }
            } else {
                nd$y = predict(fit, nd, type = type)
            }
            nd
        })
        datapoints = do.call(rbind, dat)
        datapoints = datapoints[order(datapoints$facet, datapoints$by, datapoints$x), ]
        out = list(datapoints = datapoints)
        return(out)
    }
    return(fun)
}


ci = function(estimate, std.error, conf.level, df, backtransform = identity) {
    crit = qt(1 - (1 - conf.level) / 2, df)
    out = list(
        estimate = backtransform(estimate),
        conf.low = backtransform(estimate - crit * std.error),
        conf.high = backtransform(estimate + crit * std.error)
    )
    return(out)
}
