#' LM type
#' 
#' @inheritParams type_glm
#' @importFrom stats lm predict
#' @export
type_lm = function(se = TRUE, level = 0.95) {
    assert_flag(se)
    out = list(
        draw = draw_ribbon(),
        data = data_lm(se = se, level = level),
        name = if (isTRUE(se)) "ribbon" else "l"
    )
    class(out) = "tinyplot_type"
    return(out)
}


data_lm = function(se, level, ...) {
    fun = function(datapoints, ...) {
        dat = split(datapoints, list(datapoints$facet, datapoints$by))
        dat = lapply(dat, function(x) {
            if (nrow(x) == 0) return(x)
            if (nrow(x) < 3) {
                x$y = NA
                return(x)
            }
            fit = lm(y ~ x, data = x)
            nd = data.frame(x = seq(min(x$x, na.rm = TRUE), max(x$x, na.rm = TRUE), length.out = 100))
            nd$by = x$by[1]
            nd$facet = x$facet[1]
            if (se == TRUE) {
                p = predict(fit, newdata = nd, se.fit = TRUE)
                p = ci(p$fit, p$se.fit, conf.level = level, fit$df.residual)
                nd$y = p$estimate
                nd$ymax = p$conf.high
                nd$ymin = p$conf.low
            } else {
                nd$y = predict(fit, nd)
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

