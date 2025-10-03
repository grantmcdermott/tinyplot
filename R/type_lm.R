#' Linear model plot type
#'
#' @description Type function for plotting a linear model fit.
#' Arguments are passed to \code{\link[stats]{lm}}.
#'
#' @inheritParams type_glm
#' @importFrom stats lm predict
#' @examples
#' # "lm" type convenience string
#' tinyplot(Sepal.Width ~ Petal.Width, data = iris, type = "lm")
#'
#' # Grouped model fits (here: illustrating an example of Simpson's paradox)
#' tinyplot(Sepal.Width ~ Petal.Width | Species, data = iris, type = "lm")
#' tinyplot_add(type = "p")
#'
#' # Use `type_lm()` to pass extra arguments for customization
#' tinyplot(Sepal.Width ~ Petal.Width, data = iris, type = type_lm(level = 0.8))
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
    fun = function(settings, ...) {
        list2env(settings[c("datapoints")], environment())
        dat = split(datapoints, list(datapoints$facet, datapoints$by))
        dat = lapply(dat, function(x) {
            if (nrow(x) == 0) {
                return(x)
            }
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
                nd$y = predict(fit, newdata = nd)
            }
            nd
        })
        datapoints = do.call(rbind, dat)
        datapoints = datapoints[order(datapoints$facet, datapoints$by, datapoints$x), ]
        out = list(datapoints = datapoints)
        out = modify_list(settings, out)
        return(out)
    }
    return(fun)
}
