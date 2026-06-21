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
#'
#' # Weighted fit example, either as a top-level argument (which supports
#' # non-standard evaluation in the formula method)...
#' s77 = as.data.frame(state.x77)
#' tinyplot(`Life Exp` ~ Income, data = s77, type = "lm", weights = Population)
#' # ...or directly via the type constructor (requires an evaluated vector)
#' tinyplot(`Life Exp` ~ Income, data = s77, type = type_lm(
#'   weights = states$Population))
#' @export
type_lm = function(se = TRUE, level = 0.95, weights = NULL) {
    assert_flag(se)
    out = list(
        draw = draw_ribbon(),
        data = data_lm(se = se, level = level, weights = weights),
        name = if (isTRUE(se)) "ribbon" else "l"
    )
    class(out) = "tinyplot_type"
    return(out)
}


data_lm = function(se, level, weights = NULL, ...) {
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
            # .w is NULL when no weights column is present, which lm() treats
            # the same as omitting the argument
            .w = x[["weights"]]
            fit = lm(y ~ x, data = x, weights = .w)
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
