#' Error bar and pointrange plot types
#'
#' @description Type function(s) for producing error bar and pointrange plots.
#'
#' @param dodge Numeric value (>= 0) for dodging of overlapping points. Dodging
#'   is scaled relative to the x-axis tick locations (i.e., unique levels of
#'   `x`). For example, `dodge = 0.5` would place the outermost dodged points
#'   exactly midway the between axis ticks. Default value is 0 (no dodging).
#' @inheritParams graphics::arrows
#' @examples
#' mod = lm(mpg ~ wt * factor(am), mtcars)
#' coefs = data.frame(names(coef(mod)), coef(mod), confint(mod))
#' colnames(coefs) = c("term", "est", "lwr", "upr")
#'
#' op = tpar(pch = 19)
#'
#' # "errorbar" and "pointrange" type convenience strings
#' tinyplot(est ~ term, ymin = lwr, ymax = upr, data = coefs, type = "errorbar")
#' tinyplot(est ~ term, ymin = lwr, ymax = upr, data = coefs, type = "pointrange")
#'
#' # Use `type_errorbar()` to pass extra arguments for customization
#' tinyplot(est ~ term, ymin = lwr, ymax = upr, data = coefs, type = type_errorbar(length = 0.2))
#' # display three models side-by-side with dodging
#' models = list(
#'     "Model A" = lm(mpg ~ wt + cyl, data = mtcars),
#'     "Model B" = lm(mpg ~ wt + hp + cyl, data = mtcars),
#'     "Model C" = lm(mpg ~ wt, data = mtcars)
#' )
#'
#' results = lapply(names(models), function(m) {
#'     data.frame(
#'         model = m,
#'         term = names(coef(models[[m]])),
#'         estimate = coef(models[[m]]),
#'         setNames(data.frame(confint(models[[m]])), c("conf.low", "conf.high"))
#'     )
#' })
#' results = do.call(rbind, results)
#'
#' tinyplot(estimate ~ term | model,
#'     ymin = conf.low, ymax = conf.high,
#'     flip = TRUE, data = results,
#'     type = type_pointrange(dodge = 0.2))
#'
#' tpar(op)
#'
#' @export
type_errorbar = function(length = 0.05, dodge = 0) {
    out = list(
        draw = draw_errorbar(length = length),
        data = data_pointrange(dodge = dodge),
        name = "p"
    )
    class(out) = "tinyplot_type"
    return(out)
}


draw_errorbar = function(length = 0.05) {
    fun = function(
        ix,
        iy,
        ixmin,
        iymin,
        ixmax,
        iymax,
        icol,
        ibg,
        ipch,
        ilwd,
        icex,
        ...
    ) {
        arrows(
            x0 = ixmin,
            y0 = iymin,
            x1 = ixmax,
            y1 = iymax,
            col = icol,
            lwd = ilwd,
            length = length,
            angle = 90,
            code = 3
        )
        draw_points()(
            ix = ix,
            iy = iy,
            icol = icol,
            ibg = ibg,
            ipch = ipch,
            ilwd = ilwd,
            icex = icex
        )
    }
    return(fun)
}
