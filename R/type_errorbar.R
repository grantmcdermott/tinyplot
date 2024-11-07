#' Error bar and pointrange plot types
#' 
#' @description Type function(s) for producing error bar and pointrange plots.
#' 
#' @inheritParams graphics::arrows
#' @examples
#' mod = lm(Sepal.Length ~ 0 + Sepal.Width * Species, iris)
#' mod = lm(mpg ~ wt * factor(am), mtcars)
#' coefs = data.frame(names(coef(mod)), coef(mod), confint(mod))
#' colnames(coefs) = c("term", "est", "lwr", "upr")
#' 
#' op = tpar(pch = 19)
#'  
#' # "errorbar" and "pointrange" type convenience strings
#' with(
#'   coefs,
#'   tinyplot(x = term, y = est, ymin = lwr, ymax = upr, type = "errorbar")
#' )
#' with(
#'   coefs,
#'   tinyplot(x = term, y = est, ymin = lwr, ymax = upr, type = "pointrange")
#' )
#' 
#' # Use `type_errorbar()` to pass extra arguments for customization
#' with(
#'   coefs,
#'   tinyplot(x = term, y = est, ymin = lwr, ymax = upr, type = type_errorbar(length = 0.2))
#' )
#' 
#' tpar(op)
#' 
#' @export
type_errorbar = function(length = 0.05) {
  out = list(
    draw = draw_errorbar(length = length),
    data = data_pointrange(),
    name = "p"
  )
  class(out) = "tinyplot_type"
  return(out)
}


draw_errorbar = function(length = 0.05) {
    fun = function(ix, iy, ixmin, iymin, ixmax, iymax, icol, ibg, ipch, ilwd, cex, ...) {
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
        draw_points()(ix = ix, iy = iy, icol = icol, ibg = ibg, ipch = ipch, ilwd = ilwd, cex = cex)
    }
    return(fun)
}


