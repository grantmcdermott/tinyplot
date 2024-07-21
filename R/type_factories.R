assert_dependency = function(library_name) {
  flag = requireNamespace(library_name, quietly = TRUE)
  if (isFALSE(flag)) {
    stop(sprintf("Please install the `%s` package.", library_name), .call = FALSE)
  }
  return(invisible(TRUE))
}


ci = function(estimate, std.error, conf.level, df, backtransform = identity) {
  crit = qt(1 - (1-conf.level)/2, df)
  out = list(
    estimate = backtransform(estimate),
    conf.low = backtransform(estimate - crit * std.error),
    conf.high = backtransform(estimate + crit * std.error)
  )
  return(out)
}


#' Type factory: Spline
#'
#' Call this function in the `type` argument of `tinyplot()` to display a 
#' spline-smoothed version of the relationship between `y` and `x`.
#'
#' @inheritParams tinyplot
#' @inheritParams mgcv::s
#' @param conf.level FALSE or a numeric value between 0 and 1 determining the size of confidence intervals to display as ribbons.
#' @details
#'
#' @examples
#' plt(Sepal.Length ~ Petal.Length, type = type_spline(), data = iris)
#' @export
type_spline = function(x, y, conf.level = 0.95, k = -1, fx = FALSE, bs = "tp", ...) {
    se = if (isFALSE(conf.level)) FALSE else TRUE
    conf.level = if (isFALSE(conf.level)) 0.95 else conf.level
    assert_dependency("mgcv")
    fun = function(x, y, ...) {
        if (missing(x) || missing(y)) {
            if (isTRUE(se)) {
                return("ribbon")
            } else {
                return("l")
            }
        }
        dat = data.frame(x, y)
        fit = mgcv::gam(y ~ s(x, k = k, fx = fx, bs = bs), data = dat)
        nd = data.frame(x = seq(min(x), max(x), length.out = 100))
        p <- stats::predict(fit, newdata = nd, se.fit = TRUE)
        p <- ci(
            estimate = p$fit,
            std.error = p$se.fit,
            conf.level = conf.level, 
            df = fit$df.residual)
        out = list(x = nd$x,
            y = p$estimate,
            ymin = p$conf.low,
            ymax = p$conf.high)
        return(out)
    }
    return(fun)
}


#' Type factory: LOESS
#'
#' Call this function in the `type` argument of `tinyplot()` to display a 
#' LOESS-smoothed version of the relationship between `y` and `x`.
#'
#' @inheritParams tinyplot
#' @inheritParams type_spline
#' @details
#' First, we fit a model using the [stats::loess]. The predicted values of `y` are then obtained by calling [stats::predict].
#'
#' @examples
#' plt(Sepal.Length ~ Petal.Length, type = type_loess(), data = iris)
#' @export
type_loess = function(x, y, ...) {
    fun = function(x, y, ...) {
        if (missing(x) || missing(y)) return("l")
        dat = data.frame(x, y)
        fit = stats::loess(y ~ x, data = dat)
        nd = data.frame(x = seq(min(x), max(x), length.out = 100))
        y = stats::predict(fit, newdata = nd)
        out = list(y = y, x = nd$x)
        return(out)
    }
    return(fun)
}


#' Type factory: GLM
#'
#' Call this function in the `type` argument of `tinyplot()` to display a 
#' GLM-fitted version of the relationship between `y` and `x`.
#'
#' @inheritParams tinyplot
#' @inheritParams type_spline
#' @inheritParams stats::glm
#' @examples
#' plt(vs ~ mpg, type = type_glm(family = binomial), data = mtcars)
#' @export
type_glm = function(x, y, family = stats::gaussian(), conf.level = 0.95) {
    se = if (isFALSE(conf.level)) FALSE else TRUE
    conf.level = if (isFALSE(conf.level)) 0.95 else conf.level
    fun = function(x, y, ...) {
        if (missing(x) || missing(y)) {
            if (isTRUE(se)) {
                return("ribbon")
            } else {
                return("l")
            }
        }
        dat = data.frame(x, y)
        fit = stats::glm(y ~ x, family = family, data = dat)
        nd = data.frame(x = seq(min(x), max(x), length.out = 100))
        p <- stats::predict(fit, newdata = nd, se.fit = TRUE)
        p <- ci(
            estimate = p$fit,
            std.error = p$se.fit,
            conf.level = conf.level, 
            df = fit$df.residual,
            backtransform = fit$family$linkinv)
        out = list(x = nd$x,
            y = p$estimate,
            ymin = p$conf.low,
            ymax = p$conf.high)
        return(out)
    }
    return(fun)
}


#' Type factory: LM
#'
#' Call this function in the `type` argument of `tinyplot()` to display a 
#' LM-fitted version of the relationship between `y` and `x`.
#'
#' @inheritParams tinyplot
#' @inheritParams type_spline
#' @details
#'
#' @examples
#' plt(hp ~ mpg, type = type_lm(), data = mtcars)
#' @export
type_lm = function(x, y, conf.level = 0.95) {
    se = if (isFALSE(conf.level)) FALSE else TRUE
    conf.level = if (isFALSE(conf.level)) 0.95 else conf.level
    fun = function(x, y, ...) {
        if (missing(x) || missing(y)) {
            if (isTRUE(se)) {
                return("ribbon")
            } else {
                return("l")
            }
        }
        dat = data.frame(x, y)
        fit = stats::lm(y ~ x, data = dat)
        nd = data.frame(x = seq(min(x), max(x), length.out = 100))
        p <- stats::predict(fit, newdata = nd, se.fit = TRUE)
        p <- ci(
            estimate = p$fit,
            std.error = p$se.fit,
            conf.level = conf.level, 
            df = fit$df.residual)
        out = list(x = nd$x,
            y = p$estimate,
            ymin = p$conf.low,
            ymax = p$conf.high)
        return(out)
    }
    return(fun)
}
