assert_dependency <- function(library_name) {
  flag <- requireNamespace(library_name, quietly = TRUE)
  if (isFALSE(flag)) {
    stop(sprintf("Please install the `%s` package.", library_name), .call = FALSE)
  }
  return(invisible(TRUE))
}


#' Type factory: Spline
#'
#' Call this function in the `type` argument of `tinyplot()` to display a 
#' spline-smoothed version of the relationship between `y` and `x`.
#'
#' @inheritParams tinyplot
#' @inheritParams mgcv::s
#' @details
#' First, we fit a model using the [mgcv::gam] and [mgcv:s] function. The predicted values of `y` are then obtained by calling [marginaleffects::predictions].
#'
#' @examples
#' plt(Sepal.Length ~ Petal.Length, type = type_spline(), data = iris)
#' @export
type_spline = function(x, y, se = TRUE, k = -1, fx = FALSE, bs = "tp", ...) {
    assert_dependency("mgcv")
    assert_dependency("marginaleffects")
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
        nd = data.frame(x = seq(min(x), max(x), length.out = 1000))
        p = marginaleffects::predictions(fit, newdata = nd)
        out = list(x = p$x,
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
#' @details
#' First, we fit a model using the [stats::loess]. The predicted values of `y` are then obtained by calling [stats::predict].
#'
#' @examples
#' plt(Sepal.Length ~ Petal.Length, type = type_spline(), data = iris)
#' @export
type_loess = function(x, y, ...) {
    fun = function(x, y, ...) {
        if (missing(x) || missing(y)) return("l")
        dat = data.frame(x, y)
        fit = stats::loess(y ~ x, data = dat)
        nd = data.frame(x = seq(min(x), max(x), length.out = 1000))
        y = predict(fit, newdata = nd)
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
#' @details
#' First, we fit a model using the [stats::glm]. The predicted values of `y` are then obtained by calling [marginaleffects::predictions].
#'
#' @examples
#' plt(vs ~ mpg, type = type_glm(family = binomial), data = mtcars)
#' @export
type_glm = function(x, y, family = gaussian(), se = TRUE) {
    assert_dependency("marginaleffects")
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
        nd = data.frame(x = seq(min(x), max(x), length.out = 1000))
        p = marginaleffects::predictions(fit, newdata = nd)
        out = list(x = p$x,
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
#' @details
#' First, we fit a model using the [stats::lm]. The predicted values of `y` are then obtained by calling [marginaleffects::predictions].
#'
#' @examples
#' plt(hp ~ mpg, type = type_lm(), data = mtcars)
#' @export
type_lm = function(x, y, se = TRUE) {
    assert_dependency("marginaleffects")
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
        nd = data.frame(x = seq(min(x), max(x), length.out = 1000))
        p = marginaleffects::predictions(fit, newdata = nd)
        out = list(x = p$x,
            y = p$estimate,
            ymin = p$conf.low,
            ymax = p$conf.high)
        return(out)
    }
    return(fun)
}
