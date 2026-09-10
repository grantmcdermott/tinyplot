source("helpers.R")
using("tinysnapshot")

mod = lm(mpg ~ hp + factor(cyl), mtcars)
coefs = data.frame(names(coef(mod)), coef(mod), confint(mod))
coefs = setNames(coefs, c("x", "y", "ymin", "ymax"))

fun = function() {
    with(
      coefs,
      tinyplot(
        pch = 17,
        x = 1:4,
        y = y,
        ymin = ymin,
        ymax = ymax,
        type = "pointrange"
      )
    )
}
expect_snapshot_plot(fun, label = "pointrange_triangle")

fun = function() {
    with(
        coefs,
        tinyplot(
          x = x,
          y = y,
          ymin = ymin,
          ymax = ymax,
          type = "errorbar"
        )
      )
}
expect_snapshot_plot(fun, label = "pointrange_errorbar")

# issues #511 & #516: adding layers to coefplot
fun = function() {
  tinyplot(
    y ~ x, ymin = ymin, ymax = ymax,
    data = coefs,
    type = "pointrange",
    theme = "basic"
  )
  tinyplot_add(type = "ribbon")
  tinyplot_add(type = "hline", lty = 2)
}
expect_snapshot_plot(fun, label = "pointrange_with_layers")

# test the reverse too (i.e., adding errorbars on a ribbon)
fun = function() {
  tinyplot(
    y ~ x, ymin = ymin, ymax = ymax,
    data = coefs,
    type = "ribbon",
    theme = "basic"
  )
  tinyplot_add(type = "errorbar")
}
expect_snapshot_plot(fun, label = "ribbon_with_errorbar")

fun = function() {
  tinyplot(
    y ~ x, ymin = ymin, ymax = ymax,
    data = coefs,
    type = "pointrange",
    theme = "classic",
    flip = TRUE
  )
  tinyplot_add(type = "ribbon")
  tinyplot_add(type = "vline", lty = 2)
}
expect_snapshot_plot(fun, label = "pointrange_with_layers_flipped")

# xord = NULL overrides the "asis" default, ordering the terms by their
# factor levels (alphabetical here) instead of their row order (#679)
fun = function() {
    with(
        coefs,
        tinyplot(
          x = x,
          y = y,
          ymin = ymin,
          ymax = ymax,
          type = type_pointrange(xord = NULL)
        )
      )
}
expect_snapshot_plot(fun, label = "pointrange_xlevels_null")

# `xord` defaults to "asis" here, so the ignored-argument warning must key on
# whether the user actually supplied it -- otherwise every numeric-x
# coefficient plot would warn on its own default
cf2 = data.frame(x = c(1, 2, 3), lo = c(0, 1, 2), hi = c(2, 3, 4))
expect_silent(
  tinyplot(x ~ x, data = cf2, ymin = lo, ymax = hi, type = type_pointrange())
)
expect_warning(
  tinyplot(x ~ x, data = cf2, ymin = lo, ymax = hi, type = type_pointrange(xord = "rev")),
  pattern = "only categorical"
)
