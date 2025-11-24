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
    theme = "basic",
    flip = TRUE
  )
  tinyplot_add(type = "ribbon")
  tinyplot_add(type = "hline", lty = 2)
}
expect_snapshot_plot(fun, label = "pointrange_with_layers_flipped")
