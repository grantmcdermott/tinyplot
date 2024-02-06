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