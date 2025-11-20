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

# issue 511: adding hline to coefplot
fun = function() {
  tinyplot(
    y ~ x, ymin = ymin, ymax = ymax,
    data = coefs,
    type = "pointrange",
    theme = "basic"
  )
  tinyplot_add(type = "hline", lty = 2)
}
expect_snapshot_plot(fun, label = "pointrange_with_hline")

# Issue #406: dodge pointrange and errorbar
models = list(
    "Model A" = lm(mpg ~ wt + cyl, data = mtcars),
    "Model B" = lm(mpg ~ wt + hp + cyl, data = mtcars),
    "Model C" = lm(mpg ~ wt, data = mtcars)
)
results = lapply(names(models), function(m) {
    data.frame(
        model = m,
        term = names(coef(models[[m]])),
        estimate = coef(models[[m]]),
        setNames(data.frame(confint(models[[m]])), c("conf.low", "conf.high"))
    )
})
results = do.call(rbind, results)
fun = function() {
  tinyplot(estimate ~ term | model,
    ymin = conf.low, ymax = conf.high,
    flip = TRUE, data = results,
    type = type_pointrange(dodge = 0.2))
}
expect_snapshot_plot(fun, label = "pointrange_dodge_01")
