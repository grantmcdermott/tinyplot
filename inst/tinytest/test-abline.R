source("helpers.R")
using("tinysnapshot")

expect_error(tinyplot(type = type_hline(h = 10)), pattern = "data points")
expect_error(tinyplot(type = type_vline(v = 10)), pattern = "data points")
expect_error(tinyplot(type = type_abline(a = 0, b = 1)), pattern = "data points")


f = function() {
  plt(mpg ~ hp | factor(cyl), facet = ~ factor(cyl), data = mtcars)
  plt_add(type = type_hline(h = 12), col = "pink", lty = 3, lwd = 3)
}
expect_snapshot_plot(f, label = "hline")


f = function() {
  tinyplot(
    mpg ~ hp | factor(cyl),
    facet = ~ factor(cyl),
    data = mtcars,
    col = c("black", "green", "orange"))
  tinyplot_add(
    lty = 3, lwd = 3, col = c("black", "green", "orange"),
    type = type_vline(v = c(100, 150, 200)))
}
expect_snapshot_plot(f, label = "vline_vector")


## TODO: uncomment this when ready to test. Probably after the tinyplot_add
## refactor to save in an environment instead of global option
# f = function() {
#   mod = lm(mpg ~ hp, data = mtcars)
#   y = mtcars$mpg
#   yhat = predict(mod)
#   tinyplot(y, yhat, type = type_abline(a = 0, b = 1), xlim = c(0, 40), ylim = c(0, 40))
#   tinyplot_add(type = type_abline(a = 0, b = 1), xlim = c(0, 40), ylim = c(0, 40))
# }
# expect_snapshot_plot(f, label = "abline")
