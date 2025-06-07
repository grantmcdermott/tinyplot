source("helpers.R")
using("tinysnapshot")

expect_error(tinyplot(type = type_hline(h = 10)), pattern = "data points")
expect_error(tinyplot(type = type_vline(v = 10)), pattern = "data points")
expect_error(tinyplot(type = type_abline(a = 0, b = 1)), pattern = "data points")


f = function() {
  plt(mpg ~ hp | cyl, facet = "by", data = mtcars)
  plt_add(type = type_hline(h = 12), col = "pink", lty = 3, lwd = 3)
}
expect_snapshot_plot(f, label = "hline")


f = function() {
  tinyplot(
    mpg ~ hp | cyl, facet = "by", data = mtcars,
    col = c("black", "green", "orange"))
  tinyplot_add(
    lty = 3, lwd = 3, col = c("black", "green", "orange"),
    type = type_vline(v = c(100, 150, 200)))
}
expect_snapshot_plot(f, label = "vline_vector")

# check recycling logic across different cases (by and/or facet combos)
# https://github.com/grantmcdermott/tinyplot/pull/422

f = function() {
  tinyplot(mpg ~ hp, facet = ~cyl, data = mtcars)
  tinyplot_add(type = type_hline(20))
  tinyplot_add(type = type_vline(200))
}
expect_snapshot_plot(f, label = "hline_recyle_facet")

f = function() {
  tinyplot(mpg ~ hp | wt, facet = ~cyl, data = mtcars)
  tinyplot_add(type = type_hline(20))
}
expect_snapshot_plot(f, label = "hline_recyle_facet_bycont")

f = function() {
  tinyplot(
    mpg ~ hp | factor(cyl), data = mtcars,
    type = type_hline(1:3 * 10), lwd = c(10, 5, 1)
  )
}
expect_snapshot_plot(f, label = "hline_recyle_by")

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
