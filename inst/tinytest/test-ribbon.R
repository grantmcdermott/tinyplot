source("helpers.R")
using("tinysnapshot")

mod = lm(mpg ~ wt, mtcars)
pred = predict(mod, interval = "confidence")

mtcars2 = cbind(mtcars, pred)


f = function() {
  with(
    mtcars2,
    plot2(
      x = wt, y = fit,
      ymin = lwr, ymax = upr,
      type = "ribbon"
    )
  )
}
expect_snapshot_plot(f, label = "ribbon")

f = function() {
  with(
    mtcars2,
    plot2(
      x = wt,
      ymin = lwr, ymax = upr,
      type = "ribbon"
    )
  )
}
expect_snapshot_plot(f, label = "ribbon_no_y")