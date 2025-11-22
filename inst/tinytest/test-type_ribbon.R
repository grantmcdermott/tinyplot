source("helpers.R")
using("tinysnapshot")

mod = lm(mpg ~ wt, mtcars)
pred = predict(mod, interval = "confidence")

mtcars2 = cbind(mtcars, pred)


f = function() {
  with(
    mtcars2,
    tinyplot(
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
    tinyplot(
      x = wt,
      ymin = lwr, ymax = upr,
      type = "ribbon"
    )
  )
}
expect_snapshot_plot(f, label = "ribbon_no_y")

f = function() {
  with(
    mtcars2,
    tinyplot(
      x = wt, y = fit,
      type = "ribbon"
    )
  )
}
expect_snapshot_plot(f, label = "ribbon_no_yminymax")


f = function() {
  with(
    mtcars2,
    tinyplot(
      x = wt, y = fit,
      ymin = lwr, ymax = upr,
      type = type_ribbon(alpha = .5)
    )
  )
}
expect_snapshot_plot(f, label = "ribbon_alpha50")


# Dodged ribbon test
f = function() {
  dat = data.frame(
    x = rep(c("Before", "After"), each = 2),
    grp = rep(c("A", "B"), 2),
    y = c(10, 10.5, 15, 15.3),
    lwr = c(8, 8.5, 13, 13.3),
    upr = c(12, 12.5, 17, 17.3)
  )
  tinyplot(
    y ~ x | grp,
    data = dat,
    ymin = lwr, ymax = upr,
    type = type_ribbon(dodge = 0.1)
  )
}
expect_snapshot_plot(f, label = "ribbon_dodge")
