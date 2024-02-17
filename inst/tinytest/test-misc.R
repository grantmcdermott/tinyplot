source("helpers.R")
using("tinysnapshot")
if (ON_CRAN) exit_file("CRAN")

f = function() {
  par(mfrow = c(1, 2))
  plot(Temp ~ Day, data = airquality, log = "x")
  tinyplot(Temp ~ Day, data = airquality, log = "x")
} 
expect_snapshot_plot(f, label = "arg_log_x")

f = function() {
  par(mfrow = c(1, 2))
  plot(Temp ~ Day, data = airquality, log = "y")
  tinyplot(Temp ~ Day, data = airquality, log = "y")
} 
expect_snapshot_plot(f, label = "arg_log_y")

f = function() {
  par(mfrow = c(1, 2))
  plot(Temp ~ Day, data = airquality, log = "xy")
  tinyplot(Temp ~ Day, data = airquality, log = "xy")
} 
expect_snapshot_plot(f, label = "arg_log_xy")

f = function() {
  par(mfrow = c(1, 2))
  plot(Temp ~ Day, data = airquality, log = "yx")
  tinyplot(Temp ~ Day, data = airquality, log = "yx")
} 
expect_snapshot_plot(f, label = "arg_log_yx")

## Sidestep test fails due to new (R 4.4.0) density grid value calculations.
## https://bugs.r-project.org/show_bug.cgi?id=18337
exit_if_not(getRversion()  <= "4.3.2")
## Note: Once 4.4.0 is released we can either generate some new plots or
## test with something like:
# f = function() tinyplot(..., old.coords=TRUE))

f = function() {
  par(mfrow = c(1, 1))
  pred = predict(lm(mpg~wt+factor(cyl),mtcars), interval = "confidence")
  m = cbind(mtcars, pred)
  with(
    m,
    tinyplot(wt, fit, ymin = lwr, ymax = upr, by = cyl, type = "ribbon", grid = TRUE)
  )
  with(
    m,
    tinyplot(wt, mpg, by = cyl, pch = 16, add = TRUE)
  )
}
expect_snapshot_plot(f, label = "addTRUE")