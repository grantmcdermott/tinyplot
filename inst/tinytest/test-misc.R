source("helpers.R")
using("tinysnapshot")

# empty plot(s)
f = function () {
  tinyplot(Sepal.Length ~ Petal.Length, data = iris, type = "n")
}
expect_snapshot_plot(f, label = "type_n")
# empty plot(s)
f = function () {
  tinyplot(Sepal.Length ~ Petal.Length | Species, data = iris, type = "n")
}
expect_snapshot_plot(f, label = "type_n_by")

# log axes
f = function() {
  op = tpar(mfrow = c(1, 2))
  plot(Temp ~ Day, data = airquality, log = "x")
  tinyplot(Temp ~ Day, data = airquality, log = "x")
  tpar(op)
} 
expect_snapshot_plot(f, label = "arg_log_x")

f = function() {
  op = tpar(mfrow = c(1, 2))
  plot(Temp ~ Day, data = airquality, log = "y")
  tinyplot(Temp ~ Day, data = airquality, log = "y")
  tpar(op)
} 
expect_snapshot_plot(f, label = "arg_log_y")

f = function() {
  op = tpar(mfrow = c(1, 2))
  plot(Temp ~ Day, data = airquality, log = "xy")
  tinyplot(Temp ~ Day, data = airquality, log = "xy")
  tpar(op)
} 
expect_snapshot_plot(f, label = "arg_log_xy")

f = function() {
  op = tpar(mfrow = c(1, 2))
  plot(Temp ~ Day, data = airquality, log = "yx")
  tinyplot(Temp ~ Day, data = airquality, log = "yx")
  tpar(op)
} 
expect_snapshot_plot(f, label = "arg_log_yx")

f = function() {
  op = tpar(mfrow = c(1, 1))
  m = transform(mtcars, cyl = factor(cyl))
  pred = predict(lm(mpg ~ wt + cyl, m), interval = "confidence")
  m = cbind(m, pred)
  with(
    m,
    tinyplot(wt, fit, ymin = lwr, ymax = upr, by = cyl, type = "ribbon", grid = TRUE)
  )
  with(
    m,
    tinyplot(wt, mpg, by = cyl, pch = 16, add = TRUE)
  )
  tpar(op)
}
expect_snapshot_plot(f, label = "addTRUE")