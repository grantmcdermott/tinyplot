source("helpers.R")
using("tinysnapshot")

f = function() {
  par(mfrow = c(1, 2))
  plot(Temp ~ Day, data = airquality, log = "x")
  plot2(Temp ~ Day, data = airquality, log = "x")
} 
expect_snapshot_plot(f, label = "arg_log_x")

f = function() {
  par(mfrow = c(1, 2))
  plot(Temp ~ Day, data = airquality, log = "y")
  plot2(Temp ~ Day, data = airquality, log = "y")
} 
expect_snapshot_plot(f, label = "arg_log_y")

f = function() {
  par(mfrow = c(1, 2))
  plot(Temp ~ Day, data = airquality, log = "xy")
  plot2(Temp ~ Day, data = airquality, log = "xy")
} 
expect_snapshot_plot(f, label = "arg_log_xy")

f = function() {
  par(mfrow = c(1, 2))
  plot(Temp ~ Day, data = airquality, log = "yx")
  plot2(Temp ~ Day, data = airquality, log = "yx")
} 
expect_snapshot_plot(f, label = "arg_log_yx")