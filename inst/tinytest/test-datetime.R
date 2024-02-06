source("helpers.R")
using("tinysnapshot")


x_dtt = strptime(20010101:20010110, format="%Y%m%d", tz = "UTC")
x_dt = as.Date(x_dtt, tz = "UTC")
y = 1:10

f = function() tinyplot(x_dtt, y, grid = TRUE)
expect_snapshot_plot(f, label = "date_time")

f = function() tinyplot(x_dt, y, grid = TRUE)
expect_snapshot_plot(f, label = "date")