source("helpers.R")
using("tinysnapshot")

f = function() tinyplot(0:10, type = "c")
expect_snapshot_plot(f, label = "type_c")

f = function() tinyplot(0:10, type = "h")
expect_snapshot_plot(f, label = "type_h")

f = function() tinyplot(0:10, type = "s")
expect_snapshot_plot(f, label = "type_s")

f = function() tinyplot(0:10, type = "S")
expect_snapshot_plot(f, label = "type_cap_s")

f = function() tinyplot(Temp ~ Day | Month, airquality, type = "c")
expect_snapshot_plot(f, label = "type_c_group")

f = function() tinyplot(Temp ~ Day | Month, airquality, type = "h")
expect_snapshot_plot(f, label = "type_h_group")

f = function() tinyplot(Temp ~ Day | Month, airquality, type = "s")
expect_snapshot_plot(f, label = "type_s_group")

f = function() tinyplot(Temp ~ Day | Month, airquality, type = "S")
expect_snapshot_plot(f, label = "type_cap_s_group")
