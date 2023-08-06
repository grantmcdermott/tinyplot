source("helpers.R")
using("tinysnapshot")
if (Sys.info()["sysname"] != "Linux") exit_file("Linux snapshots")

f = function() plot2(0:10, type = "c")
expect_snapshot_plot(f, label = "type_c")

f = function() plot2(0:10, type = "h")
expect_snapshot_plot(f, label = "type_h")

f = function() plot2(0:10, type = "s")
expect_snapshot_plot(f, label = "type_s")

f = function() plot2(0:10, type = "S")
expect_snapshot_plot(f, label = "type_cap_s")

f = function() plot2(Temp ~ Day | Month, airquality, type = "c")
expect_snapshot_plot(f, label = "type_c_group")

f = function() plot2(Temp ~ Day | Month, airquality, type = "h")
expect_snapshot_plot(f, label = "type_h_group")

f = function() plot2(Temp ~ Day | Month, airquality, type = "s")
expect_snapshot_plot(f, label = "type_s_group")

f = function() plot2(Temp ~ Day | Month, airquality, type = "S")
expect_snapshot_plot(f, label = "type_cap_s_group")

dev.off()