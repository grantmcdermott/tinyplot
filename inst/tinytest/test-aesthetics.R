source("helpers.R")
using("tinysnapshot")
if (Sys.info()["sysname"] != "Linux") exit_file("Linux snapshots")

# type="b" + aesthetics
f = function() plot2(Temp ~ Day | Month, data = airquality, type = "b")
expect_snapshot_plot(f, label = "aesthetics_type_b")

f = function() plot2(Temp ~ Day | Month, data = airquality, type = "b", lty = 1:5)
expect_snapshot_plot(f, label = "aesthetics_type_b_lty")

f = function() plot2(Temp ~ Day | Month, data = airquality, type = "b", col = 1, pch = 1:5)
expect_snapshot_plot(f, label = "aesthetics_type_b_col_pch")

# check that non-point types don't generate points accidentally
f = function() plot2(Temp ~ Day | Month, data = airquality, type = "l")
expect_snapshot_plot(f, label = "aesthetics_type_l")