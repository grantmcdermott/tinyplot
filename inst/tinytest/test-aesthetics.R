source("helpers.R")
using("tinysnapshot")
if (Sys.info()["sysname"] != "Linux") exit_file("Linux snapshots")

op = par(no.readonly = TRUE)

#
# "Manual" versions first
#

# type="b" + aesthetics
f = function() plot2(Temp ~ Day | Month, data = airquality, type = "b")
expect_snapshot_plot(f, label = "aesthetics_type_b")

f = function() plot2(Temp ~ Day | Month, data = airquality, type = "b", lty = 1:5)
expect_snapshot_plot(f, label = "aesthetics_type_b_lty")

f = function() plot2(Temp ~ Day | Month, data = airquality, type = "b", col = "black", pch = 1:5)
expect_snapshot_plot(f, label = "aesthetics_type_b_col_pch")

# check that non-point types don't generate points accidentally
f = function() plot2(Temp ~ Day | Month, data = airquality, type = "l")
expect_snapshot_plot(f, label = "aesthetics_type_l")

#
# Next, convenience versions using the "by" keyword
# 

f = function() plot2(Temp ~ Day | Month, data = airquality, type = "b", pch = "by", lty = "by")
expect_snapshot_plot(f, label = "aesthetics_by")

# check non-point types don't generate points in legend accidentally
f = function() plot2(Temp ~ Day | Month, data = airquality, type = "l", pch = "by", lty = "by")
expect_snapshot_plot(f, label = "aesthetics_by_type_l")

# no color version
f = function() plot2(Temp ~ Day | Month, data = airquality, type = "b", pch = "by", lty = "by", col = "black")
expect_snapshot_plot(f, label = "aesthetics_by_nocol")

# inherit global par
f = function() {
  par(pch = 16)
  plot2(Temp ~ Day | Month, data = airquality, type = "b", pch = "by", lty = "by")
}
expect_snapshot_plot(f, label = "aesthetics_by_par")

# as above but check recycling
f = function() {
  par(pch = 25, lty = 6)
  plot2(Temp ~ Day | Month, data = airquality, type = "b", pch = "by", lty = "by")
}
expect_snapshot_plot(f, label = "aesthetics_by_recycle")

# reset par
par(op)