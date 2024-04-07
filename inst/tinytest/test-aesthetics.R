source("helpers.R")
using("tinysnapshot")

aq = airquality
aq$Month = as.factor(aq$Month)

op = par(no.readonly = TRUE)

#
# "Manual" versions first
#

# type="b" + aesthetics
f = function() tinyplot(Temp ~ Day | Month, data = aq, type = "p")
expect_snapshot_plot(f, label = "aesthetics_type_b")

f = function() tinyplot(Temp ~ Day | Month, data = aq, type = "b", lty = 1:5)
expect_snapshot_plot(f, label = "aesthetics_type_b_lty")

f = function() tinyplot(Temp ~ Day | Month, data = aq, type = "b", lwd = 1:5)
expect_snapshot_plot(f, label = "aesthetics_type_b_lwd")

f = function() tinyplot(Temp ~ Day | Month, data = aq, type = "b", col = "black", pch = 1:5)
expect_snapshot_plot(f, label = "aesthetics_type_b_col_pch")

# check that non-point types don't generate points accidentally
f = function() tinyplot(Temp ~ Day | Month, data = aq, type = "l")
expect_snapshot_plot(f, label = "aesthetics_type_l")

#
# Next, convenience versions using the "by" keyword
# 

f = function() tinyplot(Temp ~ Day | Month, data = aq, type = "b", pch = "by", lty = "by", lwd = "by")
expect_snapshot_plot(f, label = "aesthetics_by")

# check non-point types don't generate points in legend accidentally
f = function() tinyplot(Temp ~ Day | Month, data = aq, type = "l", pch = "by", lty = "by", lwd = "by")
expect_snapshot_plot(f, label = "aesthetics_by_type_l")

# no color version
f = function() tinyplot(Temp ~ Day | Month, data = aq, type = "b", pch = "by", lty = "by", lwd = "by", col = "black")
expect_snapshot_plot(f, label = "aesthetics_by_nocol")

# inherit global par
f = function() {
  par(pch = 16)
  tinyplot(Temp ~ Day | Month, data = aq, type = "b", pch = "by", lty = "by", lwd = "by")
}
expect_snapshot_plot(f, label = "aesthetics_by_par")

# as above but check recycling
f = function() {
  par(pch = 25, lty = 6, lwd = 0.5)
  tinyplot(Temp ~ Day | Month, data = aq, type = "b", pch = "by", lty = "by", lwd = "by")
}
expect_snapshot_plot(f, label = "aesthetics_by_recycle")

# reset par
par(op)