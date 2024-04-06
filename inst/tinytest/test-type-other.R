source("helpers.R")
using("tinysnapshot")

aq = airquality
aq$Month = as.factor(aq$Month)

f = function() tinyplot(0:10, type = "c")
expect_snapshot_plot(f, label = "type_c")

f = function() tinyplot(0:10, type = "h")
expect_snapshot_plot(f, label = "type_h")

f = function() tinyplot(0:10, type = "s")
expect_snapshot_plot(f, label = "type_s")

f = function() tinyplot(0:10, type = "S")
expect_snapshot_plot(f, label = "type_cap_s")

f = function() tinyplot(Temp ~ Day | Month, aq, type = "c")
expect_snapshot_plot(f, label = "type_c_group")

f = function() tinyplot(Temp ~ Day | Month, aq, type = "h")
expect_snapshot_plot(f, label = "type_h_group")

f = function() tinyplot(Temp ~ Day | Month, aq, type = "s")
expect_snapshot_plot(f, label = "type_s_group")

f = function() tinyplot(Temp ~ Day | Month, aq, type = "S")
expect_snapshot_plot(f, label = "type_cap_s_group")
