source("helpers.R")
using("tinysnapshot")

f = function() {
  tinyplot(~hp, type = "qq", data = mtcars, lty = 3, pch = 1, col = "red")
}
expect_snapshot_plot(f, label = "qq_01")

f = function() {
  tinyplot(~hp, type = "qq", data = mtcars, lty = 0)
}
expect_snapshot_plot(f, label = "qq_02")
