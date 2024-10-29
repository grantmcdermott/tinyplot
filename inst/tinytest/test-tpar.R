source("helpers.R")
using("tinysnapshot")

f = function () {
  op = tpar(grid = TRUE, grid.col = "hotpink", grid.lty = "solid", grid.lwd = 2)
  tinyplot(0:10, pch = 19)
  tpar(op)
}
expect_snapshot_plot(f, label = "tpar_grid")