source("helpers.R")
using("tinysnapshot")

i = seq(0, 2*pi, by = 0.01)
x = 16*sin(i)^3
y = 13*cos(i) - 5*cos(2*i) - 2*cos(3*i) - cos(4*i)

f = function() {
  plt(
    x, y,
    type = "polygon",
    col = "red", fill = adjustcolor("firebrick", 0.8),
    axes = FALSE, xlab = NA, ylab = NA
  )
}
expect_snapshot_plot(f, label = "polygon_heart")
