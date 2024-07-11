source("helpers.R")
using("tinysnapshot")

# test case adapted from ?segments Examples
set.seed(42L)
x = stats::runif(12); y <- stats::rnorm(12)
i = order(x, y); x = x[i]; y = y[i]
s = seq(length(x)-1)  # one shorter than data
s = s[-length(s)]
grp = rep(LETTERS[1:2], 5)

f = function() {
  plt(
    xmin = x[s], ymin = y[s], xmax = x[s+2], ymax = y[s+2],
    by = grp,
    type = "segments"
  )
}
expect_snapshot_plot(f, label = "segments_by")

# also vary lty and lwd by groups
f = function() {
  plt(
    xmin = x[s], ymin = y[s], xmax = x[s+2], ymax = y[s+2],
    by = grp,
    lty = "by", lwd = "by",
    type = "segments"
  )
}
expect_snapshot_plot(f, label = "segments_by_extra")

# xmin = xmax
f = function() {
  plt(
    xmin = x[s], ymin = y[s], xmax = x[s], ymax = y[s+2],
    by = grp,
    lty = "by", lwd = "by",
    type = "segments"
  )
}
expect_snapshot_plot(f, label = "segments_by_xequal")

# ymin = ymax
f = function() {
  plt(
    xmin = x[s], ymin = y[s], xmax = x[s+2], ymax = y[s],
    by = grp,
    lty = "by", lwd = "by",
    type = "segments"
  )
}
expect_snapshot_plot(f, label = "segments_by_yequal")

rm(x, y, i, s, grp)
