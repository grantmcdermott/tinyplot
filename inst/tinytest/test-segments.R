source("helpers.R")
using("tinysnapshot")

f = function() {
  # test case adapted from ?segments Examples
  set.seed(42L)
  x = stats::runif(12); y <- stats::rnorm(12)
  i = order(x, y); x <- x[i]; y <- y[i]
  s = seq(length(x)-1)  # one shorter than data
  s = s[-length(s)]
  grp = rep(LETTERS[1:2], 5)
  plt(
    xmin = x[s], ymin = y[s], xmax = x[s+2], ymax = y[s+2],
    by = grp,
    type = "segments"
  )
}
expect_snapshot_plot(f, label = "segments_by")
