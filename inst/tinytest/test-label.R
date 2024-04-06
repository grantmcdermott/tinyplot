source("helpers.R")
using("tinysnapshot")

op = tpar()

f = function() {
  set.seed(42)
  tinyplot(rnorm(1))
}
expect_snapshot_plot(f, label = "ylab_good")

# reset par
tpar(op)