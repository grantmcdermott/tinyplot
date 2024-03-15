source("helpers.R")
using("tinysnapshot")
if (Sys.info()["sysname"] != "Linux") exit_file("Linux snapshots")

op = tpar()

f = function() {
  set.seed(42)
  tinyplot(rnorm(1))
}
expect_snapshot_plot(f, label = "ylab_good")

# reset par
tpar(op)