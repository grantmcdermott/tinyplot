source("helpers.R")
using("tinysnapshot")
if (ON_CRAN) exit_file("CRAN")
if (!ON_LINUX) exit_file("Linux snapshots")

op = par(no.readonly = TRUE)

f = function() {
  set.seed(42)
  tinyplot(rnorm(1))
}
expect_snapshot_plot(f, label = "ylab_good")

# reset par
par(op)