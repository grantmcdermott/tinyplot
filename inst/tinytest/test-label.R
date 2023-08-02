source("helpers.R")
using("tinysnapshot")
# if (Sys.info()["sysname"] != "Linux") exit_file("Linux snapshots")

op = par(no.readonly = TRUE)

f = function() {
  set.seed(42)
  plot2(rnorm(1))
} 
expect_snapshot_plot(f, label = "ylab_good")

# reset par
par(op)
