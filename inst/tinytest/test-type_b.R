source("helpers.R")
using("tinysnapshot")

## Avoid test fails on older R versions (pre 4.4.0) due to slight change in
## density grid value calculations.
## https://bugs.r-project.org/show_bug.cgi?id=18337
if (getRversion() < "4.4.0") exit_file("R < 4.4.0")


# Issue #307
set.seed(48103)
f1 = function() plt(runif(10) ~ rnorm(10), type = "b", cex = 1)
f2 = function() plt(runif(10) ~ rnorm(10), type = "b", cex = 3)
expect_snapshot_plot(f1, label = "type_b_cex_1")
expect_snapshot_plot(f2, label = "type_b_cex_3")
