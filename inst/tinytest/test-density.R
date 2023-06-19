source("helpers.R")
using("tinysnapshot")
if (Sys.info()["sysname"] != "Linux") exit_file("Linux snapshots")

## Sidestep test fails due to new (R 4.4.0) density grid value calculations.
## https://bugs.r-project.org/show_bug.cgi?id=18337
exit_if_not(getRversion()  <= "4.3.1")
## Note: Once 4.4.0 is released we can either generate some new plots or
## test with something like:
# f = function() plot2(density(mtcars$mpg, old.coords=TRUE))

f = function() plot2(density(mtcars$mpg))
expect_snapshot_plot(f, label = "density_nogroups")

f = function() plot2(density(mtcars$mpg), by = mtcars$am)
expect_snapshot_plot(f, label = "density_numeric")

f = function() plot2(density(iris$Sepal.Width), by = iris$Species)
expect_snapshot_plot(f, label = "density_factor")
