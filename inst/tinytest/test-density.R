source("helpers.R")
using("tinysnapshot")
if (Sys.info()["sysname"] != "Linux") exit_file("Linux snapshots")

f = function() plot2(density(mtcars$mpg), old.coords=TRUE)
expect_snapshot_plot(f, label = "density_nogroups")

f = function() plot2(density(mtcars$mpg), by = mtcars$am, old.coords=TRUE)
expect_snapshot_plot(f, label = "density_numeric")

f = function() plot2(density(iris$Sepal.Width), by = iris$Species, old.coords=TRUE)
expect_snapshot_plot(f, label = "density_factor")
