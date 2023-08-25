source("helpers.R")
using("tinysnapshot")
if (Sys.info()["sysname"] != "Linux") exit_file("Linux snapshots")

## Sidestep test fails due to new (R 4.4.0) density grid value calculations.
## https://bugs.r-project.org/show_bug.cgi?id=18337
exit_if_not(getRversion()  <= "4.3.1")
## Note: Once 4.4.0 is released we can either generate some new plots or
## test with something like:
# f = function() plot2(density(mtcars$mpg, old.coords=TRUE))

f = function() with(mtcars, plot2(density(mpg)))
expect_snapshot_plot(f, label = "density_nogroups")

f = function() with(mtcars, plot2(density(mpg), by = am))
expect_snapshot_plot(f, label = "density_numeric")

f = function() with(iris, plot2(density(Sepal.Width), by = Species))
expect_snapshot_plot(f, label = "density_factor")

f = function() with(iris, plot2(density(Sepal.Width), by = Species, bg = "by"))
expect_snapshot_plot(f, label = "density_fill")

f = function() with(iris, plot2(density(Sepal.Width), by = Species, fill = "by"))
expect_snapshot_plot(f, label = "density_fill")

f = function() with(iris, plot2(density(Sepal.Width), by = Species, type = "area"))
expect_snapshot_plot(f, label = "density_fill")

## Now test `type = "density"` versions (both atomic and formula)
## Should be the same as above, modulo missing titles

f1 = function() with(mtcars, plot2(mpg, type = "density"))
f2 = function() plot2(~ mpg, mtcars, type = "density")
expect_snapshot_plot(f1, label = "density_type_nogroups")
expect_snapshot_plot(f2, label = "density_type_nogroups")

f1 = function() with(mtcars, plot2(mpg, by = am, type = "density"))
f2 = function() plot2(~ mpg | am, mtcars, type = "density")
expect_snapshot_plot(f1, label = "density_type_numeric")
expect_snapshot_plot(f2, label = "density_type_numeric")

f1 = function() with(iris, plot2(Sepal.Width, by = Species, type = "density"))
f2 = function() plot2(~ Sepal.Width | Species, iris, type = "density")
expect_snapshot_plot(f1, label = "density_type_factor")
expect_snapshot_plot(f2, label = "density_type_factor")

f1a = function() with(iris, plot2(Sepal.Width, by = Species, type = "density", bg = "by"))
f1b = function() with(iris, plot2(Sepal.Width, by = Species, type = "density", fill = "by"))
f2a = function() plot2(~ Sepal.Width | Species, iris, type = "density", bg = "by")
f2b = function() plot2(~ Sepal.Width | Species, iris, type = "density", fill = "by")
expect_snapshot_plot(f1a, label = "density_type_fill")
expect_snapshot_plot(f1b, label = "density_type_fill")
expect_snapshot_plot(f2a, label = "density_type_fill")
expect_snapshot_plot(f2b, label = "density_type_fill")
