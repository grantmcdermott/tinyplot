source("helpers.R")
using("tinysnapshot")
if (Sys.info()["sysname"] != "Linux") exit_file("Linux snapshots")

op = par(no.readonly = TRUE)


# logical legend example

f = function() plot2(Temp ~ Day | Month, data = airquality, legend = FALSE)
expect_snapshot_plot(f, label = "legend_false")

f = function() plot2(Temp ~ Day | Month, data = airquality, legend = TRUE)
expect_snapshot_plot(f, label = "legend_true")

# position keyword examples 

f = function() with(airquality, plot2(x = Day, y = Temp, by = Month, legend = "bottom!"))
expect_snapshot_plot(f, label = "legend_default_keyword")

f = function() with(airquality, plot2(x = density(Temp), by = Month, legend = "bottom!"))
expect_snapshot_plot(f, label = "legend_density_keyword")

f = function() plot2(Temp ~ Day | Month, data = airquality, legend = "bottom!")
expect_snapshot_plot(f, label = "legend_formula_keyword")


# legend function examples

f = function() with(
  airquality,
  plot2(
    x = Day, y = Temp, by = Month,
    legend = legend("bottom!", title = "Month of the year", bty = "o")
    )
  )
expect_snapshot_plot(f, label = "legend_default_legend")

f = function() with(
  airquality,
  plot2(
    x = density(Temp), by = Month,
    legend = legend("bottom!", title = "Month of the year", bty = "o")
    )
  )
expect_snapshot_plot(f, label = "legend_density_legend")

f = function() plot2(
  Temp ~ Day | Month, data = airquality,
  legend = legend("bottom!", title = "Month of the year", bty = "o")
  )
expect_snapshot_plot(f, label = "legend_formula_legend")

f = function() plot2(
  Temp ~ Day | Month, data = airquality,
  legend = legend(title = NULL)
  )
expect_snapshot_plot(f, label = "legend_title_null")


# reset par
par(op)
