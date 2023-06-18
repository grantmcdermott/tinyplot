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
expect_snapshot_plot(f, label = "legend_keyword_default")

f = function() with(airquality, plot2(x = density(Temp), by = Month, legend = "bottom!"))
expect_snapshot_plot(f, label = "legend_keyword_density")

f = function() plot2(Temp ~ Day | Month, data = airquality, legend = "bottom!")
expect_snapshot_plot(f, label = "legend_keyword_formula")

# test other outer keywords

f = function() plot2(Temp ~ Day | Month, data = airquality, legend = "left!")
expect_snapshot_plot(f, label = "legend_keyword_outerleft")
f = function() plot2(Temp ~ Day | Month, data = airquality, legend = "bottom!")
expect_snapshot_plot(f, label = "legend_keyword_outerbottom")
f = function() plot2(Temp ~ Day | Month, data = airquality, legend = "top!")
expect_snapshot_plot(f, label = "legend_keyword_outertop")
f = function() plot2(Temp ~ Day | Month, data = airquality, legend = "topright!")
expect_snapshot_plot(f, label = "legend_keyword_outertopright")
f = function() plot2(Temp ~ Day | Month, data = airquality, legend = "bottomleft!")
expect_snapshot_plot(f, label = "legend_keyword_outerbottomleft")


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

f = function() plot2(
  Temp ~ Day | Month, data = airquality,
  legend = legend(legend = month.abb[5:9])
  )
expect_snapshot_plot(f, label = "legend_user_labs")

f = function() plot2(
  Temp ~ Day | Month, data = airquality,
  legend = legend(legend = month.abb[5:10])
  )
expect_warning(expect_snapshot_plot(f, label = "legend_user_labs_override"))


# reset par
par(op)
