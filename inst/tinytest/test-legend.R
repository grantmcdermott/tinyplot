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

if ((getRversion() <= "4.3.2")) {
  f = function() with(airquality, plot2(x = density(Temp), by = Month, legend = "bottom!"))
  expect_snapshot_plot(f, label = "legend_keyword_density")
}

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

# Long legend titles

f = function() plot2(
  Temp ~ Day | Month, data = airquality, main = "Long title",
  legend = list("right!", title = "What month of the year is it?", bty = "o")
)
expect_snapshot_plot(f, label = "legend_long_right")

f = function() plot2(
  Temp ~ Day | Month, data = airquality, main = "Multiline title",
  legend = list("left!", title = "What month of the year is it?", bty = "o")
)
expect_snapshot_plot(f, label = "legend_long_left")

# multi-line legend titles

f = function() plot2(
  Temp ~ Day | Month, data = airquality, main = "Multiline title",
  legend = list("top!", title = "Month\nof\nthe\nyear", bty = "o")
)
expect_snapshot_plot(f, label = "legend_multiline_top")

f = function() plot2(
  Temp ~ Day | Month, data = airquality, main = "Multiline title",
  legend = list("bottom!", title = "Month\nof\nthe\nyear", bty = "o")
)
expect_snapshot_plot(f, label = "legend_multiline_bottom")

# legend function examples

f = function() with(
  airquality,
  plot2(
    x = Day, y = Temp, by = Month,
    legend = legend("bottom!", title = "Month of the year", bty = "o")
  )
)
expect_snapshot_plot(f, label = "legend_default_legend")

if ((getRversion() <= "4.3.1")) {
  f = function() with(
    airquality,
    plot2(
      x = density(Temp), by = Month,
      legend = legend("bottom!", title = "Month of the year", bty = "o")
    )
  )
  expect_snapshot_plot(f, label = "legend_density_legend")
}

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


# override default legend margins with par2("lmar")

f = function() {
  olmar = par2("lmar")
  par2(lmar = c(1.5, 0.5))
  plot2(Temp ~ Day | Month, data = airquality, legend = list("right!", bty = "o"))
  box("figure", lty = 2, col = "blue")
  par2(lmar = olmar)
}
expect_snapshot_plot(f, label = "legend_lmar_right")
f = function() {
  olmar = par2("lmar")
  par2(lmar = c(1.5, 0.5))
  plot2(Temp ~ Day | Month, data = airquality, legend = list("left!", bty = "o"))
  box("figure", lty = 2, col = "blue")
  par2(lmar = olmar)
}
expect_snapshot_plot(f, label = "legend_lmar_left")
f = function() {
  olmar = par2("lmar")
  par2(lmar = c(1.5, 0.5))
  plot2(Temp ~ Day | Month, data = airquality, legend = list("bottom!", bty = "o"))
  box("figure", lty = 2, col = "blue")
  par2(lmar = olmar)
}
expect_snapshot_plot(f, label = "legend_lmar_bottom")
f = function() {
  olmar = par2("lmar")
  par2(lmar = c(1.5, 0.5))
  plot2(Temp ~ Day | Month, data = airquality, legend = list("top!", bty = "o"))
  box("figure", lty = 2, col = "blue")
  par2(lmar = olmar)
}
expect_snapshot_plot(f, label = "legend_lmar_top")


# reset par
par(op)
