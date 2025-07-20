source("helpers.R")
using("tinysnapshot")

aq = airquality
aq$Month = as.factor(aq$Month)

op = par(no.readonly = TRUE)

# logical legend example

f = function() tinyplot(Temp ~ Day | Month, data = aq, legend = FALSE)
expect_snapshot_plot(f, label = "legend_false")

f = function() tinyplot(Temp ~ Day | Month, data = aq, legend = TRUE)
expect_snapshot_plot(f, label = "legend_true")

# position keyword examples 

f = function() with(aq, tinyplot(x = Day, y = Temp, by = Month, legend = "bottom!"))
expect_snapshot_plot(f, label = "legend_keyword_default")

if ((getRversion() <= "4.3.2")) {
  f = function() with(aq, tinyplot(x = density(Temp), by = Month, legend = "bottom!"))
  expect_snapshot_plot(f, label = "legend_keyword_density")
}

f = function() tinyplot(Temp ~ Day | Month, data = aq, legend = "bottom!")
expect_snapshot_plot(f, label = "legend_keyword_formula")

# test other outer keywords

f = function() tinyplot(Temp ~ Day | Month, data = aq, legend = "left!")
expect_snapshot_plot(f, label = "legend_keyword_outerleft")
f = function() tinyplot(Temp ~ Day | Month, data = aq, legend = "bottom!")
expect_snapshot_plot(f, label = "legend_keyword_outerbottom")
f = function() tinyplot(Temp ~ Day | Month, data = aq, legend = "top!")
expect_snapshot_plot(f, label = "legend_keyword_outertop")
f = function() tinyplot(Temp ~ Day | Month, data = aq, legend = "topright!")
expect_snapshot_plot(f, label = "legend_keyword_outertopright")
f = function() tinyplot(Temp ~ Day | Month, data = aq, legend = "bottomleft!")
expect_snapshot_plot(f, label = "legend_keyword_outerbottomleft")

# Horizontal and/or multicolumn legend spacing

aq$Month2 = factor(month.name[aq$Month], levels = month.name[5:9])
f = function() tinyplot(Temp ~ Day | Month2, data = aq, legend = "bottom!")
expect_snapshot_plot(f, label = "legend_spacing_horiz_label_bottom")

f = function() tinyplot(
  weight ~ Time | Chick,
  data = ChickWeight,
  type = "ribbon", # not necessary for plot but helps to check some internal logic
  legend = list("right!", ncol = 3)
)
expect_snapshot_plot(f, label = "legend_spacing_ncol_right")

f = function() tinyplot(
  weight ~ Time | Chick,
  data = ChickWeight,
  type = "l",
  legend = list("bottom!", ncol = 5)
)
expect_snapshot_plot(f, label = "legend_spacing_ncol_bottom")

# Long legend titles

f = function() tinyplot(
  Temp ~ Day | Month, data = aq, main = "Long title",
  legend = list("right!", title = "What month of the year is it?", bty = "o")
)
expect_snapshot_plot(f, label = "legend_long_right")

f = function() tinyplot(
  Temp ~ Day | Month, data = aq, main = "Multiline title",
  legend = list("left!", title = "What month of the year is it?", bty = "o")
)
expect_snapshot_plot(f, label = "legend_long_left")

# multi-line legend titles

f = function() tinyplot(
  Temp ~ Day | Month, data = aq, main = "Multiline title",
  legend = list("top!", title = "Month\nof\nthe\nyear", bty = "o")
)
expect_snapshot_plot(f, label = "legend_multiline_top")

f = function() tinyplot(
  Temp ~ Day | Month, data = aq, main = "Multiline title",
  legend = list("bottom!", title = "Month\nof\nthe\nyear", bty = "o")
)
expect_snapshot_plot(f, label = "legend_multiline_bottom")

# legend function examples

f = function() with(
  aq,
  tinyplot(
    x = Day, y = Temp, by = Month,
    legend = legend("bottom!", title = "Month of the year", bty = "o")
  )
)
expect_snapshot_plot(f, label = "legend_default_legend")

if ((getRversion() <= "4.3.1")) {
  f = function() with(
    aq,
    tinyplot(
      x = density(Temp), by = Month,
      legend = legend("bottom!", title = "Month of the year", bty = "o")
    )
  )
  expect_snapshot_plot(f, label = "legend_density_legend")
}

f = function() tinyplot(
  Temp ~ Day | Month, data = aq,
  legend = legend("bottom!", title = "Month of the year", bty = "o")
)
expect_snapshot_plot(f, label = "legend_formula_legend")

f = function() tinyplot(
  Temp ~ Day | Month, data = aq,
  legend = legend(title = NULL)
)
expect_snapshot_plot(f, label = "legend_title_null")

f = function() tinyplot(
  Temp ~ Day | Month, data = aq,
  legend = legend(legend = month.abb[5:9])
)
expect_snapshot_plot(f, label = "legend_user_labs")

f = function() tinyplot(
  Temp ~ Day | Month, data = aq,
  legend = legend(legend = month.abb[5:10])
)
expect_warning(expect_snapshot_plot(f, label = "legend_user_labs_override"))


# override default legend margins with tpar("lmar")

f = function() {
  olmar = tpar("lmar")
  tpar(lmar = c(1.5, 0.5))
  tinyplot(Temp ~ Day | Month, data = aq, legend = list("right!", bty = "o"))
  box("figure", lty = 2, col = "blue")
  tpar(lmar = olmar)
}
expect_snapshot_plot(f, label = "legend_lmar_right")
f = function() {
  olmar = tpar("lmar")
  tpar(lmar = c(1.5, 0.5))
  tinyplot(Temp ~ Day | Month, data = aq, legend = list("left!", bty = "o"))
  box("figure", lty = 2, col = "blue")
  tpar(lmar = olmar)
}
expect_snapshot_plot(f, label = "legend_lmar_left")
f = function() {
  olmar = tpar("lmar")
  tpar(lmar = c(1.5, 0.5))
  tinyplot(Temp ~ Day | Month, data = aq, legend = list("bottom!", bty = "o"))
  box("figure", lty = 2, col = "blue")
  tpar(lmar = olmar)
}
expect_snapshot_plot(f, label = "legend_lmar_bottom")
f = function() {
  olmar = tpar("lmar")
  tpar(lmar = c(1.5, 0.5))
  tinyplot(Temp ~ Day | Month, data = aq, legend = list("top!", bty = "o"))
  box("figure", lty = 2, col = "blue")
  tpar(lmar = olmar)
}
expect_snapshot_plot(f, label = "legend_lmar_top")


# reset par
par(op)
