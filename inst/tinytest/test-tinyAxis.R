source("helpers.R")
using("tinysnapshot")

cyl_labs = c(
  "4" = "Four cylinders",
  "6" = "Six cylinders",
  "8" = "Eight cylinders"
)
bars = function(...) {
  tinyplot(~cyl, data = mtcars, type = "barplot", xaxl = cyl_labs,
           theme = "dynamic", ...)
}


f = function() bars(xaxr = 45)
expect_snapshot_plot(f, label = "axis_rotation_x45")

# srt = 90 should look like the las = 2 it reduces to.
f = function() bars(xaxr = 90)
expect_snapshot_plot(f, label = "axis_rotation_x90")

f = function() {
  tinyplot(mpg ~ factor(cyl), facet = ~am, data = mtcars, type = "boxplot",
           xaxl = cyl_labs, xaxr = 45, theme = "clean")
}
expect_snapshot_plot(f, label = "axis_rotation_facet")

# flip moves the x variable onto the vertical axis; xaxr follows the variable,
# not the side, and the margin has to follow it too.
#
# Both axis titles are load-bearing here. Placing either from the other side's
# measurement goes badly in a way only this combination shows: side 2 puts the
# title over the tick labels, side 1 puts it off the canvas entirely.
f = function() {
  tinyplot(mpg ~ factor(cyl), data = mtcars, type = "boxplot", flip = TRUE,
           xaxl = cyl_labs, xaxr = 45, theme = "clean")
}
expect_snapshot_plot(f, label = "axis_rotation_flip")
