source("helpers.R")
using("tinysnapshot")

f = function() tinyplot(
  lat ~ long | depth, quakes,
  pch = 19,
  main = "Gradient legend"
)
expect_snapshot_plot(f, label = "legend_gradient_default")

f = function() tinyplot(
  lat ~ long | depth, quakes,
  pch = 19,
  legend = "bottom!",
  main = "Gradient legend (bottom!)"
)
expect_snapshot_plot(f, label = "legend_gradient_outer_bottom")

f = function() tinyplot(
  lat ~ long | depth, quakes,
  pch = 19,
  legend = "bottomleft",
  main = "Gradient legend (bottom left)"
)
expect_snapshot_plot(f, label = "legend_gradient_inner_bottomleft")

f = function() tinyplot(
  lat ~ long | depth, quakes,
  pch = 19,
  palette = hcl.colors(palette = "rocket", alpha = 0.5),
  main = "Gradient legend (alpha)"
)
expect_snapshot_plot(f, label = "legend_gradient_alpha")

f = function() tinyplot(
  lat ~ long | depth, quakes,
  pch = 21,
  col = "white", bg = "by", cex = 1.5,
  main = "Gradient legend (bg)"
)
expect_snapshot_plot(f, label = "legend_gradient_bg")

f = function() tinyplot(
  lat ~ long | depth, quakes,
  pch = 21,
  bg = "grey", cex = 1.5,
  main = "Gradient legend (bg scalar)"
)
expect_snapshot_plot(f, label = "legend_gradient_bg_scalar")

f = function() tinyplot(
  mpg ~ wt | disp, mtcars,
  type = "l",
  main = "Gradient legend (line segments)"
)
expect_snapshot_plot(f, label = "legend_gradient_line_segments")

# check overrides ----

# discrete override with warning for the types that still can't carry a
# per-observation colour (e.g. "b", whose gap handling isn't wired up yet)
expect_warning(tinyplot(mpg ~ wt | disp, mtcars, type = "b"))
