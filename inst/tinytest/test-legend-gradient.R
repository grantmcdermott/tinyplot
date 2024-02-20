source("helpers.R")
using("tinysnapshot")
if (Sys.info()["sysname"] != "Linux") exit_file("Linux snapshots")

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
  palette = hcl.colors(palette = "rocket", alpha = 0.5),
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

# check overrides ----

# discrete override with warning for certain types (e.g. "l")
expect_warning(tinyplot(mpg ~ wt | disp, mtcars, type = "l"))
