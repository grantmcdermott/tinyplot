source("helpers.R")
using("tinysnapshot")

# Missing labels should reclaim margin space under dynmar themes.
f = function() {
  tinytheme("clean")
  tinyplot(1:10, 1:10, xlab = NA, ylab = NA, main = NA)
  tinytheme()
}
expect_snapshot_plot(f, label = "margins_missing_labels")

# Multi-line annotation strings should increase margins as needed.
f = function() {
  tinytheme("clean")
  tinyplot(1:10, 1:10, xlab = "A\nB", ylab = "C\nD", main = "E\nF")
  tinytheme()
}
expect_snapshot_plot(f, label = "margins_multiline_labels")

# Faceted path should also handle multi-line annotation strings.
f = function() {
  tinytheme("clean")
  tinyplot(mpg ~ wt | cyl, data = mtcars, xlab = "Weight\n(1000 lbs)")
  tinytheme()
}
expect_snapshot_plot(f, label = "margins_facet_multiline")

# sub = NA should not reserve extra bottom legend space.
f = function() {
  tinytheme("clean", side.sub = 1)
  tinyplot(mpg ~ wt | factor(cyl), data = mtcars, legend = "bottom!", sub = NA)
  tinytheme()
}
expect_snapshot_plot(f, label = "margins_sub_na_bottom_legend")

# 2x2 layout from reprex should handle mixed single/multiline labels.
f = function() {
  tinytheme("clean")
  on.exit(tinytheme(), add = TRUE)

  set.seed(1)
  x = 1:20
  y = x + stats::rnorm(length(x), sd = 2)

  op = par(mfrow = c(2, 2))
  on.exit(par(op), add = TRUE)

  tinyplot(x, y, main = "xlab = NA", xlab = NA, ylab = "Y")
  tinyplot(x, y, main = "xlab present", xlab = "X", ylab = "Y")
  tinyplot(x, y, main = "Multi-line labels",
    xlab = "X line 1\nX line 2", ylab = "Y line 1\nY line 2")
  tinyplot(x, y, main = "Multi-line\nmain title", xlab = "X", ylab = "Y")
}
expect_snapshot_plot(f, label = "margins_multiline_2x2")

get_plot_mar = function(...) {
  op = par(no.readonly = TRUE)
  on.exit(par(op), add = TRUE)
  tinyplot(...)
  par("mar")
}

expect_true(
  {
    tinytheme("clean")
    on.exit(tinytheme(), add = TRUE)
    mar_with_xlab = get_plot_mar(1:10, 1:10, xlab = "X")
    mar_without_xlab = get_plot_mar(1:10, 1:10, xlab = NA)
    mar_without_xlab[1] < mar_with_xlab[1]
  },
  info = "bottom_margin_shrinks_when_xlab_missing"
)

expect_true(
  {
    tinytheme("clean")
    on.exit(tinytheme(), add = TRUE)
    mar_single_xlab = get_plot_mar(1:10, 1:10, xlab = "X")
    mar_multiline_xlab = get_plot_mar(1:10, 1:10, xlab = "X\nY")
    mar_multiline_xlab[1] > mar_single_xlab[1]
  },
  info = "bottom_margin_grows_for_multiline_xlab"
)
