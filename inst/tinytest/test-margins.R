source("helpers.R")
using("tinysnapshot")

# Missing labels should reclaim margin space under dynmar themes.
f = function() {
  tinyplot(1:10, 1:10, xlab = NA, ylab = NA, main = NA, theme = "clean")
}
expect_snapshot_plot(f, label = "margins_missing_labels")

# Multi-line annotation strings should increase margins as needed.
f = function() {
  tinyplot(
    1000:1010,
    xlab = "xlab 1\nxlab 2", ylab = "ylab 1\nylab 2",
    main = "main 1\nmain 2", sub = "sub 1\nsub 2",
    theme = "clean"
  )
}
expect_snapshot_plot(f, label = "margins_multiline_labels")

# Faceted path should also handle multi-line annotation strings.
f = function() {
  tinyplot(
    mpg ~ wt | cyl, data = mtcars,
    xlab = "Weight\n(1000 lbs)",
    theme = "clean"
  )
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

# Expression labels should expand margins beyond plain text (#549)
expect_true(
  {
    tinytheme("clean")
    on.exit(tinytheme(), add = TRUE)
    mar_text = get_plot_mar(1:10, 1:10, ylab = "Precipitation")
    mar_expr = get_plot_mar(1:10, 1:10, ylab = expression(Precipitation~"["~mm^{1/2}~"]"))
    mar_expr[2] > mar_text[2]
  },
  info = "left_margin_grows_for_expression_ylab"
)

expect_true(
  {
    tinytheme("clean")
    on.exit(tinytheme(), add = TRUE)
    mar_text = get_plot_mar(1:10, 1:10, xlab = "x")
    mar_expr = get_plot_mar(1:10, 1:10, xlab = expression(frac(y, x)))
    mar_expr[1] > mar_text[1]
  },
  info = "bottom_margin_grows_for_expression_xlab"
)

# NOTE: Snapshot test commented out due to minor, unexplained rendering diffs
# between devcontainer and CI (likely R 4.6.0 font metric changes). The logical
# tests above verify the margin math is correct.
# f = function() {
#   tinyplot(1:10,
#     xlab = expression(frac(y, x)),
#     ylab = expression(Precipitation~"["~mm^{1/2}~"]"),
#     theme = "clean")
# }
# expect_snapshot_plot(f, label = "margins_math_expression")

# Dynamic margin and mgp scaling with large cex values (#574)

# Helper: capture mar and mgp from inside the plot
get_plot_pars = function(...) {
  tinyplot(...)
  list(mar = par("mar"), mgp = par("mgp"))
}

# At default cex=1, mgp is unchanged from the theme value
expect_equal(
  {
    tinytheme("clean")
    p = get_plot_pars(1:10, 1:10, xlab = "X", ylab = "Y")
    tinytheme()
    p$mgp
  },
  c(2.2, 0.7, 0),
  info = "dynmar_mgp_unchanged_at_cex_1"
)

# cex.axis=3, cex.lab=2: mgp scales to accommodate larger text
expect_equal(
  {
    tinytheme("clean", cex.axis = 3, cex.lab = 2)
    p = get_plot_pars(1:10, 1:10, xlab = "X", ylab = "Y")
    tinytheme()
    p$mgp
  },
  c(3.7, 1.7, 0),
  info = "dynmar_mgp_at_cex_axis_3_cex_lab_2"
)

# cex.axis=0.5, cex.lab=0.5: mgp shrinks for small text
expect_equal(
  {
    tinytheme("clean", cex.axis = 0.5, cex.lab = 0.5)
    p = get_plot_pars(1:10, 1:10, xlab = "X", ylab = "Y")
    tinytheme()
    p$mgp
  },
  c(1.7, 0.45, 0),
  info = "dynmar_mgp_shrinks_at_small_cex"
)

# Snapshot tests for scaled-cex margins
f = function() {
  tinytheme("clean", cex.axis = 3, cex.lab = 2)
  tinyplot(1000:1010, xlab = "X label", ylab = "Y label", main = "cex.axis=3, cex.lab=2")
  tinytheme()
}
expect_snapshot_plot(f, label = "margins_large_cex")

f = function() {
  tinytheme("clean", cex.axis = 2, cex.lab = 1.5)
  tinyplot(mpg ~ wt | cyl, facet = "by", data = mtcars, xlab = "Weight", ylab = "Miles per gallon")
  tinytheme()
}
expect_snapshot_plot(f, label = "margins_large_cex_facets")

# Varying cex.axis vs cex.lab independently to check gap constancy
f = function() {
  tinytheme("clean", cex.axis = 3, cex.lab = 1)
  tinyplot(1000:1010, xlab = "X title (JjQqYy)", ylab = "Y title (JjQqYy)",
           main = "cex.axis = 3, cex.lab = 1")
  box("inner", lty = 2)
  tinytheme()
}
expect_snapshot_plot(f, label = "margins_cex_axis3_lab1")

f = function() {
  tinytheme("clean", cex.axis = 1, cex.lab = 3)
  tinyplot(1000:1010, xlab = "X title (JjQqYy)", ylab = "Y title (JjQqYy)",
           main = "cex.axis = 1, cex.lab = 3")
  box("inner", lty = 2)
  tinytheme()
}
expect_snapshot_plot(f, label = "margins_cex_axis1_lab3")

f = function() {
  tinytheme("clean", cex.axis = 3, cex.lab = 3)
  tinyplot(1000:1010, xlab = "X title (JjQqYy)", ylab = "Y title (JjQqYy)",
           main = "cex.axis = 3, cex.lab = 3")
  box("inner", lty = 2)
  tinytheme()
}
expect_snapshot_plot(f, label = "margins_cex_axis3_lab3")

# Consistent y-title gap across label widths (#596)
f = function() {
  tinytheme("bw", grid = FALSE)
  tinyplot(1, xlab = "Xx", ylab = "Yy", type = "n")
  tinytheme()
}
expect_snapshot_plot(f, label = "margins_whtsbp_degenerate")

f = function() {
  tinytheme("bw", grid = FALSE)
  tinyplot(1, yaxb = 1, xaxb = 1, xlab = "Xx", ylab = "Yy", type = "n")
  tinytheme()
}
expect_snapshot_plot(f, label = "margins_whtsbp_1digit")

f = function() {
  tinytheme("bw", grid = FALSE)
  tinyplot(10, yaxb = 10, xaxb = 1, xlab = "Xx", ylab = "Yy", type = "n")
  tinytheme()
}
expect_snapshot_plot(f, label = "margins_whtsbp_2digit")

f = function() {
  tinytheme("bw", grid = FALSE)
  tinyplot(100, yaxb = 100, xaxb = 1, xlab = "Xx", ylab = "Yy", type = "n")
  tinytheme()
}
expect_snapshot_plot(f, label = "margins_whtsbp_3digit")