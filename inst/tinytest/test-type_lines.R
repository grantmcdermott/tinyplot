source("helpers.R")
using("tinysnapshot")

# Issue #679: type_lines() should place categorical data on the axes the same
# way type_points() does, i.e. following the factor levels (rather than the
# order in which the categories happen to appear in the data).

LOTR = data.frame(
  name = c("Fellowship", "Two Towers", "Return"),
  runtime = c(178, 179, 201)
)

# Level order, not appearance order, and identical for both types
f = function() tinyplot(runtime ~ name, data = LOTR, type = type_points())
expect_snapshot_plot(f, label = "type_lines_categorical_points")

f = function() tinyplot(runtime ~ name, data = LOTR, type = type_lines(type = "p"))
expect_snapshot_plot(f, label = "type_lines_categorical_lines")

# An explicit level order is honoured
LOTR2 = transform(
  LOTR,
  name = factor(name, levels = c("Fellowship", "Two Towers", "Return"))
)
f = function() tinyplot(runtime ~ name, data = LOTR2, type = "b")
expect_snapshot_plot(f, label = "type_lines_explicit_levels")

# Categorical labels survive on a flipped axis for non-"p" line types
f = function() tinyplot(runtime ~ name, data = LOTR, type = "b", flip = TRUE)
expect_snapshot_plot(f, label = "type_lines_flip_labels")

# ... and on an unflipped categorical y-axis
f = function() tinyplot(name ~ runtime, data = LOTR, type = "b")
expect_snapshot_plot(f, label = "type_lines_categorical_y")

# A line type layered onto a point type lands on the same categories
f = function() {
  tinyplot(runtime ~ name, data = LOTR, type = "h")
  tinyplot_add(type = "p")
}
expect_snapshot_plot(f, label = "type_lines_layer_h_p")

# xlevels: on-the-fly reordering of a categorical x variable (#679). The
# "data" keyword orders by first appearance, restoring the pre-fix behaviour
# on demand; forwarded automatically from the top-level call.
f = function() tinyplot(runtime ~ name, data = LOTR, type = "b", xlevels = "data")
expect_snapshot_plot(f, label = "type_lines_xlevels_data")

# numeric indexes into the existing levels, via the constructor
f = function() tinyplot(runtime ~ name, data = LOTR, type = type_points(xlevels = 3:1))
expect_snapshot_plot(f, label = "type_points_xlevels_idx")

# unknown levels warn (mirroring type_barplot / type_spineplot)
png(tmp_png <- tempfile(fileext = ".png"))
expect_warning(
  tinyplot(runtime ~ name, data = LOTR, type = "b", xlevels = c("Nope", "Return")),
  pattern = "not all 'xlevels' correspond"
)
dev.off()
unlink(tmp_png)
