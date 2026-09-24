source("helpers.R")
using("tinysnapshot")

# tinyplot.array() method

set.seed(42)
sims = array(
  cumsum(rnorm(20 * 3 * 4)), dim = c(20, 3, 4),
  dimnames = list(NULL, paste("Series", 1:3), paste("Run", 1:4))
)
sims4 = array(
  rnorm(10 * 3 * 2 * 2), dim = c(10, 3, 2, 2),
  dimnames = list(
    time = NULL, series = paste0("s", 1:3),
    model = c("A", "B"), scenario = c("low", "high")
  )
)

# 3D -> facet wrap by the third dimension
f = function() tinyplot(sims, type = "l")
expect_snapshot_plot(f, label = "array_3d")

# 4D -> facet grid, with dimnames names as axis/legend/facet titles
f = function() tinyplot(sims4, type = "b", facet.args = list(prefix = TRUE))
expect_snapshot_plot(f, label = "array_4d")

# tile / heatmap layout per 2D slice
f = function() tinyplot(sims4[1:5, , , ], type = "heatmap", theme = "heatmap")
expect_snapshot_plot(f, label = "array_4d_heatmap")

# 1D arrays are index plots
f = function() tinyplot(array(1:5, 5, list(letters[1:5])), type = "b")
expect_snapshot_plot(f, label = "array_1d")

# unsupported inputs
expect_error(tinyplot(array(1:32, rep(2, 5))), "more than 4 dimensions")
expect_error(tinyplot(sims, facet = "by"), "must be NULL")

# degenerate (1-row or 1-column) array inputs are dropped to vectors (#548)
f = function() tinyplot(1:10, array(1:10, c(1, 10)), ylab = "y")
expect_snapshot_plot(f, label = "array_1row_y")
f = function() tinyplot(1:10, array(1:10, c(10, 1)), ylab = "y")
expect_snapshot_plot(f, label = "array_1row_y")

# facet formulas passed to the default method (i.e. without a model formula)
f = function() {
  tinyplot(mtcars$wt, mtcars$mpg, facet = am ~ vs, data = mtcars)
}
expect_snapshot_plot(f, label = "facet_grid_default_method")
