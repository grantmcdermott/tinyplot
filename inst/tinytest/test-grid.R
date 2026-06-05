source("helpers.R")
using("tinysnapshot")

set.seed(42)
dat = data.frame(
  y = cumsum(rnorm(120)),
  ds = seq(as.Date("2015-01-01"), as.Date("2024-12-01"), by = "month")
)

# Numeric axes

f = function() plt(wt ~ mpg, mtcars, grid = TRUE, main = "numeric: grid = TRUE")
expect_snapshot_plot(f, label = "grid_numeric_true")

f = function() plt(wt ~ mpg, mtcars, grid = "XY", main = "numeric: grid = \"XY\"")
expect_snapshot_plot(f, label = "grid_numeric_upper_xy")

f = function() plt(wt ~ mpg, mtcars, grid = "xy", main = "numeric: grid = \"xy\"")
expect_snapshot_plot(f, label = "grid_numeric_xy")

f = function() plt(wt ~ mpg, mtcars, grid = "x", main = "numeric: grid = \"x\"")
expect_snapshot_plot(f, label = "grid_numeric_x")

f = function() plt(wt ~ mpg, mtcars, grid = "Y", main = "numeric: grid = \"Y\"")
expect_snapshot_plot(f, label = "grid_numeric_Y")

f = function() plt(wt ~ mpg, mtcars, grid = FALSE, main = "numeric: grid = FALSE")
expect_snapshot_plot(f, label = "grid_numeric_false")

# Date axes

f = function() plt(y ~ ds, dat, grid = TRUE, main = "date: grid = TRUE")
expect_snapshot_plot(f, label = "grid_date_true")

f = function() plt(y ~ ds, dat, grid = "xy", main = "date: grid = \"xy\"")
expect_snapshot_plot(f, label = "grid_date_xy")

# Log axes

f = function() plt(1:100, log = "xy", grid = TRUE, main = "log: grid = TRUE")
expect_snapshot_plot(f, label = "grid_log_true")

f = function() plt(1:100, log = "xy", grid = "xy", main = "log: grid = \"xy\"")
expect_snapshot_plot(f, label = "grid_log_xy")

# Single axis on log

f = function() plt(1:100, log = "x", grid = "x", main = "log x: grid = \"x\"")
expect_snapshot_plot(f, label = "grid_logx_x")

# Faceted

f = function() {
  tinytheme("clean2", grid = "xy")
  plt(wt ~ mpg | factor(cyl), mtcars, main = "faceted: grid = \"xy\"")
  tinytheme()
}
expect_snapshot_plot(f, label = "grid_facet_xy")

# Via theme

f = function() {
  tinytheme("clean", grid = "xy")
  plt(wt ~ mpg, mtcars, main = "theme: grid = \"xy\"")
  tinytheme()
}
expect_snapshot_plot(f, label = "grid_theme_xy")

# grid = grid() with facets (issue #193)

f = function() {
  plt(
    Sepal.Length ~ Petal.Length | Sepal.Length, data = iris,
    facet = ~Species, pch = 19,
    grid = grid(),
    main = "grid = grid()"
  )
}
expect_snapshot_plot(f, label = "grid_func_facet")

# Input validation

expect_error(plt(wt ~ mpg, mtcars, grid = "zz"))
expect_error(plt(wt ~ mpg, mtcars, grid = "abc"))
