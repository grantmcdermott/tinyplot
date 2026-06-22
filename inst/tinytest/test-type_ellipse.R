source("helpers.R")
using("tinysnapshot")

# basic ellipse
f = function() tinyplot(
  Sepal.Length ~ Petal.Length | Species, data = iris,
  type = "ellipse"
)
expect_snapshot_plot(f, label = "type_ellipse")

# filled ellipse layer
f = function() {
  tinyplot(Sepal.Length ~ Petal.Length | Species, data = iris)
  tinyplot_add(type = "ellipse", fill = 0.2)
}
expect_snapshot_plot(f, label = "type_ellipse_fill")

# custom level
f = function() tinyplot(
  Sepal.Length ~ Petal.Length | Species, data = iris,
  type = type_ellipse(level = 0.5)
)
expect_snapshot_plot(f, label = "type_ellipse_level50")
