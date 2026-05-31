source("helpers.R")
using("tinysnapshot")

# basic convex hull with grouped data
f = function() {
  plt(Sepal.Length ~ Petal.Length | Species, data = iris, type = "chull")
}
expect_snapshot_plot(f, label = "chull_basic")

# filled convex hull as a layer
f = function() {
  plt(Sepal.Length ~ Petal.Length | Species, data = iris)
  plt_add(type = "chull", fill = 0.2, col = NA)
}
expect_snapshot_plot(f, label = "chull_filled_layer")

# continuous by triggers warning and reverts to discrete
expect_warning(
  plt(Sepal.Length ~ Petal.Length | Sepal.Width, data = iris, type = "chull"),
  "Continuous"
)
