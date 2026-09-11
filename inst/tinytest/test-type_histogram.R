source("helpers.R")
using("tinysnapshot")

f = function() {
  plt(Nile, type = "histogram")
}
expect_snapshot_plot(f, label = "hist_simple")

f = function() {
  plt(Nile, type = "histogram", col = "white")
}
expect_snapshot_plot(f, label = "hist_simple_white")


f = function() {
  with(iris, plt(Petal.Length, by = Species, type = "hist"))
}
expect_snapshot_plot(f, label = "hist_grouped")

f = function() {
  with(
    iris,
    plt(
      Petal.Length, by = Species,
      type = type_histogram(breaks = 30),
      fill = 0.4, palette = "classic"
    )
  )
}
expect_snapshot_plot(f, label = "hist_grouped_fancy")

f = function() {
  with(iris, plt(Petal.Length, facet = Species, type = "hist"))
}
expect_snapshot_plot(f, label = "hist_faceted")

f = function() {
  with(iris, plt(Petal.Length, by = Species, facet = "by", type = "hist"))
}
expect_snapshot_plot(f, label = "hist_byfacet")

f = function() {
  with(
    transform(iris, long_sepal = paste("Long sepal:", Sepal.Length > mean(Sepal.Length))),
    plt(
      Petal.Length, by = Species, facet = long_sepal,
      type = type_histogram(breaks = 30),
      fill = 0.4, palette = "classic", frame = FALSE, grid = TRUE
    )
  )
}
expect_snapshot_plot(f, label = "hist_grouped_faceted")

f = function() {
  plt(
    ~Petal.Length | Species,
    data = transform(iris, long_sepal = paste("Long sepal:", Sepal.Length > mean(Sepal.Length))),
    facet = ~long_sepal,
    type = type_histogram(breaks = 30),
    fill = 0.4,
    palette = "classic", frame = FALSE, grid = TRUE,
  )
}
expect_snapshot_plot(f, label = "hist_grouped_faceted")


# free facet scales (and free histogram breaks)

f = function() {
  tinyplot(
    ~Petal.Width, facet = ~Species,
    facet.args = list(free = TRUE),
    type = type_histogram(),
    data = iris
  )
}
expect_snapshot_plot(f, label = "hist_facet_free")

f = function() {
  tinyplot(
    ~Petal.Width, facet = ~Species,
    facet.args = list(free = TRUE),
    type = type_histogram(free = TRUE),
    data = iris
  )
}
expect_snapshot_plot(f, label = "hist_facet_free_breaks_free")


# univariate formula: y ~ 1 infers histogram
f = function() {
  tinyplot(Sepal.Length ~ 1, data = iris)
}
expect_snapshot_plot(f, label = "hist_formula_y1")


# weighted histogram (#332): base hist() has no weights arg, so bin counts are
# recomputed as weighted sums via the top-level `weights` argument
f = function() {
  tinyplot(~Sepal.Length, data = iris, type = "histogram", weights = Petal.Width)
}
expect_snapshot_plot(f, label = "hist_weights")


# `by == x`: each bin is coloured by its own position along the x-axis, rather
# than every distinct x value being binned (and overplotted) as a group of its
# own. (#725)
f = function() {
  tinyplot(~mpg | mpg, data = mtcars, type = "hist")
}
expect_snapshot_plot(f, label = "hist_by_equals_x")

f = function() {
  tinyplot(
    ~Petal.Width | Petal.Width, data = iris,
    facet = ~Species, facet.args = list(nrow = 1),
    type = "hist"
  )
}
expect_snapshot_plot(f, label = "hist_by_equals_x_faceted")

