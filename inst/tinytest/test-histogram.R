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