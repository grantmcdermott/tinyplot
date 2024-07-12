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
      Petal.Length, by = Species, type = "hist",
      fill = 0.4, breaks = 30, palette = "classic"
    )
  )
}
expect_snapshot_plot(f, label = "hist_grouped_fancy")

f = function() {
  with(iris, plt(Petal.Length, facet = Species, type = "hist"))
}
expect_snapshot_plot(f, label = "hist_faceted")

f = function() {
  with(iris, plt(Petal.Length, by = Species, facet = "by", type = "hist", col = "white"))
}
expect_snapshot_plot(f, label = "hist_byfacet")

f = function() {
  with(
    transform(iris, long_sepal = paste("Long sepal:", Sepal.Length > mean(Sepal.Length))),
    plt(
      Petal.Length, by = Species, facet = long_sepal, type = "hist",
      fill = 0.4, breaks = 30, palette = "classic", frame = FALSE, grid = TRUE
    )
  )
}
expect_snapshot_plot(f, label = "hist_grouped_faceted")

f = function() {
  plt(
    ~Petal.Length | Species,
    data = transform(iris, long_sepal = paste("Long sepal:", Sepal.Length > mean(Sepal.Length))),
    facet = ~long_sepal,
    type = "hist", fill = 0.4, 
    breaks = 30, palette = "classic", frame = FALSE, grid = TRUE,
  )
}
expect_snapshot_plot(f, label = "hist_grouped_faceted")


