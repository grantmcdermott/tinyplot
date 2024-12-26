source("helpers.R")
using("tinysnapshot")

f = function() {
  tinyplot(mpg ~ hp | factor(cyl),
    data = mtcars,
    facet = ~am,
    type = type_text(
      labels = row.names(mtcars),
      font = 2,
      adj = 0))
}
expect_snapshot_plot(f, label = "text_facets")


f = function() {
  x = c(1, 2, 3)
  y = c(1, 2, 3)
  z = c("a", NA, "c")
  tinyplot(x, y, labels = z, type = "text")
}
expect_snapshot_plot(f, label = "text_missing_label")


pkgload::load_all()
f = function() {
  x = c(1, 2, 3)
  y = c(1, 2, 3)
  z = "a"
  tinyplot(x, y, labels = z, type = "text")
}
expect_snapshot_plot(f, label = "text_single_character")
