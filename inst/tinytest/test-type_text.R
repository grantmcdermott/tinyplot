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


f = function() {
  x = c(1, 2, 3)
  y = c(1, 2, 3)
  z = "a"
  tinyplot(x, y, labels = z, type = "text")
}
expect_snapshot_plot(f, label = "text_single_character")


# labeller arg formats text labels, e.g. to match a formatted axis (#617)
f = function() {
  d = data.frame(x = c("A", "B"), y = c(0.5, 0.8))
  tinyplot(y ~ x, data = d, type = "bar", ylim = c(0, 1), yaxl = "%")
  tinyplot_add(type = type_text(labeller = "%", adj = c(0.5, -0.5)))
}
expect_snapshot_plot(f, label = "text_labeller_percent")
