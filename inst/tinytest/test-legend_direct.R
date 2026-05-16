source("helpers.R")
using("tinysnapshot")

aq = airquality
aq$Month = factor(month.name[aq$Month], levels = month.name[5:9])

# Basic direct labels with line plot
f = function() {
  plt(Temp ~ Day | Month, data = aq, type = "l", legend = "direct")
  box("outer", lty = 2)
}
expect_snapshot_plot(f, label = "legend_direct_lines")

# With dynamic theme (ephemeral)
f = function() {
  plt(Temp ~ Day | Month, data = aq, type = "l",
      legend = "direct", theme = "clean2")
  box("outer", lty = 2)
}
expect_snapshot_plot(f, label = "legend_direct_clean2")

# With type = "lm"
f = function() {
  plt(Sepal.Length ~ Petal.Length | Species, data = iris,
      type = "lm", legend = "direct", theme = "clean2")
  box("outer", lty = 2)
}
expect_snapshot_plot(f, label = "legend_direct_lm")

# Long labels expand margin correctly
f = function() {
  iris2 = iris
  levels(iris2$Species) = c("A very long species name", "Medium", "C")
  plt(Sepal.Length ~ Petal.Length | Species, data = iris2,
      type = "lm", legend = "direct", theme = "clean2")
  box("outer", lty = 2)
}
expect_snapshot_plot(f, label = "legend_direct_long_label")

# With facets
f = function() {
  aq2 = aq
  aq2$hot = ifelse(aq2$Temp > 80, "hot", "cool")
  plt(Temp ~ Day | Month, data = aq2, facet = ~hot, type = "l",
      legend = "direct", theme = "clean2")
  box("outer", lty = 2)
}
expect_snapshot_plot(f, label = "legend_direct_facet")

# No by variable: should warn and produce plot without labels
expect_warning(
  plt(0:10, type = "l", legend = "direct"),
  "discrete groups"
)

# Continuous by: should warn
expect_warning(
  plt(Sepal.Length ~ Petal.Length | Sepal.Width, data = iris, legend = "direct"),
  "discrete groups"
)
