source("helpers.R")
using("tinysnapshot")

f = function() {
  tinyplot(Sepal.Length ~ Petal.Length | Species, data = iris,
           theme = "clean",
           cap = "Source: Anderson (1935)")
}
expect_snapshot_plot(f, label = "cap_clean")

f = function() {
  tinyplot(Sepal.Length ~ Petal.Length | Species, data = iris,
           legend = "bottom!", 
           theme = "clean",
           cap = "Source: Anderson (1935)")
}
expect_snapshot_plot(f, label = "cap_bottom_legend")

f = function() {
  tinyplot(Sepal.Length ~ Petal.Length, data = iris,
           main = "`sub` and `cap` overlap for default theme",
           sub = "Subtitle", cap = "Caption")
}
expect_snapshot_plot(f, label = "cap_default_theme")
