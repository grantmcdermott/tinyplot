source("helpers.R")
using("tinysnapshot")

# basic plot
f = function() {
  plt(mpg ~ cyl, data = mtcars, type = "mean_se")
}
expect_snapshot_plot(f, label = "mean_se_simple")


# change colour
f = function() {
  plt(mpg ~ cyl, data = mtcars, type = "mean_se", col = "red")
}
expect_snapshot_plot(f, label = "mean_se_simple_red")



# custom confint

f = function() {
  plt(mpg ~ cyl, data = mtcars, type = type_mean_se(conf.int = .99))
}
expect_snapshot_plot(f, label = "mean_se_confint99")

# grouped

f = function() {
  plt(mpg ~ cyl | gear, data = mtcars, type = "mean_se", dodge = .1)
}
expect_snapshot_plot(f, label = "mean_se_grouped")

# faceted

f = function() {
  plt(mpg ~ cyl, facet = ~gear, data = mtcars, type = "mean_se")
}
expect_snapshot_plot(f, label = "mean_se_faceted")
